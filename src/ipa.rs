use regex::Regex;
use serde::{Deserialize, Serialize, Serializer};
use serde_cbor::Deserializer;
use std::{
    borrow::Cow,
    // cmp::Ord,
    collections::BTreeSet,
    convert::{Infallible, TryFrom},
    num::NonZeroUsize,
    str::FromStr,
};
use unicase::UniCase;
use warp::{
    self, http::StatusCode, reply, Filter, Rejection, Reply,
};

use crate::common::{
    mmap_file, RegexWrapper, SearchResults, TemplateBorrowed, TemplatesInPage,
    TextOrHTML, TitleAndTemplates,
};
use crate::error::Error;
use crate::html::Page;

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
struct Query {
    langs: Option<String>,
    search: Option<String>,
    regex: Option<String>,
    limit: Option<NonZeroUsize>,
    #[serde(default)]
    offset: usize,
}

impl Query {
    /// Determines whether the query contains any parameters that require
    /// search results to be displayed.
    fn is_some(&self) -> bool {
        self.langs.is_some()
            || self.search.is_some()
            || self.regex.is_some()
            || self.limit.is_some()
    }

    fn find_longer_than(&self, limit: usize) -> Option<&'static str> {
        param_map![self: [langs, search, regex]]
            .iter()
            .find_map(|(key, value)| {
                value.as_ref().filter(|s| s.len() > limit).map(|_| key).copied()
            })
    }
}

#[derive(Serialize, Debug)]
struct Args {
    langs: Option<Langs>,
    search: Option<String>,
    regex: Option<RegexWrapper>,
    limit: NonZeroUsize,
    offset: usize,
}

#[derive(Debug, Clone)]
struct Langs(BTreeSet<String>);

impl Langs {
    fn is_valid_char(c: char) -> bool {
        ('a'..='z').contains(&c) || c == '-' || c == ','
    }

    fn contains(&self, s: &str) -> bool {
        self.0.contains(s)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

pub struct LangsErr(pub String);

impl FromStr for Langs {
    type Err = LangsErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut bad_chars =
            s.chars().filter(|c| !Langs::is_valid_char(*c)).peekable();
        if bad_chars.peek().is_some() {
            Err(LangsErr(bad_chars.collect()))
        } else {
            let langs = s
                .split(',')
                .filter_map(|lang| {
                    if lang.is_empty() {
                        None
                    } else {
                        Some(lang.to_string())
                    }
                })
                .collect();
            Ok(Langs(langs))
        }
    }
}

impl Serialize for Langs {
    // Serialize as a comma-separated list.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut string = String::with_capacity(
            self.0.iter().map(|s| s.len()).sum::<usize>() + self.0.len() - 1,
        );
        for (i, lang) in self.0.iter().enumerate() {
            if i > 0 {
                string.push(',')
            }
            string.push_str(lang)
        }
        serializer.serialize_str(&string)
    }
}

#[test]
fn test_serialize_args() {
    use serde_urlencoded;
    let args = Args {
        langs: "en,de".parse().ok(),
        search: Some("kul".into()),
        regex: Some(Regex::new("a[aeiou]").unwrap().into()),
        offset: 0,
        limit: NonZeroUsize::new(500).unwrap(),
    };
    // Check that langs are reordered.
    assert_eq!(
        serde_urlencoded::to_string(args),
        Ok(
            "langs=de%2Cen&search=kul&regex=a%5Baeiou%5D&limit=500&offset=0"
                .into()
        )
    );
}

impl TryFrom<Query> for Args {
    type Error = Error;

    fn try_from(query: Query) -> Result<Self, Self::Error> {
        use Error::*;

        if !query.is_some() {
            Err(MissingParameters(&["langs", "search", "regex", "limit"]))
        } else {
            let Query {
                mut langs,
                mut search,
                mut regex,
                limit,
                offset,
            } = query;

            langs = langs.filter(|s| !s.is_empty());
            search = search.filter(|s| !s.is_empty());
            regex = regex.filter(|s| !s.is_empty());

            let limit = if let Some(l) = limit {
                l
            } else {
                return Err(NoLimit);
            };

            let langs = langs
                .map(|s| s.parse::<Langs>())
                .transpose()?
                .filter(|l| !l.is_empty());
            let regex =
                regex.map(|r| Regex::new(&r)).transpose()?.map(|r| r.into());
            Ok(Args {
                langs,
                search,
                regex,
                limit,
                offset,
            })
        }
    }
}

#[test]
fn test_try_from_args() {
    use matches::assert_matches;

    // Test default values for empty strings in `Query`.
    let query = Query {
        langs: Some("".parse().unwrap()),
        search: Some("faɪnd".into()),
        regex: Some("".into()),
        offset: 0,
        limit: Some(NonZeroUsize::new(500).unwrap()),
    };

    let actual = Args::try_from(query);

    assert_matches!(
        actual,
        Ok(Args {
            langs: None,
            search: Some(_),
            regex: None,
            offset: 0,
            limit: _,
        })
    );

    let query = Query {
        langs: Some("".parse().unwrap()),
        search: Some("".into()),
        regex: Some("mæt͡ʃ".into()),
        offset: 0,
        limit: Some(NonZeroUsize::new(500).unwrap()),
    };

    let actual = Args::try_from(query);

    assert_matches!(
        actual,
        Ok(Args {
            langs: None,
            search: None,
            regex: Some(_),
            offset: 0,
            limit: _,
        })
    );
}

// Returns an iterator filtered by the language codes, search string,
// and regex (if any). If the search string or regex are present, returns templates
// containing transcriptions that match each of them; if the list of language codes
// is present, the language code (argument 1) must be be found in the list. If all
// are `None`, all results are returned.
fn ipa_search_results<'iter, 'template: 'iter, 'matcher: 'iter>(
    deserializer: &'iter mut (impl Iterator<
        Item = serde_cbor::Result<TemplatesInPage<'template>>,
    > + 'iter),
    langs: &'matcher Option<Langs>,
    search: &'matcher Option<String>,
    regex: &'matcher Option<RegexWrapper>,
) -> impl Iterator<Item = serde_cbor::Result<TitleAndTemplates<'template>>> + 'iter
{
    fn any_transcription<T, F>(
        val: &Option<T>,
        template: &TemplateBorrowed<'_>,
        filter: F,
    ) -> bool
    where
        F: Fn(&T, &str) -> bool,
    {
        val.as_ref()
            .map(|s| {
                template.parameters.iter().skip(1).any(|(k, v)| {
                    k.bytes().all(|c| c.is_ascii_digit()) && filter(&s, &v)
                })
            })
            .unwrap_or(true)
    }

    let ipa_filter = move |template: &TemplateBorrowed<'_>| {
        langs
            .as_ref()
            .map(|l| {
                template
                    .parameters
                    .get("1")
                    .map(|p| l.contains(*p))
                    .unwrap_or(false)
            })
            .unwrap_or(true)
            && any_transcription(&search, &template, |search, tr| {
                tr.contains(search)
            })
            && any_transcription(&regex, &template, |regex, tr| {
                regex.is_match(tr)
            })
    };

    deserializer.filter_map(move |page| {
        page.map(|p| {
            let TemplatesInPage { title, templates } = p;
            let mut templates =
                templates.into_iter().filter(&ipa_filter).peekable();
            if templates.peek().is_some() {
                let title = UniCase::new(title);
                let templates = templates.collect();
                Some(TitleAndTemplates { title, templates })
            } else {
                None
            }
        })
        .transpose()
    })
}

fn do_search(
    Args {
        langs,
        search,
        regex,
        limit,
        offset,
    }: Args,
    cbor_path: &str,
) -> Result<String, Error> {
    let text =
        mmap_file(cbor_path).map_err(|e| {
            Error::template_dump_not_found("IPA", e)
        })?;

    let mut pages = Deserializer::from_slice(&text).into_iter();

    let mut results = ipa_search_results(&mut pages, &langs, &search, &regex)
        .skip(offset)
        .take(limit.get())
        .collect::<Result<Vec<_>, _>>()?;
    results.sort_unstable_by(|a, b| a.title.cmp(&b.title));

    Ok(serde_json::to_string(&SearchResults {
        complete: results.len() < limit.get(),
        templates: results,
    })?)
}

async fn print_err(
    err: Rejection,
    search_page_path: &str,
    max_query_len: NonZeroUsize,
) -> Result<impl Reply, Infallible> {
    let mut status = None;
    use Cow::*;
    use TextOrHTML::*;
    let err_txt = if let Some(e) = err.find::<Error>() {
        match &e {
            Error::MissingParameters(_) => {
                if let Ok(search_page) =
                    std::fs::read_to_string(&search_page_path)
                {
                    status = Some(StatusCode::OK);
                    HTML(Owned(search_page))
                } else {
                    status = Some(StatusCode::INTERNAL_SERVER_ERROR);
                    Text(Borrowed("Internal server error\n"))
                }
            }
            // Limit should have been set before `Query` was converted into `Args`.
            Error::NoLimit => {
                status = Some(StatusCode::INTERNAL_SERVER_ERROR);
                Text(Borrowed("Internal server error\n"))
            }
            Error::TooLong(_) => Text(Owned(format!(
                "{} The limit is {}.",
                e.to_string(),
                max_query_len.get()
            ))),
            _ => Text(Owned(e.to_string())),
        }
    } else if err.find::<warp::reject::InvalidQuery>().is_some() {
        Text(Borrowed(concat!(
            "Invalid query string. You may have omitted a parameter ",
            "or added an invalid one. One of “langs”, “search”, or “regex” ",
            "is required; “offset” and “limit” are optional.\n"
        )))
    } else {
        Text(Borrowed("Unknown error\n"))
    };
    Ok(warp::reply::with_status(
        err_txt,
        status.unwrap_or(StatusCode::NOT_FOUND),
    ))
}

pub fn handler<'a>(
    cbor_path: &'a str,
    search_page_path: &'a str,
    max_limit: NonZeroUsize,
    max_query_len: NonZeroUsize,
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    warp::query::<Query>()
        .and_then(move |mut params: Query| async move {
            if params.is_some() {
                params.limit =
                    params.limit.map(|l| l.min(max_limit)).or(Some(max_limit));
            }
            if let Some(key) = params.find_longer_than(max_query_len.get()) {
                Err(warp::reject::custom(Error::TooLong(key)))
            } else {
                Args::try_from(params).map_err(warp::reject::custom)
            }
        })
        .and_then(move |args| async move {
            log::info!(
                target: "all",
                "{:?}",
                &args,
            );
            do_search(args, cbor_path)
                .map(|json| {
                    reply::html(
                        Page {
                            title: "IPA search results",
                            json,
                        }
                        .to_string(),
                    )
                })
                .map_err(|e| {
                    warp::reject::custom(e)
                })
        })
        .recover(move |e| print_err(e, search_page_path, max_query_len))
}
