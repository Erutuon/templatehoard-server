use crate::html::Page;
use memmap::Mmap;
use serde::{Deserialize, Serialize, Serializer};
use serde_cbor::Deserializer;
use serde_urlencoded;
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    convert::{Infallible, TryInto},
    fmt::{Display, Formatter, Result as FmtResult},
    fs::File,
    io::Result as IoResult,
    num::NonZeroUsize,
    str::FromStr,
};
use unicase::UniCase;
use warp::{
    self, http::StatusCode, reject::Reject, reply::html, Filter, Rejection,
    Reply,
};

#[derive(Debug, Deserialize)]
pub struct TemplateBorrowed<'a> {
    name: &'a str,
    parameters: BTreeMap<&'a str, &'a str>,
    text: Option<&'a str>,
}

// If `text` is `None`, print the parameters in a sensible way.
impl<'a> Display for TemplateBorrowed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(text) = self.text {
            write!(f, "{}", text)
        } else {
            write!(f, "{{{{{}", self.name)?;
            let mut prev = 0;
            let mut sequential = true;
            for (k, v) in &self.parameters {
                if sequential && k.bytes().all(|b| b.is_ascii_digit()) {
                    let cur: u64 = k.parse().unwrap();
                    sequential = cur == prev + 1;
                    if sequential {
                        write!(f, "|{}", v)?;
                    } else {
                        write!(f, "|{}={}", k, v)?;
                    }
                    prev = cur;
                } else {
                    write!(f, "|{}={}", k, v)?;
                }
            }
            write!(f, "}}}}")
        }
    }
}

#[derive(Deserialize)]
struct TemplatesInPage<'a> {
    title: &'a str,
    templates: Vec<TemplateBorrowed<'a>>,
}

markup::define! {
    TitleAndTemplates<'a> (
        title: UniCase<&'a str>,
        templates: Vec<TemplateBorrowed<'a>>
    ) {
        tr {
            td."title" {
                a[href={format!("https://en.wiktionary.org/wiki/{}", title)}] {
                    {title.as_ref()}
                }
            }
            td."templates" {
                ul {
                    @for template in templates.iter() {
                        li {
                            {template.to_string()}
                        }
                    }
                }
            }
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Query {
    search: Option<String>,
    langs: Option<String>,
    limit: Option<NonZeroUsize>,
    offset: Option<usize>,
}

#[derive(Serialize)]
struct Args {
    search: Option<String>,
    langs: Option<Langs>,
    limit: Option<NonZeroUsize>,
    offset: usize,
}

struct Langs(BTreeSet<String>);

impl Langs {
    fn is_valid_char(c: char) -> bool {
        ('a'..='z').contains(&c) || c == '-' || c == ','
    }

    fn contains(&self, s: &str) -> bool {
        self.0.contains(s)
    }
}

struct LangsErr(String);

impl From<LangsErr> for ArgsError {
    fn from(LangsErr(chars): LangsErr) -> Self {
        ArgsError::LangsErr(chars)
    }
}

impl FromStr for Langs {
    type Err = LangsErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut bad_chars =
            s.chars().filter(|c| !Langs::is_valid_char(*c)).peekable();
        if bad_chars.peek().is_some() {
            Err(LangsErr(bad_chars.collect()))
        } else {
            let langs = s.split(',').map(|lang| lang.to_string()).collect();
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
    let args = Args {
        search: Some("kul".into()),
        langs: "en,de".parse().ok(),
        offset: 0,
        limit: Some(500),
    };
    // Check that langs are reordered.
    assert_eq!(
        serde_urlencoded::to_string(args),
        Ok("search=kul&langs=de%2Cen&offset=0&limit=500".into())
    );
}

impl TryInto<Args> for Query {
    type Error = ArgsError;

    fn try_into(self) -> Result<Args, Self::Error> {
        use ArgsError::*;

        let Self {
            mut search,
            langs,
            limit,
            offset,
        } = self;

        // Cannot reject an empty search parameter because the form appends
        // a search parameter even when the input box is empty.
        if search.as_deref() == Some("") {
            search = None;
        }

        if search.is_none() && langs.is_none() {
            Err(MissingParameters)
        } else if is_too_long(&search) || is_too_long(&langs) {
            Err(TooLong)
        } else {
            let langs = langs.map(|s| s.parse::<Langs>()).transpose()?;
            let offset = offset.unwrap_or(0);
            Ok(Args {
                search,
                langs,
                limit,
                offset,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ArgsError {
    MissingParameters,
    LangsErr(String),
    TooLong,
}

impl Display for ArgsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use ArgsError::*;
        match self {
            MissingParameters => write!(
                f,
                concat!(
                    r#"At least one query parameter, "search" or "langs" "#,
                    "is required. Append one of the following to the URL:

?search=<search text>
?langs=<language codes separated by ,>
?search=<search text>&langs=<language codes separated by ,>"
                ),
            ),
            LangsErr(chars) => write!(
                f,
                r#"The "langs" parameter contains the following bad characters: "{}"."#,
                chars
            ),
            TooLong => write!(
                f,
                r#"Either the "langs" or "search" parameter was too long. The limit is {} bytes."#,
                MAX_PARAM_LEN
            ),
        }
    }
}

impl Reject for ArgsError {}

const MAX_PARAM_LEN: usize = 256;
fn is_too_long(param: &Option<String>) -> bool {
    param
        .as_ref()
        .map(|s| s.len() > MAX_PARAM_LEN)
        .unwrap_or(false)
}

fn mmap_file(path: &str) -> IoResult<Mmap> {
    let file = File::open(path)?;
    unsafe { Mmap::map(&file) }
}

// Returns a map from title to templates, filtered by the language codes
// and search string. If the search string is present, returns templates
// containing transcriptions that contain it; if the list of language codes
// is present, the language code (argument 1) must be be found in the list.
fn ipa_search_results<'a, 'b: 'a>(
    deserializer: impl Iterator<Item = serde_cbor::Result<TemplatesInPage<'a>>>,
    search: &Option<String>,
    langs: &Option<Langs>,
    limit: NonZeroUsize,
    offset: usize,
) -> BTreeMap<UniCase<&'a str>, Vec<TemplateBorrowed<'a>>> {
    let ipa_filter = |template: &TemplateBorrowed<'_>| {
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
            && search
                .as_ref()
                .map(|s| {
                    template.parameters.iter().skip(1).any(|(k, v)| {
                        k.bytes().all(|c| c.is_ascii_digit())
                            && v.contains(s.as_str())
                    })
                })
                .unwrap_or(true)
    };
    deserializer
        .filter_map(|val| {
            let TemplatesInPage { title, templates } = val.unwrap();
            let matches: Vec<TemplateBorrowed<'a>> =
                templates.into_iter().filter(&ipa_filter).collect();
            if matches.is_empty() {
                None
            } else {
                Some((UniCase::new(title), matches))
            }
        })
        .skip(offset)
        .take(limit.get())
        .collect()
}

fn do_search(
    Args {
        search,
        langs,
        limit,
        offset,
    }: Args,
    cbor_path: &str,
    max_limit: NonZeroUsize,
) -> String {
    let text =
        mmap_file(cbor_path).expect("could not memory map CBOR stream file");

    let limit = limit
        .map(|l| if l > max_limit { max_limit } else { l })
        .unwrap_or(max_limit);
    let results = ipa_search_results(
        Deserializer::from_slice(&text).into_iter(),
        &search,
        &langs,
        limit,
        offset,
    );

    let mut args = Args {
        search,
        langs,
        limit: Some(limit),
        offset,
    };
    let mut make_link =
        |checked_arith: &dyn Fn(usize, usize) -> Option<usize>, text| {
            checked_arith(offset, limit.get())
                .map(|offset| {
                    args.offset = offset;
                    // Should be infallible because a UTF-8 error is impossible.
                    let query = serde_urlencoded::to_string(&args).unwrap();
                    Cow::Owned(format!(r#"<a href="?{}">{}</a>"#, query, text))
                })
                .unwrap_or(Cow::Borrowed(text))
        };
    let prev = make_link(&usize::checked_sub, "prev");
    let next = make_link(&usize::checked_add, "next");

    Page {
        title: "IPA search result",
        body: markup::raw(format!(
            concat!(
                r#"<div id="navigation">"#,
                r#"<div id="prev">{prev}</div><div id="next">{next}</div></div>"#,
                r#"<table class="search-results">"#,
                "<thead><tr><th>page</th><th>IPA templates</th></tr></thead>",
                "<tbody>{results}</tbody></table>"
            ),
            prev = prev,
            next = next,
            results = results
                .into_iter()
                .map(|(title, templates)| TitleAndTemplates {
                    title: title,
                    templates
                }
                .to_string())
                .collect::<String>()
        )),
    }
    .to_string()
}

enum TextOrHTML<S: Into<String>> {
    HTML(S),
    Text(S),
}

impl<S: Into<String> + Send> Reply for TextOrHTML<S> {
    fn into_response(self) -> warp::reply::Response {
        match self {
            Self::HTML(s) => html(s.into()).into_response(),
            Self::Text(s) => s.into().into_response(),
        }
    }
}

async fn print_err(
    err: Rejection,
    search_page_path: &str,
) -> Result<impl Reply, Infallible> {
    let mut status = None;
    use Cow::*;
    use TextOrHTML::*;
    let err_txt = if let Some(e) = err.find::<ArgsError>() {
        if *e == ArgsError::MissingParameters {
            if let Ok(search_page) = std::fs::read_to_string(&search_page_path)
            {
                status = Some(StatusCode::OK);
                HTML(Owned(search_page))
            } else {
                status = Some(StatusCode::INTERNAL_SERVER_ERROR);
                Text(Borrowed("Internal server error\n"))
            }
        } else {
            Text(Owned(e.to_string()))
        }
    } else if err.find::<warp::reject::InvalidQuery>().is_some() {
        Text(Borrowed(
            r#"Invalid query string; valid parameters are "search" and "langs".\n"#,
        ))
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
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    warp::query::<Query>()
        .and_then(|params: Query| {
            async { params.try_into().map_err(warp::reject::custom) }
        })
        .map(move |args| html(do_search(args, cbor_path, max_limit)))
        .recover(move |e| print_err(e, search_page_path))
}
