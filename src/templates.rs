use log;
use once_cell::sync::OnceCell;
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};
use serde_cbor::{Deserializer, Result as CborResult};
use serde_json;
use std::{
    collections::HashMap,
    convert::{Infallible, TryFrom},
    num::NonZeroUsize,
    path::{Path, PathBuf},
    sync::Arc,
};
use unicase::UniCase;
use warp::{self, http::StatusCode, reply::html, Filter, Rejection, Reply};

use crate::common::{
    mmap_file, RegexWrapper, SearchResults, TemplateBorrowed, TemplatesInPage,
    TextOrHTML, TitleAndTemplates,
};
use crate::error::Error;
use crate::html::Page;

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
enum Matcher {
    String(String),
    Regex(RegexWrapper),
    IsPresent,
}

impl Matcher {
    fn is_match(&self, haystack: &str) -> bool {
        match self {
            Self::Regex(r) => r.is_match(haystack),
            Self::String(s) => haystack.contains(s),
            Self::IsPresent => true,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct ParameterMatcher {
    parameter: String,
    matcher: Matcher,
}

impl ParameterMatcher {
    fn is_match<'a>(&self, template: &TemplateBorrowed<'a>) -> bool {
        template
            .parameters
            .get(self.parameter.as_str())
            .map(|p| self.matcher.is_match(p))
            .unwrap_or(false)
    }
}

// Strictly speaking `template` and `parameter` are required and should
// `String` rather than `Option<String>`, but I haven't found a way to
// distinguish between an empty query and and an actually invalid query
// except by deserializing.
#[derive(Debug, Deserialize)]
struct Query {
    template: Option<String>,
    parameter: Option<String>,
    search: Option<String>,
    regex: Option<String>,
    #[serde(default)]
    offset: usize,
    limit: Option<NonZeroUsize>,
}

impl Query {
    fn is_some(&self) -> bool {
        self.template.is_some()
            || self.parameter.is_some()
            || self.search.is_some()
            || self.regex.is_some()
            || self.limit.is_some()
    }
    fn find_longer_than(&self, limit: usize) -> Option<&'static str> {
        param_map! [self: [template, parameter, search, regex]]
            .iter()
            .find_map(|(key, value)| {
                value.as_ref().filter(|s| s.len() > limit).map(|_| *key)
            })
    }
}

/// Identifies a query and the number of results that should be displayed.
#[derive(Debug, Clone)]
struct Args {
    template_name: String,
    matcher: Option<ParameterMatcher>,
    offset: usize,
    limit: NonZeroUsize,
}

/// Contains the fields of `Args` that uniquely identify an offset within
/// the CBOR stream file.
struct ArgsId<'a> {
    template_name: &'a str,
    matcher: &'a Option<ParameterMatcher>,
    offset: &'a usize,
}

impl<'a> Serialize for ArgsId<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Args", 4)?;
        state.serialize_field("template", &self.template_name)?;
        if let Some(ParameterMatcher {
            ref parameter,
            ref matcher,
        }) = self.matcher
        {
            state.serialize_field("parameter", parameter)?;
            match matcher {
                Matcher::String(s) => state.serialize_field("search", s)?,
                Matcher::Regex(r) => state.serialize_field("regex", r)?,
                Matcher::IsPresent => (),
            }
        }
        state.serialize_field("offset", &self.offset)?;
        state.end()
    }
}

fn spaces_to_underscores(s: &mut str) {
    unsafe {
        for b in s.as_bytes_mut().iter_mut() {
            if *b == b' ' {
                *b = b'_';
            }
        }
    }
}

impl TryFrom<Query> for Args {
    type Error = Error;

    fn try_from(query: Query) -> Result<Self, Self::Error> {
        use Error::*;

        let Query {
            template,
            parameter,
            search,
            regex,
            offset,
            limit,
        } = query;

        let template = if let Some(mut t) = template.filter(|t| !t.is_empty()) {
            spaces_to_underscores(&mut t);
            t
        } else {
            return Err(MissingTemplate);
        };
        let parameter = parameter.filter(|t| !t.is_empty());
        let search = search.filter(|p| !p.is_empty());
        let regex = regex.filter(|p| !p.is_empty());

        let parameter_matcher = if let Some(parameter) = parameter {
            let matcher = match (search, regex) {
                (Some(_), Some(_)) => return Err(BothStringAndRegex),
                (Some(s), None) => Matcher::String(s),
                (None, Some(r)) => Matcher::Regex(r.parse()?),
                (None, None) => Matcher::IsPresent,
            };
            Some(ParameterMatcher { parameter, matcher })
        } else {
            if search.is_some() || regex.is_some() {
                return Err(UnusedMatcher);
            }
            None
        };

        let limit = if let Some(l) = limit {
            l
        } else {
            return Err(NoLimit);
        };

        Ok(Args {
            template_name: template,
            matcher: parameter_matcher,
            offset,
            limit,
        })
    }
}

/// Serializes as `Query` because it will be deserialized as `Query`.
impl Serialize for Args {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Args", 4)?;
        state.serialize_field("template", &self.template_name)?;
        if let Some(ParameterMatcher {
            ref parameter,
            ref matcher,
        }) = self.matcher
        {
            state.serialize_field("parameter", parameter)?;
            match matcher {
                Matcher::String(s) => state.serialize_field("search", s)?,
                Matcher::Regex(r) => state.serialize_field("regex", r)?,
                Matcher::IsPresent => (),
            }
        }
        state.serialize_field("offset", &self.offset)?;
        state.serialize_field("limit", &self.limit)?;
        state.end()
    }
}

fn do_search<'iter, 'template: 'iter, 'matcher: 'iter>(
    pages: &'iter mut (impl Iterator<Item = CborResult<TemplatesInPage<'template>>>
                    + 'iter),
    matcher: &'matcher Option<ParameterMatcher>,
) -> impl Iterator<Item = CborResult<TitleAndTemplates<'template>>> + 'iter {
    pages.filter_map(move |page| {
        page.map(|p| {
            let templates = p.templates.into_iter();
            let templates = if let Some(matcher) = matcher {
                Box::new(
                    templates
                        .filter(move |template| matcher.is_match(template)),
                )
                    as Box<dyn Iterator<Item = TemplateBorrowed<'template>>>
            } else {
                Box::new(templates)
            };
            let mut templates = templates.peekable();
            if templates.peek().is_some() {
                let templates = templates.collect();
                let title = UniCase::new(p.title);
                Some(TitleAndTemplates { title, templates })
            } else {
                None
            }
        })
        .transpose()
    })
}

fn filter_templates_by_parameter(
    args: Args,
    cbor_dir: impl AsRef<Path>,
) -> Result<String, Error> {
    let cbor_path = {
        let mut p = cbor_dir.as_ref().join(&args.template_name);
        p.set_extension("cbor");
        p
    };
    let text = mmap_file(&cbor_path)
        .map_err(|e| Error::template_dump_not_found(&args.template_name, e))?;

    let mut pages = Deserializer::from_slice(&text).into_iter();

    // So that `args` can be sent to `print_template_search_results`.
    let matcher = args.matcher.clone();
    let mut results = do_search(&mut pages, &matcher)
        .skip(args.offset)
        .take(args.limit.get())
        .collect::<Result<Vec<_>, _>>()?;

    results.sort_unstable_by(|a, b| a.title.cmp(&b.title));

    Ok(serde_json::to_string(&SearchResults {
        complete: results.len() < args.limit.get(),
        templates: results,
    })?)
}

fn print_err(
    err: Rejection,
    search_page_path: &'_ Path,
    max_query_len: NonZeroUsize,
) -> Result<impl Reply, Infallible> {
    use std::borrow::Cow::*;
    use TextOrHTML::*;
    let mut status = None;
    let missing_parameter_error = concat!(
        "Invalid query string. You may have omitted a parameter ",
        "or added an invalid one. The parameters “template” and ",
        "“parameter” are required, and one of “search” and “regex” is ",
        "required; “offset” and “limit” are optional.\n"
    );
    let err_text = if let Some(e) = err.find::<Error>() {
        use Error::*;
        match &e {
            MissingTemplate => {
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
            NoLimit => {
                status = Some(StatusCode::INTERNAL_SERVER_ERROR);
                Text(Borrowed("Internal server error\n"))
            }
            TooLong(_) => Text(Owned(format!(
                "{} The limit is {}.",
                e.to_string(),
                max_query_len.get()
            ))),
            e => Text(Owned(format!("{}\n", e))),
        }
    } else if err.find::<warp::reject::InvalidQuery>().is_some() {
        Text(Borrowed(missing_parameter_error))
    } else {
        Text(Owned(format!("{:?}", err)))
    };
    Ok(warp::reply::with_status(
        err_text,
        status.unwrap_or(StatusCode::NOT_FOUND),
    ))
}

static REDIRECTS: OnceCell<Option<HashMap<String, String>>> = OnceCell::new();

pub fn handler<'a>(
    search_page_path: PathBuf,
    template_redirect_filepath: Option<PathBuf>,
    cbor_dir: PathBuf,
    max_limit: NonZeroUsize,
    max_query_len: NonZeroUsize,
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    let cbor_dir = cbor_dir.into();
    let search_page_path = search_page_path.into();
    // Expects file in JSON format with template names and the redirects
    // that point to them (without the namespace prefix):
    // { "template name": [ "redirect 1", "redirect 2", ... ], ...}.
    let template_redirects: &Option<HashMap<String, String>> = REDIRECTS
        .get_or_init(|| {
            let text =
                std::fs::read_to_string(template_redirect_filepath?).ok()?;
            let target_to_redirects: HashMap<String, Vec<String>> =
                serde_json::from_str(&text).ok()?;
            Some(
                target_to_redirects
                    .into_iter()
                    .flat_map(|(target, redirects)| {
                        redirects
                            .into_iter()
                            .map(move |redirect| (redirect, target.clone()))
                    })
                    .collect(),
            )
        });

    warp::query::<Query>()
        .and_then(move |mut params: Query| async move {
            if params.is_some() {
                params.limit =
                    params.limit.map(|l| l.min(max_limit)).or(Some(max_limit));
            }
            if let Some(redirects) = template_redirects {
                if let Some(template) = params.template {
                    params.template =
                        redirects.get(&template).cloned().or(Some(template));
                }
            }
            if let Some(key) = params.find_longer_than(max_query_len.get()) {
                Err(warp::reject::custom(Error::TooLong(key)))
            } else {
                Args::try_from(params).map_err(warp::reject::custom)
            }
        })
        .and_then(move |args| {
            let cbor_dir = Arc::clone(&cbor_dir);
            async move {
                log::info!(
                    target: "all",
                    "{:?}",
                    &args,
                );

                filter_templates_by_parameter(args, &Arc::clone(&cbor_dir))
                    .map(|json| {
                        html(
                            Page {
                                title: "Template search results",
                                json,
                            }
                            .to_string(),
                        )
                    })
                    .map_err(warp::reject::custom)
            }
        })
        .recover(move |e| {
            let search_page_path = Arc::clone(&search_page_path);
            async move { print_err(e, &search_page_path, max_query_len) }
        })
}
