use log;
use regex::Error as RegexError;
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};
use serde_cbor::{Deserializer, Result as CBORResult};
use std::{
    convert::{Infallible, TryFrom},
    fmt::{Display, Formatter, Result as FmtResult},
    num::NonZeroUsize,
};
use unicase::UniCase;
use warp::{
    self, http::StatusCode, reject::Reject, reply::html, Filter, Rejection,
    Reply,
};

use crate::common::{
    mmap_file, print_template_search_results, RegexWrapper, TemplateBorrowed,
    TemplatesInPage, TextOrHTML, TitleAndTemplates, WithLimitAndOffset,
};

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
    fn any_longer_than(&self, limit: usize) -> bool {
        [
            self.template.as_ref(),
            self.parameter.as_ref(),
            self.search.as_ref(),
            self.regex.as_ref(),
        ]
        .iter()
        .any(|param| param.as_ref().map(|s| s.len() > limit).unwrap_or(false))
    }
}

#[derive(Debug, Clone)]
struct Args {
    template_name: String,
    matcher: Option<ParameterMatcher>,
    offset: usize,
    limit: NonZeroUsize,
}

impl TryFrom<Query> for Args {
    type Error = TemplatesError;

    fn try_from(query: Query) -> Result<Self, Self::Error> {
        use TemplatesError::*;

        let Query {
            template,
            parameter,
            search,
            regex,
            offset,
            limit,
        } = query;

        let template = if let Some(t) = template.filter(|t| !t.is_empty()) {
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

#[derive(Debug, Clone)]
enum TemplatesError {
    MissingTemplate,
    UnusedMatcher,
    TooLong,
    BothStringAndRegex,
    RegexError(RegexError),
    CBORError(String),
    NoLimit,
    FileNotFound(String),
}

impl Reject for TemplatesError {}

impl From<RegexError> for TemplatesError {
    fn from(err: RegexError) -> Self {
        Self::RegexError(err)
    }
}

impl From<serde_cbor::Error> for TemplatesError {
    fn from(err: serde_cbor::Error) -> Self {
        TemplatesError::CBORError(err.to_string())
    }
}
impl Display for TemplatesError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use TemplatesError::*;
        match self {
            MissingTemplate => write!(
                f,
                "“template” parameter is missing.",
            ),
            UnusedMatcher => write!(
                f,
                "“search”, or “regex” will not be used because “parameter” is not present."
            ),
            TooLong => write!(
                f,
                "One of “template”, “parameter”, “search”, or “regex” was too long.",
            ),
            BothStringAndRegex => write!(f, "Choose either “search” or “regex”."),
            RegexError(e) => write!(f, "{}", e),
            CBORError(e) => write!(f, "{}", e),
            NoLimit => write!(f, "A limit is required."),
            FileNotFound(file) => write!(f, "Could not open {}", file),
        }
    }
}

impl WithLimitAndOffset for Args {
    fn get_offset(&self) -> usize {
        self.offset
    }
    fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }
    fn get_limit(&self) -> NonZeroUsize {
        self.limit
    }
}

fn do_search<'iter, 'template: 'iter, 'matcher: 'iter>(
    pages: impl Iterator<Item = CBORResult<TemplatesInPage<'template>>> + 'iter,
    matcher: &'matcher Option<ParameterMatcher>,
) -> impl Iterator<Item = CBORResult<TitleAndTemplates<'template>>> + 'iter {
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

fn filter_templates_by_parameter(args: Args) -> Result<String, TemplatesError> {
    let cbor_path = format!("cbor/{}.cbor", &args.template_name);
    let text = if let Ok(t) = mmap_file(&cbor_path) {
        t
    } else {
        // log error?
        return Err(TemplatesError::FileNotFound(cbor_path));
    };

    let pages = Deserializer::from_slice(&text).into_iter();

    // To allow us to send `args` to `print_template_search_results`.
    let matcher = args.matcher.clone();
    let matches = do_search(pages, &matcher);
    print_template_search_results(matches, args, "Template search results")
        .map_err(|e| e.into())
}

fn print_err(
    err: Rejection,
    search_page_path: &str,
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
    let err_text = if let Some(e) = err.find::<TemplatesError>() {
        use TemplatesError::*;
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
            NoLimit | FileNotFound(_) => {
                status = Some(StatusCode::INTERNAL_SERVER_ERROR);
                Text(Borrowed("Internal server error\n"))
            }
            TooLong => Text(Owned(format!(
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

pub fn handler<'a>(
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
            if params.any_longer_than(max_query_len.get()) {
                Err(warp::reject::custom(TemplatesError::TooLong))
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
            filter_templates_by_parameter(args)
                .map(html)
                .map_err(warp::reject::custom)
        })
        .recover(move |e| async move {
            print_err(e, search_page_path, max_query_len)
        })
}
