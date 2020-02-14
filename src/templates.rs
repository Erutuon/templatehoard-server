use log;
use once_cell::sync::OnceCell;
use regex::Error as RegexError;
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};
use serde_cbor::{Deserializer, Result as CBORResult};
use serde_json;
use std::{
    collections::HashMap,
    convert::{Infallible, TryFrom},
    fmt::{Display, Formatter, Result as FmtResult},
    num::NonZeroUsize,
    sync::{Arc, Mutex},
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

impl Args {
    fn make_id(&self) -> serde_json::Result<String> {
        let Args {
            template_name,
            matcher,
            offset,
            ..
        } = self;
        serde_json::to_string(&ArgsId {
            template_name,
            matcher,
            offset,
        })
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

#[derive(Debug, Clone)]
enum TemplatesError {
    MissingTemplate,
    UnusedMatcher,
    TooLong,
    BothStringAndRegex,
    RegexError(RegexError),
    CBORError(String),
    NoLimit,
    TemplateDumpNotFound(String),
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
            TemplateDumpNotFound(t) => write!(
                f,
                "The template {{{{{template}}}}} has not been dumped.",
                template = t
            ),
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
    pages: &'iter mut (impl Iterator<Item = CBORResult<TemplatesInPage<'template>>>
                    + 'iter),
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

fn filter_templates_by_parameter(
    args: Args,
    cache: Arc<Mutex<HashMap<String, usize>>>,
) -> Result<String, TemplatesError> {
    let cbor_path = format!("cbor/{}.cbor", &args.template_name);
    let text = if let Ok(t) = mmap_file(&cbor_path) {
        t
    } else {
        // log error?
        return Err(TemplatesError::TemplateDumpNotFound(args.template_name));
    };

    let id = args.make_id().ok();
    if id.is_none() {
        log::error!(
            target: "all",
            "failed to serialize {:?}",
            args
        );
    }

    let start_offset = if let Some(id) = id.as_ref() {
        let cache = cache.lock().unwrap();
        if let Some(&start_offset) = cache.get(id) {
            if start_offset >= text.len() {
                log::error!(
                    target: "all",
                    "start_offset {} for args {} is out of bounds of slice of length {}",
                    start_offset,
                    id,
                    text.len(),
                );
            }
            start_offset
        } else {
            0
        }
    } else {
        0
    };
    let slice = if let Some(slice) = text.get(start_offset..) {
        slice
    } else {
        b""
    };

    let mut pages = Deserializer::from_slice(slice).into_iter();

    // So that `args` can be sent to `print_template_search_results`.
    let matcher = args.matcher.clone();
    let matches = do_search(&mut pages, &matcher);
    let mut next_args = args.clone();
    next_args.offset += args.limit.get();
    let res =
        print_template_search_results(matches, args, "Template search results")
            .map_err(|e| e.into());

    if let Ok(id) = next_args.make_id() {
        let end_offset = start_offset + pages.byte_offset();
        let mut cache = cache.lock().unwrap();
        cache.insert(id, end_offset);
    }

    res
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
            NoLimit => {
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

static REDIRECTS: OnceCell<Option<HashMap<String, String>>> = OnceCell::new();

pub fn handler<'a>(
    search_page_path: &'a str,
    template_redirect_filepath: &'a str,
    max_limit: NonZeroUsize,
    max_query_len: NonZeroUsize,
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    // Expects file in JSON format with template names and the redirects
    // that point to them (without the namespace prefix):
    // { "template name": [ "redirect 1", "redirect 2", ... ], ...}.
    let template_redirects: &Option<HashMap<String, String>> = REDIRECTS
        .get_or_init(|| {
            let text =
                std::fs::read_to_string(template_redirect_filepath).ok()?;
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
    let cache = Arc::new(Mutex::new(HashMap::new()));

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
            if params.any_longer_than(max_query_len.get()) {
                Err(warp::reject::custom(TemplatesError::TooLong))
            } else {
                Args::try_from(params).map_err(warp::reject::custom)
            }
        })
        .and_then(move |args| {
            let cache = Arc::clone(&cache);
            async move {
                log::info!(
                    target: "all",
                    "{:?}",
                    &args,
                );
                filter_templates_by_parameter(args, cache)
                    .map(html)
                    .map_err(warp::reject::custom)
            }
        })
        .recover(move |e| async move {
            print_err(e, search_page_path, max_query_len)
        })
}
