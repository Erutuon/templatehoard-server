use itertools::Itertools;
use regex::Error as RegexError;
use serde_cbor::Error as CborError;
use serde_json::Error as JsonError;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::{io::Error as IoError, sync::Arc};
use warp::reject::Reject;

#[derive(Debug, Clone)]
pub enum Error {
    MissingParameters(&'static [&'static str]),
    TooLong(&'static str),
    LangsErr(String),
    Regex(RegexError),
    Cbor(String),
    NoLimit,
    MissingTemplate,
    UnusedMatcher,
    BothStringAndRegex,
    Json(String),
    TemplateDumpNotFound {
        template: String,
        error: Arc<IoError>,
    },
}

impl Error {
    pub fn template_dump_not_found(
        template: impl Into<String>,
        error: impl Into<Arc<IoError>>,
    ) -> Error {
        let template = template.into();
        let error = error.into();
        Self::TemplateDumpNotFound { template, error }
    }
}

impl Reject for Error {}

impl From<RegexError> for Error {
    fn from(err: RegexError) -> Self {
        Error::Regex(err)
    }
}

impl From<CborError> for Error {
    fn from(err: CborError) -> Self {
        Error::Cbor(err.to_string())
    }
}

impl From<JsonError> for Error {
    fn from(err: JsonError) -> Self {
        Error::Json(err.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Error::*;
        match self {
            MissingParameters(parameters) => write!(
                f,
                "Parameters missing: expected one of “{}”",
                parameters.iter().format(", ")
            ),
            TooLong(key) => write!(
                f,
                r#"The “{}” parameter was too long."#,
                key
            ),
            LangsErr(chars) => write!(
                f,
                r#"The "langs" parameter contains the following bad characters: "{}"."#,
                chars
            ),
            Regex(e) => write!(f, "{}", e),
            Cbor(e) => write!(f, "{}", e),
            NoLimit => write!(f, "A limit is required."),
            MissingTemplate => write!(
                f,
                "“template” parameter is missing.",
            ),
            UnusedMatcher => write!(
                f,
                "“search”, or “regex” will not be used because “parameter” is not present."
            ),
            BothStringAndRegex => write!(f, "Choose either “search” or “regex”."),
            Json(e) => write!(f, "{}", e),
            TemplateDumpNotFound { template, .. } => write!(
                f,
                "The template {{{{{template}}}}} has not been dumped.",
                template = template,
            ),
        }
    }
}
