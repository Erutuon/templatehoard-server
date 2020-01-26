use crate::html::Page;
use memmap::Mmap;
use serde::Deserialize;
use serde_cbor::Deserializer;
use std::{
    collections::{BTreeMap, HashSet},
    convert::Infallible,
    fmt::{Display, Formatter, Result as FmtResult},
    fs::File,
    io::Result as IoResult,
};
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

impl<'a> Display for TemplateBorrowed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
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

#[derive(Deserialize)]
struct TemplatesInPage<'a> {
    title: &'a str,
    templates: Vec<TemplateBorrowed<'a>>,
}

/*
#[derive(Debug, Deserialize)]
pub struct Template {
    pub name: String,
    pub parameters: BTreeMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct TemplatesInPage {
    title: String,
    templates: Vec<Template>,
    text: Option<String>,
}
*/

// Returns a map from title to templates, filtered by the language codes
// and search string. If the search string is present, returns templates
// containing transcriptions that contain it; if the list of language codes
// is present, the language code (argument 1) must be be found in the list.
fn ipa_search_results<'a, 'b: 'a>(
    deserializer: impl Iterator<Item = serde_cbor::Result<TemplatesInPage<'a>>>,
    search: Option<String>,
    langs: Option<HashSet<&'b str>>,
) -> BTreeMap<&'a str, Vec<TemplateBorrowed<'a>>> {
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
                Some((title, matches))
            }
        })
        .collect()
}

markup::define! {
    TitleAndTemplates<'a, 'b> (
        title: &'b str,
        templates: Vec<TemplateBorrowed<'a>>
    ) {
        tr {
            td {
                a [href={format!("https://en.wiktionary.org/wiki/{}", title)}] {
                    {title}
                }
            }
            td {
                @for (i, template) in templates.iter().enumerate() {
                    @if i > 0 {
                        br;
                    }
                    code {
                        @if let Some(text) = template.text {
                            {text.to_string()}

                        } else {
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
struct IPASearchParameters {
    search: Option<String>,
    langs: Option<String>,
}

impl IPASearchParameters {
    fn validate(&self) -> Result<(), InvalidIPAParameters> {
        use InvalidIPAParameters::*;
        if self.search.is_none() && self.langs.is_none() {
            Err(MissingParameters)
        } else if self.langs.is_some() && self.langs.as_ref().unwrap().len() < 2
        {
            Err(LangsTooShort)
        } else if self.search.as_deref() == Some("") {
            Err(SearchEmpty)
        } else if is_too_long(&self.search) || is_too_long(&self.langs) {
            Err(TooLong)
        } else {
            let mut bad_chars = self.langs.as_ref().map(|l| {
                l.chars()
                    .filter(|c| {
                        !(('a'..='z').contains(c) || *c == '-' || *c == ',')
                    })
                    .peekable()
            });
            if bad_chars.is_some()
                && bad_chars.as_mut().unwrap().peek().is_some()
            {
                Err(BadLangCharacters(bad_chars.unwrap().collect()))
            } else {
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum InvalidIPAParameters {
    MissingParameters,
    BadLangCharacters(String),
    TooLong,
    LangsTooShort,
    SearchEmpty,
}

impl Display for InvalidIPAParameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use InvalidIPAParameters::*;
        match self {
            MissingParameters => write!(
                f,
                r#"At least one query parameter, "search" or "langs", is required. Append one of the following to the URL:

?search=<search text>
?langs=<language codes separated by ,>
?search=<search text>&langs=<language codes separated by ,>"#,
            ),
            BadLangCharacters(chars) => write!(
                f,
                r#"The "langs" parameter contains the following bad characters: {}"#,
                chars
            ),
            TooLong => write!(
                f,
                r#"Either "lang" or "search" was too long. The limit is {} bytes."#,
                MAX_PARAM_LEN
            ),
            LangsTooShort => {
                write!(f, "Langs must be two characters or longer.")
            }
            SearchEmpty => {
                write!(f, r#""search" parameter must not be empty."#)
            }
        }
    }
}

impl Reject for InvalidIPAParameters {}

const MAX_PARAM_LEN: usize = 256;
fn is_too_long(param: &Option<String>) -> bool {
    param
        .as_ref()
        .map(|s| s.len() > MAX_PARAM_LEN)
        .unwrap_or(false)
}

fn ipa_params(
) -> impl Filter<Extract = (IPASearchParameters,), Error = Rejection> + Clone {
    warp::query::<IPASearchParameters>().and_then(
        |params: IPASearchParameters| {
            async move {
                if let Err(e) = params.validate() {
                    return Err(warp::reject::custom(e));
                } else {
                    Ok(params)
                }
            }
        },
    )
}

fn mmap_file(path: &str) -> IoResult<Mmap> {
    let file = File::open(path)?;
    unsafe { Mmap::map(&file) }
}

fn do_search(
    IPASearchParameters {
        search,
        langs: langs_string,
    }: IPASearchParameters,
    cbor_path: &str,
) -> String {
    let langs = langs_string
        .as_ref()
        .map(|l| l.split(',').collect::<HashSet<_>>());

    let text =
        mmap_file(cbor_path).expect("could not memory map CBOR stream file");

    let results = ipa_search_results(
        Deserializer::from_slice(&text).into_iter(),
        search,
        langs,
    );

    Page {
        title: "IPA search result",
        body: markup::raw(format!(
            r#"<table><tbody>{}</tbody></table>"#,
            results
                .into_iter()
                .map(|(title, templates)| TitleAndTemplates {
                    title,
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

async fn handle_rejection(
    err: Rejection,
    search_page_path: &str,
) -> Result<impl Reply, Infallible> {
    let mut status = None;
    use std::borrow::Cow::*;
    use TextOrHTML::*;
    let err_txt = if let Some(e) = err.find::<InvalidIPAParameters>() {
        if *e == InvalidIPAParameters::MissingParameters {
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
    search_path: &'a str,
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    ipa_params()
        .map(move |params| html(do_search(params, cbor_path)))
        .recover(move |e| handle_rejection(e, search_path))
}
