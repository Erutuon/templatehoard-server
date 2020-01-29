use crate::html::Page;
use memmap::Mmap;
use regex::{Error as RegexError, Regex};
use serde::{Deserialize, Serialize, Serializer};
use serde_cbor::Deserializer;
use serde_urlencoded;
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    convert::{Infallible, TryFrom},
    fmt::{Display, Formatter, Result as FmtResult},
    fs::File,
    io::Result as IoResult,
    num::NonZeroUsize,
    ops::Deref,
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

/*
enum Matcher {
    PlainText(String),
    Regex(Regex),
}

impl Matcher {
    fn is_match(&self, s: &str) {
        match self {
            Matcher::PlainText(p) => s.contains(p),
            Matcher::Regex(r) => r.is_match(s),
        }
    }
}
*/

#[derive(Serialize, Debug)]
struct Args {
    langs: Option<Langs>,
    search: Option<String>,
    regex: Option<RegexWrapper>,
    limit: Option<NonZeroUsize>,
    offset: usize,
}

#[derive(Debug)]
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

struct LangsErr(String);

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

#[derive(Debug)]
struct RegexWrapper(Regex);

impl Deref for RegexWrapper {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Regex> for RegexWrapper {
    fn from(regex: Regex) -> Self {
        RegexWrapper(regex)
    }
}

impl Serialize for RegexWrapper {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[test]
fn test_serialize_args() {
    let args = Args {
        langs: "en,de".parse().ok(),
        search: Some("kul".into()),
        regex: Some(RegexWrapper(Regex::new("a[aeiou]").unwrap())),
        offset: 0,
        limit: Some(NonZeroUsize::new(500).unwrap()),
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
    type Error = ArgsError;

    fn try_from(query: Query) -> Result<Self, Self::Error> {
        use ArgsError::*;

        let Query {
            mut langs,
            mut search,
            mut regex,
            limit,
            offset,
        } = query;

        if !(search.is_some()
            || langs.is_some()
            || regex.is_some()
            || limit.is_some())
        {
            Err(MissingParameters)
        } else {
            langs = langs.filter(|s| !s.is_empty());
            search = search.filter(|s| !s.is_empty());
            regex = regex.filter(|s| !s.is_empty());

            if is_too_long(&search) || is_too_long(&langs) {
                Err(TooLong)
            } else {
                let langs = langs
                    .map(|s| s.parse::<Langs>())
                    .transpose()?
                    .filter(|l| !l.is_empty());
                let regex = regex
                    .map(|r| Regex::new(&r))
                    .transpose()?
                    .map(|r| r.into());
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
}

#[test]
fn test_try_into_args() {
    use matches::assert_matches;

    // Test default values for empty strings in `Query`.
    let query = Query {
        langs: Some("".parse().unwrap()),
        search: Some("faɪnd".into()),
        regex: Some("".into()),
        offset: 0,
        limit: None,
    };

    let actual = Args::try_from(query);

    assert_matches!(
        actual,
        Ok(Args {
            langs: None,
            search: Some(_),
            regex: None,
            offset: 0,
            limit: None,
        })
    );

    let query = Query {
        langs: Some("".parse().unwrap()),
        search: Some("".into()),
        regex: Some("mæt͡ʃ".into()),
        offset: 0,
        limit: None,
    };

    let actual = Args::try_from(query);

    assert_matches!(
        actual,
        Ok(Args {
            langs: None,
            search: None,
            regex: Some(_),
            offset: 0,
            limit: None,
        })
    );
}

#[derive(Debug, Clone)]
enum ArgsError {
    MissingParameters,
    TooLong,
    LangsErr(String),
    RegexError(RegexError),
}

impl Reject for ArgsError {}

impl From<LangsErr> for ArgsError {
    fn from(LangsErr(chars): LangsErr) -> Self {
        ArgsError::LangsErr(chars)
    }
}

impl From<RegexError> for ArgsError {
    fn from(err: RegexError) -> Self {
        ArgsError::RegexError(err)
    }
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
            TooLong => write!(
                f,
                r#"Either the "langs" or "search" parameter was too long. The limit is {} bytes."#,
                MAX_PARAM_LEN
            ),
            LangsErr(chars) => write!(
                f,
                r#"The "langs" parameter contains the following bad characters: "{}"."#,
                chars
            ),
            RegexError(e) => write!(f, "{}", e),
        }
    }
}

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
    langs: &Option<Langs>,
    search: &Option<String>,
    regex: &Option<RegexWrapper>,
    limit: NonZeroUsize,
    offset: usize,
) -> BTreeMap<UniCase<&'a str>, Vec<TemplateBorrowed<'a>>> {
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
            && any_transcription(&search, &template, |search, tr| {
                tr.contains(search)
            })
            && any_transcription(&regex, &template, |regex, tr| {
                regex.is_match(tr)
            })
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

fn escape_attribute(s: &str) -> String {
    let mut output = Vec::with_capacity(s.len());
    let mut last = 0;
    for (i, b) in s.bytes().enumerate() {
        match b {
            b'&' => {
                output.extend_from_slice(&s.as_bytes()[last..i]);
                output.extend_from_slice(b"&amp;");
                last = i + 1;
            }
            b'"' => {
                output.extend_from_slice(&s.as_bytes()[last..i]);
                output.extend_from_slice(b"&quot;");
                last = i + 1;
            }
            _ => {}
        }
    }
    output.extend_from_slice(&s.as_bytes()[last..]);
    // This is safe because we have only been replacing ASCII bytes.
    unsafe { String::from_utf8_unchecked(output) }
}

#[test]
fn test_escape_attribute() {
    assert_eq!(
        escape_attribute(r#" " & " "#).as_str(),
        " &quot; &amp; &quot; "
    );
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
    max_limit: NonZeroUsize,
) -> String {
    let text =
        mmap_file(cbor_path).expect("could not memory map CBOR stream file");

    let limit = limit
        .map(|l| if l > max_limit { max_limit } else { l })
        .unwrap_or(max_limit);
    let results = ipa_search_results(
        Deserializer::from_slice(&text).into_iter(),
        &langs,
        &search,
        &regex,
        limit,
        offset,
    );

    let mut args = Args {
        langs,
        search,
        regex,
        limit: Some(limit),
        offset,
    };

    let empty_link = |text| format!(r#"<a>{}</a>"#, text);
    let mut navigation_link =
        |checked_arith: &dyn Fn(usize, usize) -> Option<usize>, text| {
            checked_arith(offset, limit.get())
                .map(|offset| {
                    args.offset = offset;
                    // Should be infallible because a UTF-8 error is impossible.
                    let query = serde_urlencoded::to_string(&args).unwrap();
                    format!(
                        r#"<a href="?{}">{}</a>"#,
                        escape_attribute(&query),
                        text
                    )
                })
                .unwrap_or_else(|| empty_link(text))
        };
    let prev = navigation_link(&usize::checked_sub, "prev");
    let next = if results.len() >= limit.get() {
        navigation_link(&usize::checked_add, "next")
    } else {
        empty_link("next")
    };

    let caption = format!(
        "{count} result{plural}{extra}",
        count = results.len(),
        plural = if results.len() == 1 { "" } else { "s" },
        extra = if results.len() == limit.get() {
            " or more"
        } else {
            ""
        }
    );

    Page {
        title: "IPA search result",
        body: markup::raw(format!(
            concat!(
                r#"<div id="navigation">"#,
                r#"<div id="prev">{prev}</div>"#,
                r#"<div id="prev"><a href="ipa">search page</a></div>"#,
                r#"<div id="next">{next}</div></div>"#,
                r#"<table class="search-results"><caption>{caption}</caption>"#,
                "<thead><tr><th>page</th><th>IPA templates</th></tr></thead>",
                "<tbody>{results}</tbody></table>"
            ),
            prev = prev,
            next = next,
            caption = caption,
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
        if let ArgsError::MissingParameters = *e {
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
) -> impl Filter<Extract = (impl Reply,), Error = Infallible> + Clone + 'a {
    warp::query::<Query>()
        .and_then(|params: Query| {
            async { Args::try_from(params).map_err(warp::reject::custom) }
        })
        .map(move |args| html(do_search(args, cbor_path, max_limit)))
        .recover(move |e| print_err(e, search_page_path))
}
