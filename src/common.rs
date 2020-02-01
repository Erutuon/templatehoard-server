use log;
use markup::Render;
use memmap::Mmap;
use regex::{Error as RegexError, Regex};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use serde_cbor;
use serde_urlencoded;
use std::{
    collections::BTreeMap,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    fs::File,
    io::Result as IoResult,
    num::NonZeroUsize,
    ops::Deref,
    str::FromStr,
};
use unicase::UniCase;
use warp::reply::{html, Reply};

use crate::html::Page;

#[derive(Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct TemplateBorrowed<'a> {
    pub name: &'a str,
    pub parameters: BTreeMap<&'a str, &'a str>,
    pub text: Option<&'a str>,
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
                    // Infallible because `k` consists of ASCII digits.
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

#[derive(Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct TemplatesInPage<'a> {
    pub title: &'a str,
    pub templates: Vec<TemplateBorrowed<'a>>,
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

#[derive(Debug, Clone)]
pub struct RegexWrapper(Regex);

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

impl FromStr for RegexWrapper {
    type Err = RegexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Regex::new(s).map(|r| r.into())
    }
}

impl<'de> Deserialize<'de> for RegexWrapper {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s).map(RegexWrapper).map_err(de::Error::custom)
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

pub fn mmap_file(path: &str) -> IoResult<Mmap> {
    let file = File::open(path)?;
    unsafe { Mmap::map(&file) }
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

pub trait WithLimitAndOffset {
    fn get_offset(&self) -> usize;
    fn set_offset(&mut self, offset: usize);
    fn get_limit(&self) -> NonZeroUsize;
}

pub fn print_template_search_results<'template: 'iter, 'iter, A, R>(
    results: impl Iterator<Item = serde_cbor::Result<TitleAndTemplates<'template>>>
        + 'iter,
    mut args: A,
    title: R,
) -> Result<String, serde_cbor::Error>
where
    A: WithLimitAndOffset + Serialize + Debug,
    R: Render,
{
    let (limit, offset) = (args.get_limit(), args.get_offset());
    let mut results = results
        .skip(offset)
        .take(limit.get())
        .collect::<Result<Vec<_>, _>>()?;
    let result_count = results.len();

    results.sort_unstable_by(|a, b| a.title.cmp(&b.title));

    let results: String = results.into_iter().map(|p| p.to_string()).collect();

    let empty_link = |text| format!(r#"<a>{}</a>"#, text);
    let mut navigation_link = |checked_arith: &dyn Fn(
        usize,
        usize,
    ) -> Option<usize>,
                               text| {
        checked_arith(offset, limit.get())
                .map(|offset| {
                    args.set_offset(offset);
                    // Should be infallible because a UTF-8 error is impossible.
                    match serde_urlencoded::to_string(&args) {
                        Ok(query) => format!(
                            r#"<a href="?{}">{}</a>"#,
                            escape_attribute(&query),
                            text
                        ),
                        Err(e) => {
                            log::error!(
                                target: "all",
                                "failed serde_urlencoded serialization: {:?} => {:?}",
                                &args,
                                &e,
                            );
                            empty_link(text)
                        }
                    }
                })
                .unwrap_or_else(|| empty_link(text))
    };
    let prev = navigation_link(&usize::checked_sub, "prev");
    let next = if result_count >= limit.get() {
        navigation_link(&usize::checked_add, "next")
    } else {
        empty_link("next")
    };

    let caption = format!(
        "{count} result{plural}{extra}",
        count = result_count,
        plural = if result_count == 1 { "" } else { "s" },
        extra = if result_count == limit.get() {
            " or more"
        } else {
            ""
        }
    );

    let printed_results = Page {
        title,
        // Empty query string ? to avoid another argument for the path
        // to the search page.
        body: markup::raw(format!(
            concat!(
                r#"<div id="navigation">"#,
                r#"<div id="prev">{prev}</div>"#,
                r#"<div id="up"><a href="?">search page</a></div>"#,
                r#"<div id="next">{next}</div></div>"#,
                r#"<table class="search-results"><caption>{caption}</caption>"#,
                "<thead><tr><th>page</th><th>templates</th></tr></thead>",
                "<tbody>{results}</tbody></table>"
            ),
            prev = prev,
            next = next,
            caption = caption,
            results = results,
        )),
    }
    .to_string();

    Ok(printed_results)
}

pub enum TextOrHTML<S: Into<String>> {
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
