use crate::html::Page;
use once_cell::sync::Lazy;
use regex::Regex;
use rlua::{Lua, Result as LuaResult};
use serde::Deserialize;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader};
use unicode_segmentation::UnicodeSegmentation;
use warp::{self, reply::html, Filter, Rejection, Reply};

const ENTRY_LIMIT: usize = 500;

type LanguageCode = String;
type LanguageName = String;
static WIKTIONARY_LANGUAGE_CODE_TO_NAME: Lazy<
    HashMap<LanguageCode, LanguageName>,
> = Lazy::new(|| {
    let lua = Lua::new();
    lua.context(|ctx| {
        let langs: LuaResult<HashMap<LanguageCode, LanguageName>> =
            ctx.load(r#"return require "code_to_name""#).call(());
        langs.unwrap_or_else(|e| {
            panic!("Error while loading table of languages: {}", e);
        })
    })
});

fn get_language_name(language_code: &str) -> &str {
    WIKTIONARY_LANGUAGE_CODE_TO_NAME
        .get(language_code)
        .map(String::as_str)
        .unwrap_or("?")
}

markup::define! {
    EntryLinks(
        lines: Vec<String>,
        lang: String,
        find: Option<String>,
        find_regex: Option<String>,
        max_length: usize
    ) {
        #main {
            p { { Description { lang, find, find_regex, count: lines.len() } } }
            ul[style={format!("column-width: {}ch;", std::cmp::max(max_length, &5))}] {
                @for line in lines.iter() {
                    li {
                        {WiktionaryLink { page: line, lang: &lang }}
                    }
                }
            }
        }
    }
    WiktionaryLink<'a>(page: &'a str, lang: &'a str) {
        a[href = {format!("https://en.wiktionary.org/wiki/{}#{}", page, get_language_name(lang))}] {
            {page}
        }
    }
    Description<'a>(
        lang: &'a str,
        find: &'a Option<String>,
        find_regex: &'a Option<String>,
        count: usize
    ) {{{
        let count = *count;
        let mut desc = if count == 0 {
            "No".to_string()
        } else if count == ENTRY_LIMIT {
            format!(
                "{} {}",
                if find.is_none() && find_regex.is_none() {
                    "There are at least "
                } else {
                    "At least "
                },
                ENTRY_LIMIT
            )
        } else {
            count.to_string()
        };
        let language_name = get_language_name(lang);
        write!(desc, " {} entry name", language_name).unwrap();
        if count != 1 { desc.push_str("s"); }
        if let Some(f) = find {
            write!(desc, " contained {}", CodeTag { content: f, class: "bare-str" }).unwrap();
            if find_regex.is_some() { desc.push_str(" and"); }
        }
        if let Some(f) = find_regex {
            write!(desc, " matched the regex {}", CodeTag { content: f, class: "regex" }).unwrap();
        }
        desc.push_str(if count == 0 {"."} else {":"});
        markup::raw(desc)
    }}}
    CodeTag<'a>(content: &'a str, class: &'a str) { code.{class} { {content} } }
}

markup::define! {
    RegexError(error: String) {
        {markup::doctype()}
        html {
            head {
                title { "Hello!"}
                style {
                    ".error { white-space: pre; font-family: monospace; }"
                }
            }
            body {
                #main.container {
                    div.error {
                        "Invalid regex: " {error}
                    }
                }
            }
        }
    }
}

static LANG_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^(?-u)[a-z]{2,3}(?:-[a-z]{2,3}){0,2}$").unwrap());

#[derive(Deserialize)]
struct EntryParameters {
    find_regex: Option<String>,
    find: Option<String>,
}

fn max_graphemes<I, S: AsRef<str>>(iter: I) -> Option<usize>
where
    I: IntoIterator<Item = S>,
{
    iter.into_iter()
        .map(|line| UnicodeSegmentation::graphemes(line.as_ref(), true).count())
        .max()
}

fn entry_list(
    language_code: String,
    args: EntryParameters,
) -> impl warp::reply::Reply {
    if !LANG_REGEX.is_match(&language_code) {
        return html(format!("{} is not a language code.", &language_code));
    }
    let filename = format!("entries/{}.txt", language_code);
    if let Ok(file) = File::open(&filename) {
        let find_regex: Option<Regex> = match args
            .find_regex
            .filter(|r| r.as_str() != "")
            .map(|r| Regex::new(&r))
        {
            Some(Ok(regex)) => Some(regex),
            Some(Err(error)) => {
                return html(format!(
                    "{}",
                    RegexError {
                        error: error.to_string()
                    }
                ))
            }
            None => None,
        };
        let find = args.find.filter(|f| f.as_str() != "");
        let file = BufReader::new(file);
        let lines: Result<Vec<_>, _> = file
            .lines()
            .filter(|line| {
                if let Ok(line) = line {
                    find.as_ref()
                        .map(|f| line.contains(f.as_str()))
                        .unwrap_or_else(|| {
                            find_regex
                                .as_ref()
                                .map(|f| f.is_match(line))
                                .unwrap_or(true)
                        })
                } else {
                    true
                }
            })
            .take(ENTRY_LIMIT)
            .collect();
        let max_length = lines
            .as_ref()
            .ok()
            .map(|lines| max_graphemes(lines).unwrap_or(15usize))
            .unwrap();
        match lines {
            Ok(lines) => html(format!(
                "{}",
                Page {
                    title: "Entry name search results",
                    body: EntryLinks {
                        lines,
                        lang: language_code,
                        find,
                        find_regex: find_regex.map(|r| r.as_str().to_string()),
                        max_length,
                    }
                }
            )),
            Err(e) => html(format!("Error reading {}: {}.", &filename, e)),
        }
    } else {
        html(format!("No entries for language code {}.", &language_code))
    }
}

pub fn route() -> impl Filter<Extract = (impl Reply,), Error = Rejection> + Clone
{
    warp::filters::path::param()
        .and(warp::query::<EntryParameters>())
        .map(entry_list)
}
