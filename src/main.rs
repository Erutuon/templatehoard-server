use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader};
use warp::{self, Filter, reply::html};
use regex::Regex;
use once_cell::sync::Lazy;
use rlua::{Lua, Result as LuaResult};
use unicode_segmentation::UnicodeSegmentation;

const ENTRY_LIMIT: usize = 500;

type LanguageCode = String;
type LanguageName = String;
static WIKTIONARY_LANGUAGE_CODE_TO_NAME: Lazy<HashMap<LanguageCode, LanguageName>> = Lazy::new(|| {
    let lua = Lua::new();
    lua.context(|ctx| {
        let langs: LuaResult<HashMap<LanguageCode, LanguageName>> = ctx.load(r#"
            return require "code_to_name"
        "#).call(());
        langs.unwrap_or_else(|e| {
            panic!("Error while loading table of languages: {}", e);
        })
    })
});

fn get_language_name(language_code: &str) -> &str {
    WIKTIONARY_LANGUAGE_CODE_TO_NAME.get(language_code)
        .map(String::as_str)
        .unwrap_or("?")
}

markup::define! {
    EntryLinks<'a>(
        lines: Vec<String>,
        lang: String,
        find: Option<&'a str>,
        find_regex: Option<String>,
        max_length: usize
    ) {
        {markup::doctype()}
        html {
            head {
                meta[charset="utf-8"];
                title { "Entry name search results" }
                link[rel="stylesheet", type="text/css", href="style.css"];
            }
            body {
                #main {
                    p { { Header { lang, find, find_regex, count: lines.len() } } }
                    ul[style={format!("column-width: {}ch;", std::cmp::max(max_length, &5))}] {
                        @for line in lines.iter() {
                            li {
                                {WiktionaryLink { page: line, lang: &lang }}
                            }
                        }
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
    Header<'a>(
        lang: &'a str,
        find: &'a Option<&'a str>,
        find_regex: &'a Option<String>,
        count: usize
    ) {{{
        let count = *count;
        let mut header = if count == 0 {
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
        write!(header, " {} entry name", language_name).unwrap();
        if count != 1 { header.push_str("s"); }
        if let Some(f) = find {
            write!(header, " contained {}", CodeTag { content: f, class: "bare-str" }).unwrap();
            if find_regex.is_some() { header.push_str(" and"); }
        }
        if let Some(f) = find_regex {
            write!(header, " matched the regex {}", CodeTag { content: f, class: "regex" }).unwrap();
        }
        header.push_str(if count == 0 {"."} else {":"});
        markup::raw(header)
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

static LANG_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?-u)[a-z]{2,3}(?:-[a-z]{2,3}){0,2}$").unwrap()
});

fn list_entries_by_language(filename: String, args: HashMap<String, String>) -> impl warp::reply::Reply {
    let language_code = match LANG_REGEX.find(&filename) {
        Some(c) => c.as_str(),
        None => return html(format!("{} is not a language code.", &filename)),
    };
    let filename = format!("entries/{}.txt", language_code);
    if let Ok(file) = File::open(&filename) {
        let find_regex: Option<Regex> = match args.get("find_regex") {
            Some(r) if r != "" => match Regex::new(&r) {
                Ok(r) => Some(r),
                Err(error) => return html(format!("{}", RegexError { error: error.to_string() })),
            },
            _ => None,
        };
        let find = args.get("find").filter(|f| f.as_str() != "");
        let file = BufReader::new(file);
        let lines: Result<Vec<_>, _> = file.lines()
            .filter(|line| {
                if let Ok(line) = line {
                    find.map(|f| line.contains(f))
                        .unwrap_or_else(|| {
                            find_regex.as_ref()
                                .map(|f| f.is_match(line))
                                .unwrap_or(true)
                        })
                } else {
                    true
                }
            })
            .take(ENTRY_LIMIT)
            .collect();
        let max_length = lines.as_ref()
            .ok()
            .iter()
            .flat_map(|lines| {
                lines.iter()
                    .map(|line| {
                        UnicodeSegmentation::graphemes(line.as_str(), true).count()
                    })
            })
            .max()
            .unwrap_or(15usize);
        match lines {
            Ok(lines) => {
                html(format!(
                    "{}",
                    EntryLinks {
                        lines, lang: language_code.to_string(),
                        find: find.map(|s| s.as_str()),
                        find_regex: find_regex.map(|r| r.as_str().to_string()),
                        max_length,
                    }
                ))
            },
            Err(e) => {
                html(format!("Error reading {}: {}.", &filename, e))
            },
        }
    } else {
        html(format!("No entries for language code {}.", language_code))
    }
}

#[tokio::main]
async fn main() {
    let route = warp::filters::method::get()
        .and(warp::path!("entries" / String))
        .and(warp::filters::query::query::<HashMap<String, String>>())
        .map(list_entries_by_language);

    warp::serve(route).run(([127, 0, 0, 1], 3030)).await;
}