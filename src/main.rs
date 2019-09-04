use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader};
use warp::{Filter, path, reply::html};
use regex::Regex;
use once_cell::sync::Lazy;
use rlua::{Lua, Result as LuaResult};

type LanguageCode = String;
type LanguageName = String;
static WIKTIONARY_LANGUAGE_CODE_TO_NAME: Lazy<HashMap<LanguageCode, LanguageName>> = Lazy::new(|| {
    let lua = Lua::new();
    lua.context(|ctx| {
        let langs: LuaResult<HashMap<LanguageCode, LanguageName>> = ctx.load(r#"
            require "mediawiki"
            local langs = require "Module:languages/code_to_name"
            return langs
        "#).call(());
        langs.unwrap_or_else(|e| {
            panic!("Error while loading table of languages: {}", e);
        })
    })
});

markup::define! {
    EntryLinks<'a>(
        lines: Vec<String>,
        lang: String,
        find: Option<&'a str>,
        find_regex: Option<String>
    ) {
        {markup::doctype()}
        html {
            head {
                title { "Entry name search results"}
                style {
                    "a { text-decoration: none; }
                    ul { column-width: 15em; }"
                }
            }
            body {
                #main {
                    p {
                        {Header { lang, find, find_regex }}
                    }
                    ul {
                        @for line in lines.iter() {
                            {WiktionaryLink { page: line, lang: &lang }}
                        }
                    }
                }
            }
        }
    }
    WiktionaryLink<'a>(page: &'a str, lang: &'a str) {
        li {
            a[href = {"https://en.wiktionary.org/wiki/".to_string() + page + "#" + lang}] {
                {page}
            }
        }
    }
    Header<'a>(
        lang: &'a str,
        find: &'a Option<&'a str>,
        find_regex: &'a Option<String>
    ) {
        {{let mut header = format!(
            "{} entry names",
            WIKTIONARY_LANGUAGE_CODE_TO_NAME.get(*lang)
                .map(String::as_str)
                .unwrap_or("?")
        );
        if let Some(f) = find {
            write!(header, " containing {}", CodeTag { content: f }).unwrap();
            if find_regex.is_some() {
                header.push_str(" and");
            }
        }
        if let Some(f) = find_regex {
            write!(header, " matching the regex {}", CodeTag { content: f }).unwrap();
        }
        markup::raw(header)}}
    }
    CodeTag<'a>(content: &'a str) {
        code {
            {content}
        }
    }
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
    Regex::new(r"^(?-u)([a-z]{2,3}(?:-[a-z]{2,3}){0,2})$").unwrap()
});

fn list_entries_by_language(filename: String, args: HashMap<String, String>) -> impl warp::reply::Reply {
    let language_code = match LANG_REGEX.captures(&filename) {
        Some(c) => c.get(1).unwrap().as_str(),
        None => return html(format!("Filename is not a language code.")),
    };
    let filename = format!("entries/{}.txt", language_code);
    if let Ok(file) = File::open(&filename) {
        let find_regex: Option<Regex> = match args.get("find_regex") {
            Some(r) => match Regex::new(&r) {
                Ok(r) => Some(r),
                Err(error) => return html(format!("{}", RegexError { error: error.to_string() })),
            },
            None => None,
        };
        let find = args.get("find");
        let file = BufReader::new(file);
        let lines: Result<Vec<_>, _> = file.lines()
            .filter(|line| {
                if let Ok(line) = line {
                    match find {
                        Some(find) => line.contains(find),
                        None => true,
                    }
                } else {
                    true
                }
            })
            .filter(|line| {
                if let Ok(line) = line {
                    match &find_regex {
                        Some(find_regex) => find_regex.is_match(line),
                        None => true,
                    }
                } else {
                    true
                }
            })
            .take(100)
            .collect();
        match lines {
            Ok(lines) => {
                html(format!(
                    "{}",
                    EntryLinks {
                        lines, lang: language_code.to_string(),
                        find: find.map(|s| s.as_str()),
                        find_regex: find_regex.map(|r| r.as_str().to_string())
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

fn main() {
    let route = warp::get2()
        .and(path!("entries" / String))
        .and(warp::path::end())
        .and(warp::filters::query::query::<HashMap<String, String>>())
        .map(list_entries_by_language);

    warp::serve(route).run(([127, 0, 0, 1], 3030));
}