use memmap::Mmap;
use serde::Deserialize;
use serde_cbor::Deserializer;
use std::{
    collections::{BTreeMap, HashSet},
    fmt::{Display, Formatter, Result as FmtResult},
    fs::File,
};
use warp::{self, reply::html, Filter, Rejection, Reply};

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

fn ipa_search_results<'a, 'b: 'a>(
    deserializer: impl Iterator<Item = serde_cbor::Result<TemplatesInPage<'a>>>,
    search: Option<String>,
    langs_string: Option<Vec<&'b str>>,
) -> BTreeMap<&'a str, TemplateBorrowed<'a>> {
    let langs: Option<HashSet<_>> =
        langs_string.map(|l| l.into_iter().collect());
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
                    template
                        .parameters
                        .iter()
                        .skip(1)
                        .any(|(_k, v)| v.contains(s.as_str()))
                })
                .unwrap_or(true)
    };
    deserializer
        .flat_map(|val| {
            let TemplatesInPage { title, templates } = val.unwrap();
            templates
                .into_iter()
                .filter(&ipa_filter)
                .map(move |template| (title, template))
        })
        .collect()
}

#[derive(Deserialize)]
struct IPASearchParameters {
    search: Option<String>,
    langs: Option<String>,
}

markup::define! {
    TemplateAndTitle<'a, 'b> (
        title: &'b str,
        template: TemplateBorrowed<'a>
    ) {
        tr {
            td {
                a [href={format!("https://en.wiktionary.org/wiki/{}", title)}] {
                    {title}
                }
            }
            td {
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

pub fn ipa() -> impl Filter<Extract = (impl Reply,), Error = Rejection> + Clone
{
    warp::get()
        .and(warp::path!("ipa"))
        .and(warp::query::<IPASearchParameters>())
        .map(|params| {
            let file = File::open("cbor/IPA.cbor")
                .expect("could not open template dump file");
            let text =
                unsafe { Mmap::map(&file).expect("could not mmap file") };
            let IPASearchParameters {
                search,
                langs: langs_string,
            } = params;
            let langs = langs_string
                .as_ref()
                .map(|l| l.split(",").collect::<Vec<_>>());
            let deserializer =
                Deserializer::from_slice(&text).into_iter::<TemplatesInPage>();
            let results = ipa_search_results(deserializer, search, langs);

            html(format!(
                r#"
<html lang="en">
    <head>
        <meta charset="utf-8">
        <title>IPA result test!</title>
    </head>
    <body>
        <table>
            <tbody>
                {}
            </tbody>
        </table>
    </body>
</html>
"#,
                results
                    .into_iter()
                    .map(|(title, template)| TemplateAndTitle {
                        title,
                        template
                    }
                    .to_string())
                    .collect::<String>()
            ))
        })
}
