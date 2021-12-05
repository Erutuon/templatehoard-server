use memmap::Mmap;
use regex::{Error as RegexError, Regex};
use serde::{
    de, ser::SerializeStruct, Deserialize, Deserializer, Serialize, Serializer,
};
use std::{
    collections::BTreeMap,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    fs::File,
    io::Result as IoResult,
    ops::Deref,
    path::Path,
    str::FromStr,
};
use unicase::UniCase;
use warp::reply::{html, Reply};

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
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

#[test]
fn test_display_borrowed_template() {
    macro_rules! b_tree_map {
        () => {{ BTreeMap::new() }};
        ($( $key: expr => $val: expr ),+ $(,)?) => {{
            let mut map = ::std::collections::BTreeMap::new();
             $( map.insert($key, $val); )*
             map
        }};
    }

    assert_eq!(
        &TemplateBorrowed {
            name: "affix",
            parameters: b_tree_map! {
                "1" => "en",
                "2" => "de-",
                "3" => "orphan",
                "4" => "-ize",
                "5" => "-ed",
            },
            text: None,
        }
        .to_string(),
        "{{affix|en|de-|orphan|-ize|-ed}}"
    );

    assert_eq!(
        &TemplateBorrowed {
            name: "affix",
            parameters: b_tree_map! {
                "2" => "de-",
                "3" => "orphan",
                "4" => "-ize",
                "5" => "-ed",
            },
            text: None,
        }
        .to_string(),
        "{{affix|2=de-|3=orphan|4=-ize|5=-ed}}"
    );

    assert_eq!(
        &TemplateBorrowed {
            name: "odd test",
            parameters: b_tree_map! {
                "!" => "exclamation point",
                "1" => "one",
                "2" => "two",
            },
            text: None,
        }
        .to_string(),
        "{{odd test|!=exclamation point|one|two}}"
    );
}

#[derive(Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct TemplatesInPage<'a> {
    pub title: &'a str,
    pub templates: Vec<TemplateBorrowed<'a>>,
}

#[derive(Debug)]
pub struct TitleAndTemplates<'a> {
    pub title: UniCase<&'a str>,
    pub templates: Vec<TemplateBorrowed<'a>>,
}

impl<'a> Serialize for TitleAndTemplates<'a> {
    fn serialize<S: Serializer>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let mut state = serializer.serialize_struct("TitleAndTemplates", 2)?;
        state.serialize_field("title", &self.title.into_inner())?;
        state.serialize_field("templates", &self.templates)?;
        state.end()
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

pub fn mmap_file(path: &Path) -> IoResult<Mmap> {
    let file = File::open(path)?;
    unsafe { Mmap::map(&file) }
}

pub enum TextOrHTML<S: Into<String>> {
    Html(S),
    Text(S),
}

impl<S: Into<String> + Send> Reply for TextOrHTML<S> {
    fn into_response(self) -> warp::reply::Response {
        match self {
            Self::Html(s) => html(s.into()).into_response(),
            Self::Text(s) => s.into().into_response(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct SearchResults<'a> {
    pub complete: bool,
    pub templates: Vec<TitleAndTemplates<'a>>,
}

macro_rules! param_map {
    ($struct:ident: [$($field:ident),* $(,)?]) => {{
        [$((stringify!($field), &$struct.$field)),*]
    }}
}
