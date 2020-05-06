use markup::Render;
use std::fmt::Display;

enum EitherIter<I1, I2> {
    First(I1),
    Second(I2),
}

impl<T, I1, I2> Iterator for EitherIter<I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::First(s) => s.next(),
            Self::Second(o) => o.next(),
        }
    }
}

markup::define! {
    Page<T: Render, B: Render + Display>(
        title: T,
        json: B,
    ) {
        {markup::doctype()}
        html[lang="en"] {
            head {
                meta[charset="utf-8"];
                title {{title}}
                // This path is fragile because it assumes that all pages are in the same directory.
                link[rel="stylesheet", type="text/css", href="../static/style.css"];
                script[src="../static/show-results.js"]{}
            }
            body {
                div[id="navigation"] {
                    div[id="prev"]{"prev"}
                    div[id="up"] {
                        a[href="?"] {
                            "search page"
                        }
                    }
                    div[id="next"]{"next"}
                }
                div[id="search-results-json"] {
                    {
                        markup::raw(
                            String::from_utf8(json
                            .to_string()
                            .bytes()
                            .flat_map(|b| {
                                match b {
                                    b'<' => EitherIter::First(b"&lt;".into_iter().copied()),
                                    b'&' => EitherIter::First(b"&amp;".into_iter().copied()),
                                    b => EitherIter::Second(std::iter::once(b)),
                                }
                            })
                            .collect()).unwrap())
                    }
                }
            }
        }
    }
}
