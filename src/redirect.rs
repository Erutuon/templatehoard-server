use std::str::FromStr;
use warp::{
    http::{uri::InvalidUri, Uri},
    path::FullPath,
    reject::{Reject, Rejection},
    Filter, Reply,
};

#[derive(Debug)]
enum RedirectRejection {
    InvalidUri(InvalidUri),
    Other,
}

impl Reject for RedirectRejection {}

impl From<InvalidUri> for RedirectRejection {
    fn from(err: InvalidUri) -> Self {
        RedirectRejection::InvalidUri(err)
    }
}

fn ends_in_slash_and_alpha(s: &str) -> bool {
    !s.ends_with('/')
        && s.bytes()
            .rev()
            .find(|b| b.is_ascii_alphabetic())
            == Some(b'/')
}

pub fn add_slash_if_no_extension(
) -> impl Filter<Extract = (impl Reply,), Error = Rejection> + Copy {
    warp::path::full()
        .and(warp::query::raw())
        .map(|path: FullPath, query: String| {
            log::debug!("path: {:?}, query: {:?}", path.as_str(), &query);
            let path = path.as_str();
            if ends_in_slash_and_alpha(path) {
                Some(format!("{}/?{}", path, &query))
            } else {
                None
            }
        })
        .or(warp::path::full().map(|path: FullPath| {
            log::debug!("path: {:?}", path.as_str());
            let path = path.as_str();
            if ends_in_slash_and_alpha(path) {
                Some(path.to_string() + "/")
            } else {
                None
            }
        }))
        .unify()
        .and_then(|url: Option<String>| async {
            if let Some(url) = url {
                // For now use temporary redirect in case there are errors.
                Uri::from_str(&url).map(warp::redirect::temporary).map_err(
                    |e| warp::reject::custom::<RedirectRejection>(e.into()),
                )
            } else {
                Err(warp::reject::custom(RedirectRejection::Other))
            }
        })
}
