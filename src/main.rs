use env_logger;
use log;
use std::{
    convert::Infallible,
    fmt,
    num::NonZeroUsize,
};
use warp::{http::StatusCode, path, reject::Rejection, Filter, Reply};

mod common;
mod html;
mod ipa;
mod redirect;
mod templates;

struct OptFmt<T>(Option<T>);

impl<T: fmt::Display> fmt::Display for OptFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref t) = self.0 {
            fmt::Display::fmt(t, f)
        } else {
            f.write_str("-")
        }
    }
}

async fn print_err(err: Rejection) -> Result<impl Reply, Infallible> {
    Ok(warp::reply::with_status(
        format!("{:?}\n", err),
        StatusCode::NOT_FOUND,
    ))
}

#[tokio::main]
async fn main() {
    env_logger::init();
    let root = std::env::args().nth(1);
    let route = warp::get();
    let route = if let Some(r) = root {
        route.and(path(r)).boxed()
    } else {
        route.boxed()
    };
    let results_limit = NonZeroUsize::new(500).unwrap();
    let query_limit = NonZeroUsize::new(256).unwrap();
    let ipa_path = path!("ipa").and(ipa::handler(
        "cbor/IPA.cbor",
        "static/ipa.html",
        results_limit,
        query_limit,
    ));
    let templates_path = path!("templates").and(templates::handler(
        "static/templates.html",
        "template_redirects.json",
        results_limit,
        query_limit,
    ));
    let static_path = path("static").and(warp::fs::dir("static"));
    let log = warp::log::custom(|info| {
        log::info!(
            target: "all",
            r#""{} {} {:?}" {} "{}" "{}" {:?}"#,
            info.method(),
            info.path(),
            info.version(),
            info.status().as_u16(),
            OptFmt(info.referer()),
            OptFmt(info.user_agent()),
            info.elapsed(),
        );
    });
    let route = route
        .and(
            redirect::add_slash_if_no_extension().or(warp::path::end()
                .and(warp::fs::file("static/index.html"))
                .or(static_path)
                .or(ipa_path)
                .or(templates_path)),
        )
        .recover(print_err)
        .with(log);
    let port: u16 = std::env::var("PORT")
        .map(|s| s.parse().expect("could not parse PORT variable"))
        .unwrap_or(3030);
    warp::serve(route).run(([0, 0, 0, 0], port)).await;
}
