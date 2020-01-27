use std::convert::Infallible;
use warp::{http::StatusCode, path, reject::Rejection, Filter, Reply};

mod entries;
mod html;
mod ipa;

async fn print_err(err: Rejection) -> Result<impl Reply, Infallible> {
    Ok(warp::reply::with_status(
        format!("{:?}\n", err),
        StatusCode::NOT_FOUND,
    ))
}

#[tokio::main]
async fn main() {
    let root = std::env::args().nth(1);
    let route = warp::get();
    let route = if let Some(r) = root {
        route.and(path(r)).boxed()
    } else {
        route.boxed()
    };
    let entries_path = path("entries").and(entries::handler());
    let ipa_path =
        path("ipa").and(ipa::handler("cbor/IPA.cbor", "static/ipa.html"));
    let static_path = path("static").and(warp::fs::dir("static"));
    let route = route
        .and(entries_path.or(ipa_path).or(static_path))
        .recover(print_err);
    let port: u16 = std::env::var("PORT")
        .map(|s| s.parse().expect("could not parse PORT variable"))
        .unwrap_or(3030);
    warp::serve(route).run(([0, 0, 0, 0], port)).await;
}
