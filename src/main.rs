use std::convert::Infallible;
use warp::{path, reject::Rejection, Filter, Reply, http::StatusCode};

mod entries;
mod html;
mod ipa;

async fn print_err(err: Rejection) -> Result<impl Reply, Infallible> {
    Ok(warp::reply::with_status(format!("{:?}\n", err), StatusCode::NOT_FOUND))
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
    warp::serve(route).run(([127, 0, 0, 1], 3030)).await;
}
