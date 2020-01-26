use std::convert::Infallible;
use warp::{path, Filter, reject::Rejection, Reply};

mod entries;
mod html;
mod ipa;

async fn handle_rejection(err: Rejection) -> Result<impl Reply, Infallible> {
    Ok(format!("{:?}", err))
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
    let entries_path = path("entries").and(entries::route());
    let ipa_path = path("ipa").and(ipa::route());
    let static_path = path("static").and(warp::fs::dir("static"));
    let route = route.and(entries_path.or(ipa_path).or(static_path)).recover(handle_rejection);
    warp::serve(route).run(([127, 0, 0, 1], 3030)).await;
}
