use warp::Filter;

mod entries;
mod ipa;

#[tokio::main]
async fn main() {
    warp::serve(entries::entries().or(ipa::ipa()))
        .run(([127, 0, 0, 1], 3030))
        .await;
}
