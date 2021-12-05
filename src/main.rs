use getopts::Options;
use std::{
    convert::Infallible, fmt, num::NonZeroUsize, path::PathBuf, process::exit,
};
use warp::{http::StatusCode, path, reject::Rejection, Filter, Reply};

#[macro_use]
mod common;
mod error;
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

struct Args {
    static_dir: PathBuf,
    cbor_dir: PathBuf,
    port: u16,
    redirects: Option<PathBuf>,
}

macro_rules! crash_with_error {
    ($($expr:expr),* $(,)?) => {{
        eprintln!($($expr),*);
        exit(1);
    }}
}

macro_rules! canonicalize_and_bool_method {
    ($path:ident.$method:ident()) => {
        $path
            .canonicalize()
            .map(|p| p.$method())
            .unwrap_or(false)
    }
}

fn parse_args(args: impl IntoIterator<Item = String>) -> Args {
    let mut options = Options::new();
    options.reqopt(
        "c",
        "cbor",
        "path to directory of CBOR template dump files",
        "PATH",
    );
    options.reqopt("s", "static", "path to directory of static files", "PATH");
    options.optopt("r", "redirects", "path to template redirects JSON", "PATH");
    options.optopt("p", "port", "port number to listen at", "PORT");
    match options.parse(args) {
        Ok(parsed) => {
            let static_dir: PathBuf = parsed.opt_str("static").unwrap().into();
            let cbor_dir: PathBuf = parsed.opt_str("cbor").unwrap().into();
            let redirects: Option<PathBuf> =
                parsed.opt_str("redirects").map(|p| p.into());
            if !canonicalize_and_bool_method!(static_dir.is_dir())
            {
                crash_with_error!(
                    "{} is not a directory",
                    static_dir.display()
                );
            } else if !canonicalize_and_bool_method!(cbor_dir.is_dir())
            {
                crash_with_error!("{} is not a directory", cbor_dir.display());
            } else if let Some(redirects) = redirects.as_ref() {
                if !canonicalize_and_bool_method!(redirects.is_file())
                {
                    crash_with_error!("{} is not a file", redirects.display());
                }
            }
            let port: u16 = parsed
                .opt_str("port")
                .map(|p| {
                    p.parse().unwrap_or_else(|_| {
                        crash_with_error!("{} is not a valid port number", p)
                    })
                })
                .unwrap_or(3030);
            Args {
                static_dir,
                cbor_dir,
                redirects,
                port,
            }
        }
        Err(e) => {
            crash_with_error!("{}", e);
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();
    let args = parse_args(std::env::args().skip(1));
    let route = warp::get();
    let results_limit = NonZeroUsize::new(500).unwrap();
    let query_limit = NonZeroUsize::new(256).unwrap();
    let ipa_path = path!("ipa").and(ipa::handler(
        args.cbor_dir.join("IPA.cbor"),
        args.static_dir.join("ipa.html"),
        results_limit,
        query_limit,
    ));
    let templates_path = path!("templates").and(templates::handler(
        args.static_dir.join("templates.html"),
        args.redirects,
        args.cbor_dir,
        results_limit,
        query_limit,
    ));
    let static_path =
        path("static").and(warp::fs::dir(args.static_dir.clone()));
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
                .and(warp::fs::file(args.static_dir.join("index.html")))
                .or(static_path)
                .or(ipa_path)
                .or(templates_path)),
        )
        .recover(print_err)
        .with(log);
    warp::serve(route).run(([0, 0, 0, 0], args.port)).await;
}
