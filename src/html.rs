use markup::Render;

markup::define! {
    Page<T: Render, B: Render>(
        title: T,
        body: B,
    ) {
        {markup::doctype()}
        html {
            head {
                meta[charset="utf-8"];
                title {{title}}
                link[rel="stylesheet", type="text/css", href="/static/style.css"];
            }
            body {{body}}
        }
    }
}
