const defaultLimit = 500;
const defaultOffset = 0;
const elem = function(tagName, textContent) {
    const element = document.createElement(tagName);
    if (textContent) {
        element.textContent = textContent;
    }
    return element;
}

const onload = function(func) {
	if (document.readyState !== 'loading') {
		func();
	} else {
		document.addEventListener('DOMContentLoaded', func);
	}
}

const parseDecimalInt = function(num) {
    if (/^\d+$/.test(num)) {
        return parseInt(num);
    }
}

const showTemplate = function(template) {
    if (template.text) {
        return template.text;
    } else {
        const parts = [];
        parts.push(template.name);
        const keys = Object.keys(template.parameters);
        keys.sort();
        let sequential = true, prev = 0;
        for (key of keys) {
            const number = parseDecimalInt(key);
            const value = template.parameters[key];
            if (number) {
                sequential = sequential && number > 0 && number == prev + 1;
                if (sequential) {
                    parts.push(value);
                } else {
                    parts.push(key + "=" + value);
                }
                prev = number;
            } else {
                parts.push(key + "=" + value);
            }
        }
        return "{{" + parts.join("|") + "}}"
    }
}

const encloseContentsInAnchor = function(elem, href) {
    if (!(elem instanceof Element)) {
        throw new TypeError("elem should be Element");
    }
    const children = elem.childNodes;
    if (children.length !== 1) {
        throw new TypeError("elem should have exactly one child node");
    }
    const child = children[0];
    const anchor = document.createElement("a");
    if (href) {
        anchor.href = href;
    }
    anchor.appendChild(child);
    elem.appendChild(anchor);
}

const changeParams = function(url, params) {
    const newUrl = new URL(url);
    const newParams = new URLSearchParams(url.search);
    for ([k, v] of Object.entries(params)) {
        newParams.set(k, v);
    }
    newUrl.search = newParams;
    return newUrl;
}

const showResults = function() {
    const json = document.getElementById("search-results-json").textContent.trim();
    const data = JSON.parse(json);
    const baseUrl = new URL("https://en.wiktionary.org/wiki/");
    const rows = data.templates.map(page => {
        const row = elem("tr");
        const titleLink = elem("a", page.title);
        titleLink.href = new URL(page.title, baseUrl);
        const titleCell = elem("td");
        titleCell.appendChild(titleLink);
        titleCell.classList.add("title");

        const templatesCell = elem("td");
        const templates = page.templates.map(template => {
            const code = elem("code");
            code.textContent = showTemplate(template);
            return code;
        });
        let start = true;
        for (template of templates) {
            if (!start) templatesCell.appendChild(elem("br"));
            templatesCell.appendChild(template);
            start = false;
        }
        templatesCell.classList.add("templates");
        row.appendChild(titleCell);
        row.appendChild(templatesCell);
        return row;
    });
    const resultCount = rows.length;

    const url = new URL(document.URL);
    const params = new URLSearchParams(url.search);
    const offset = Number(params.get("offset") || defaultOffset);
    const limit = Number(params.get("limit") || defaultLimit);
    encloseContentsInAnchor(
        document.getElementById("prev"),
        offset >= 0 ? changeParams(url, { offset: Math.max(0, offset - limit) }) : null,
    );
    encloseContentsInAnchor(
        document.getElementById("next"),
        resultCount >= limit ? changeParams(url, { offset: offset + limit }) : null,
    );

    const table = elem("table");
    table.classList.add("search-results");
    const caption = elem(
        "caption",
        String(resultCount)
            + " result"
            + (resultCount === 1 ? "" : "s")
            + (data.complete ? "" : " or more")
    );
    const tableHeader = elem("thead");
    tableHeader.appendChild(elem("th", "page"));
    tableHeader.appendChild(elem("th", "templates"));
    const tableBody = elem("tbody");
    for (row of rows) {
        tableBody.appendChild(row);
    }
    table.appendChild(caption);
    table.appendChild(tableHeader);
    table.appendChild(tableBody);

    document.body.appendChild(table);
};

onload(showResults);