#! load_library("dom")

# Submit button
submit <- dom("button", list(id = "submit", innerText = "submit"))
submit$onclick <- function() {
    declare (R_obj, content, url)
    R_obj <- list(input = select_dom("#editor")$value)
    content <- encodeURIComponent(JSON$stringify(R_obj))
    url <- "http://0.0.0.0:8080/json?x=" %+% content
    console::log(content)
    fetch(url, list(method = "GET"))$
        then(function(response) {
            response$text()$then(function(text) {
                console::log(text)
                select_dom("#output")$srcdoc <- text
            })
        })
}
print_dom(submit)

# Editor and Output display
container_style <- "display:flex; height: 800px; margin: 12px;"
dom("div",list(id = "container", style = container_style)) %>%
    append_doms(
        dom("textarea", list(id = "editor", style = "width: 50%")),
        dom("iframe", list(id = "output", style = "width: 50%"))
    ) %>%
    print_dom()
