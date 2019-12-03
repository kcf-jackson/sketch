html_template <- function(...) {
    args <- c(...)
    utils <- base64enc::dataURI(
        file = system.file("assets/utils.js", package = "sketch")
    )
    htmltools::tagList(
        htmltools::tags$html(
            htmltools::tags$head(
                htmltools::tags$script(src = src("math")),
                htmltools::tags$script(src = src("dataframe")),
                htmltools::tags$script(src = utils),
                args$head
            ),
            htmltools::tags$body(
                htmltools::tags$div(id = "new_sketch"),
                args$body
            )
        )
    )
}
