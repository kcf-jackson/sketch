# A type for a list of HTML tags divided into head and body

# Constructor for "asset_list"
# asset_list := [head: [shiny.tag], body: [shiny.tag]]
asset_list <- function(head = NULL, body = NULL) {
    structure(
        list(head = head, body = body),
        class = c("asset_list", "list")
    )
}
ahead <- function(x) x$head
abody <- function(x) x$body

# Predicate for "asset_list"
is.asset_list <- function(x) {
    inherits(x, "asset_list")
}

# Concatenate two "asset_list"
# c.asset_list :: asset_list -> asset_list -> asset_list
c.asset_list <- function(x, y) {
    asset_list(
        head = c(ahead(x), ahead(y)),
        body = c(abody(y), abody(x))
    )  # order is strict
}

# Turn a list into an "asset_list"
# as_asset_list: [[shiny.tag], [shiny.tag]] => asset_list
as_asset_list <- function(x) {
    if (!(is.list(x) && length(x) == 2)) {
        stop("Input must be a list of length 2.")
    }
    asset_list(head = x[[1]], body = x[[2]])
}

# Apply a function to each element of a list
map <- function(x, f, ...) {
    UseMethod("map", x)
}
map.asset_list <- function(x, f, ...) {
    asset_list(
        head = Map(f, ahead(x), ...),
        body = Map(f, abody(x), ...)
    )
}


# html_builder :: asset_list -> [shiny.tag]
html_builder <- function(x) {
    htmltools::tagList(
        htmltools::tags$html(
            htmltools::tags$head(ahead(x)),
            htmltools::tags$body(abody(x))
        )
    )
}


# html_template <- function(...) {
#     args <- c(...)
#     utils <- base64enc::dataURI(
#         file = system.file("assets/utils.js", package = "sketch")
#     )
#     htmltools::tagList(
#         htmltools::tags$html(
#             htmltools::tags$head(
#                 htmltools::tags$script(src = src("math")),
#                 htmltools::tags$script(src = src("dataframe")),
#                 htmltools::tags$script(src = utils),
#                 args$head
#             ),
#             htmltools::tags$body(
#                 htmltools::tags$div(id = "new_sketch"),
#                 args$body
#             )
#         )
#     )
# }
