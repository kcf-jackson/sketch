# A type for a list of shiny.tag divided into head and body

# Constructor for "asset_list"
# asset_list := [head: [shiny.tag], body: [shiny.tag]]
#
# Note that no functions actually require the knowledge of
# how shiny.tag is implemented. Hence the type `shiny.tag`
# can be replaced by a generic type `a`, i.e.
# asset_list := [head: [a], body: [a]]
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

# Add element to the head
# append_to_head :: asset_list -> shiny.tag -> integer -> asset_list
append_to_head <- function(x, shiny_tag, after) {
    if (missing(after)) after <- length(ahead(x))
    asset_list(
        head = append(ahead(x), list(shiny_tag), after),
        body = abody(x)
    )
}

# Add element to the body
# append_to_body :: asset_list -> shiny.tag -> integer -> asset_list
append_to_body <- function(x, shiny_tag, after) {
    if (missing(after)) after <- length(abody(x))
    asset_list(
        head = ahead(x),
        body = append(abody(x), list(shiny_tag), after)
    )
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
map <- function(x, f, ...) UseMethod("map", x)
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
