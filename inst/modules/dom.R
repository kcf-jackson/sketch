# A simple interface to manipulate DOM elements in browser

#' Construct a new DOM element
#' @param tag0 A character string; name of the element.
#' @param attr0 A named list; attributes of the element.
#' @return A DOM element.
#' @export
dom <- function(tag0, attr0) {
    domObj <- document$createElement(tag0)
    for (key in Object$keys(attr0)) {
        domObj[key] <- attr0[key]
    }
    return(domObj)
}

#' Select a DOM element
#' @param x A character string; a CSS selector string.
#' @return A DOM element.
#' @export
select_dom <- function(x) {
    return(document$querySelector(x))
}

#' Select multiple DOM elements
#' @param x A character string; a CSS selector string.
#' @return A list of DOM elements.
#' @export
select_doms <- function(x) {
    return(document$querySelectorAll(x))
}

#' Attach a DOM element to another DOM element
#' @param el A DOM element
#' @param x A character string; a CSS selector string.
#' @return A DOM element; the parent element.
#' @export
print_dom <- function(el, x = "body") {
    res <- document$querySelector(x)
    res$appendChild(el)
    return(res)
}
