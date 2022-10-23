#| config(rules = basic_rules(), deparsers = default_deparsers())

# A simple interface to manipulate DOM elements in browser

#' Construct a new DOM element
#' @param tag0 A character string; name of the element.
#' @param attr0 A named list; attributes of the element.
#' @param ... (Optional) DOM elements to be added to the new DOM element.
#' @return A DOM element.
#' @export
dom <- function(tag0, attr0 = list()) {
    declare (domObj)
    domObj <- document$createElement(tag0)
    for (key in Object$keys(attr0)) {
        domObj[key] <- attr0[key]
    }

    let (args = Array(...arguments), args_len = args$length)
    if (args_len >= 3) {
        for (el in args$slice(2)) {
            domObj$appendChild(el)
        }
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

#' Attach a DOM element to another DOM element by selector
#' @param el A DOM element
#' @param x A character string (a CSS selector string) or a DOM element.
#' @return A DOM element; the parent element.
#' @export
print_dom <- function(el, x = "body") {
    if (typeof(x) == 'string') {
        declare (res)
        res <- document$querySelector(x)
        res$appendChild(el)
        return(res)
    } else {
        x$appendChild(el)
        return(x)
    }
}

# alias
render <- print_dom


#' Attach DOM elements to a DOM element
#' @param parent A DOM element
#' @param ... DOM elements
#' @return A DOM element; the parent element.
#' @export
append_doms <- function(parent) {
    let (args = Array(...arguments), args_len = args$length)
    if (args_len == 1) {
        return(parent)
    }
    for (el in args$slice(1)) {
        parent$appendChild(el)
    }
    return(parent)
}


#' Construct a new SVG element
#' @param tag0 A character string; name of the element.
#' @param attr0 A named list; attributes of the element.
#' @param ... (Optional) SVG elements to be added to the new SVG element.
#' @return A SVG element.
#' @export
svg <- function(tag0, attr0) {
    declare (svgObj)
    svgObj <- document$createElementNS('http://www.w3.org/2000/svg', tag0)
    for (key in Object$keys(attr0)) {
        svgObj$setAttribute(key, attr0[key])
    }

    let (args = Array(...arguments), args_len = args$length)
    if (args_len >= 3) {
        for (el in args$slice(2)) {
            svgObj$appendChild(el)
        }
    }
    return(svgObj)
}
