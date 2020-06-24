#' Convert an asset link into a 'shiny.tag' object
#' @param x A character string; the header line (without the prefix #!).
#' @param processors A list of handlers for processing the '#!' header.
#' @return A shiny.tag object.
# convert_src :: char -> [header_processor] -> char
# where header_processor := (predicate, process)
convert_src <- function(x, processors = default_processors()) {
    for (processor in processors) {
        if (processor$predicate(x)) {
            return(processor$process(x))
        }
    }
}

#' List of handlers for processing the '#!' header
#' @export
default_processors <- function() {
    list(
        make_processor(load_library_pred, load_library_proc),
        make_processor(load_script_pred, load_script_proc),
        make_processor(always_true, load_error_proc)
    )
}

#' Make a handle to process header
#'
#' @param pred A function, taking a string and returning a logical.
#' @param fun A function, taking a string and returning a 'shiny.tag' object.
#' @return A header processor / handler.
#'
#' @export
make_processor <- function(pred, fun) {
    list(predicate = pred, process = fun)
}


#=====================================================================
# load_library_pred :: char -> logical
load_library_pred <- function(x) {
    ast <- rlang::parse_expr(x)
    rlang::is_call(ast, "load_library")
}

# load_script_pred :: char -> logical
load_script_pred <- function(x) {
    ast <- rlang::parse_expr(x)
    rlang::is_call(ast, "load_script")
}

# load_library_proc :: char -> shiny.tag
# Delegate to `load_library`
load_library_proc <- purrr::compose(eval, rlang::parse_expr)

# load_script_proc :: char -> shiny.tag
# Delegate to `load_script`
load_script_proc <- purrr::compose(eval, rlang::parse_expr)

#' Header functions
#' @rdname empty-headers
#' @param package A character string; name of a JavaScript library.
#' @param src A character string; the full web/local path to a JavaScript library.
#' @param ... Additional arguments to pass to header processor.
# load_library :: char -> ... -> shiny.tag
load_library <- function(package, ...) load_script(src(package), ...)

#' @rdname empty-headers
# load_script :: char -> ... -> shiny.tag
load_script <- function(src, ...) to_shiny_tag(src = src, ...)

# Load error messages
# load_error_proc :: char -> IO()
load_error_proc <- function(x) {
    ast <- rlang::parse_expr(x)
    stop(glue::glue("Command '{deparse(ast[[1]])}' is not supported."))
}


#=====================================================================
#' Load JavaScript / CSS / R Sketch Script / CSV file
#'
#' @param x A character string; the link / path to the JS / CSS / R script / CSV file.
# #' @param renderer function; the function to render the shiny tag object.
# #' Use `htmltools::doRenderTags` for RMD, and `identity` for `html_template`.
#'
#' @keywords internal
# to_shiny_tag :: char -> ... -> shiny.tag
to_shiny_tag <- function(src, ...) {
    # JS, CSS: local or web,  R, CSV: local only.
    # web links are kept as-is; local are turned into URI (except CSS is kept in-line).
    if (is_web_link(src)) {
        if (is_javascript(src)) return(load_web_js(src, ...))
        if (is_css(src))        return(load_web_css(src, ...))
        if (is_font(src))       return(load_web_font(src, ...))
    }

    if (is_local(src)) {
        if (is_javascript(src)) return(load_local_js(src, ...))
        if (is_css(src))        return(load_local_css(src, ...))
        if (is_r_script(src))   return(load_sketch_script(src, ...))
        if (is_data(src))       return(load_local_data(src, ...))
    }

    stop("Web support only works for JavaScript, CSS and Web-fonts links, and
         local script must be one of JavaScript, CSS, sketch, JSON and CSV.")
}

remove_attr <- function(x, attr) {
    x[attr] <- NULL
    return(x)
}

# load_* :: char -> shiny.tag
load_web_js <- function(x, ...) {
    script(src = x, ...)
}

load_web_css <- function(x, ...) {
    link(href = x, rel = "stylesheet", ...)
}

load_web_font <- function(x, ...) {
    link(href = x, rel = "preload", ...)
}

load_local_js <- function(x, ...) {
    script(src = dataURI(file = x), ...)
}

load_local_css <- function(x, ...) {
    content <- paste(readLines(x), collapse = "\n")
    style(content, rel = "stylesheet", ...)
}

load_sketch_script <- function(x, ...) {
    index_js <- compile_r(x, tempfile())
    script(src = dataURI(file = index_js), ...)
}

load_local_data <- function(x, ...) {
    index_js <- compile_data(x, tempfile())
    script(src = dataURI(file = index_js), ...)
}


# TODO Potential future development:
# - Handle optional arguments
#     - JS - async [DONE]
#
#     - JS : add script into body (use specific order?)
#     [DONE] Order of script is handled by `asset`
#
#     - JS: library - version [Cancelled]
#
#     - Cache data
#     [DONE] Specify path and disable overwrite
#
# - `convert_src` may need to be changed accordingly
