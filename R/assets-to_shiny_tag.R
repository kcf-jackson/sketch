#' Convert an asset link into a 'shiny.tag' object
#' @param x A character string; the header line (without the prefix '#!'/'#|').
#' @param processors A list of handlers for processing the '#!'/'#|' header.
#' @return A 'shiny.tag' object.
# convert_src :: char -> [header_processor] -> shiny.tag
# where header_processor := (predicate, process)
convert_src <- function(x, processors = default_processors()) {
    for (processor in processors) {
        if (processor$predicate(x)) {
            return(processor$process(x))
        }
    }
}

#' Make a handle to process header
#'
#' @param pred A function, taking a string and returning a logical.
#' @param fun A function, taking a string and returning a 'shiny.tag' object.
#' @return A header processor / handler.
make_processor <- function(pred, fun) {
    list(predicate = pred, process = fun)
}

#' A list of handlers for processing the '#!'/'#|' header
#' @note This is used as input to \link{assets}.
#' @examples
#' default_processors()
#' @export
default_processors <- function() {
    list(
        make_processor(load_library_pred, load_library_proc),
        make_processor(load_script_pred, load_script_proc),
        make_processor(load_data_pred, load_data_proc),
        make_processor(config_pred, ignore_proc),
        make_processor(always_true, load_error_proc)
    )
}

#=====================================================================
# is_call_pred :: char -> (char -> logical)
is_call_pred <- function(name) {
    function(x) {
        is_call(parse_expr(x), name = name)
    }
}

eval_expr <- function(x) eval(parse_expr(x))

# load_library_pred :: char -> logical
load_library_pred <- is_call_pred("load_library")
# load_library_proc :: char -> shiny.tag
load_library_proc <- eval_expr  # Delegate to `load_library`


# load_script_pred :: char -> logical
load_script_pred <- is_call_pred("load_script")
# load_script_proc :: char -> shiny.tag
load_script_proc <- eval_expr  # Delegate to `load_script`


# load_data_pred :: char -> logical
load_data_pred <- is_call_pred("load_data")
# load_data_proc :: char -> shiny.tag
load_data_proc <- eval_expr  # Delegate to `load_data`


# config_pred :: char -> logical
config_pred <- is_call_pred("config")
# config_proc :: char -> NULL
ignore_proc <- function(x) NULL  # This is handled by extra-config.R


# Load error messages
# load_error_proc :: char -> IO()
load_error_proc <- function(x) {
    ast <- rlang::parse_expr(x)
    stop(glue::glue("Command '{deparse(ast[[1]])}' is not supported."))
}


#' Header functions
#' @name load_Family
#' @rdname header-functions
#' @param package A character string; name of a JavaScript library.
#' @param src A character string; the full web/local path to a JavaScript library.
#' @param x A character string; the full path to the file containing the data.
#' @param cache A character string; the full path to the cache file.
#' @param ... Additional arguments to pass to header processor.
# load_library :: char -> ... -> shiny.tag
load_library <- function(package, ...) {
    load_script(src(package), ...)
}

#' @rdname header-functions
# load_script :: char -> ... -> shiny.tag
load_script <- function(src, ...) {
    if (dir.exists(src)) {
        files <- list.files(src, pattern = "[.](r|(js))$", full.names = TRUE,
                            recursive = TRUE, ignore.case = TRUE)
        res <- purrr::map(files, ~to_shiny_tag(.x, ...))
        return(res)
    }
    to_shiny_tag(src = src, ...)
}

#' @rdname header-functions
load_data <- function(x, cache = tempfile(), ...) {
    index_js <- compile_data(x, cache, ...)
    script(src = dataURI(file = index_js, mime = "text/javascript"))
}


#=====================================================================
#' Load JavaScript / CSS / 'sketch' R / CSV file
#'
#' @param src A character string; the link / path to the JS / CSS / R script / CSV file.
# #' @param renderer function; the function to render the 'shiny.tag' object.
# #' Use `htmltools::doRenderTags` for RMD, and `identity` for `html_template`.
#'
#' @keywords internal
# to_shiny_tag :: char -> ... -> shiny.tag
to_shiny_tag <- function(src, ...) {
    # JS, CSS: local or web;  R, CSV: local only.
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
    }

    stop("Web support only works for JavaScript, CSS and Web-fonts links, and local script must be one of JavaScript, CSS, sketch files.")
}

# remove_attr <- function(x, attr) {
#     x[attr] <- NULL
#     return(x)
# }

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
    script(src = dataURI(file = x, mime = "text/javascript"), ...)
}

load_local_css <- function(x, ...) {
    content <- paste(readLines(x), collapse = "\n")
    style(content, rel = "stylesheet", ...)
}

load_sketch_script <- function(x, ...) {
    optional_args <- capture_args(list(...), c("rules", "deparsers"))
    ...1 <- optional_args$keep
    ...2 <- optional_args$left

    args_1 <- list(input = x, output = tempfile())
    index_js <- do.call(compile_r, append(args_1, ...1))

    args_2 <- list(src = dataURI(file = index_js, mime = "text/javascript"))
    do.call(script, append(args_2, ...2))
}
