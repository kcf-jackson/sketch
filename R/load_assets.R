# Take a sketch R file as input and extract the resources links as
# provided by the user with the '#!' header.
#
# Instead of implementing 'async' loading of script, we move the sketch
# files to the bottom of the body.
#
# assets_list := [head: [shiny.tag], body: [shiny.tag]]
# assets :: file -> assets_list
assets <- function(file) {
    is_header <- Vectorize(function(x) substr(x, 1, 2) == "#!")
    is_sketch <- Vectorize(is_r_script)
    filter <- function(x, pred) x[pred(x)]
    # Sort a list into two lists according to a predicate functions
    sort_list <- function(x, indicator, names) {
        setNames(list(x[indicator], x[!indicator]), names)
    }


    headers <- file %>%
        readLines() %>%
        filter(is_header) %>%
        purrr::map_chr(extract_src)
    if (purrr::is_empty(headers)) {
        return(list(head = NULL, body = NULL))
    }

    sketch_ind <- is_sketch(headers)
    parent_assets <- headers %>%
        purrr::map(convert_src) %>%
        # Assets are moved to the top, R scripts are moved to the bottom
        sort_list(sketch_ind, c("body", "head"))

    # Recursively build the assets dependencies in other sketch R files
    children_assets <- purrr::map(headers[sketch_ind], assets)
    if (purrr::is_empty(children_assets)) {
        return(parent_assets)
    }

    join_list(
        parent_assets,
        purrr::reduce(children_assets, join_list)
    )
}

# join_list :: assets_list -> assets_list -> assets_list
join_list <- function(x, y) {
    if (purrr::is_empty(x) || purrr::is_empty(y)) {
        stop("The input cannot be an empty list.")
    }
    list(head = c(x$head, y$head), body = c(y$body, x$body))  # order is strict
}


# Extract assets link
#
# Takes string input:    "#! load_script('https://abc/def.js')"
# Returns string output: "https://abc/def.js"
#
# Takes string input:    "#! load_library('p5')"
# Returns string output: "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js"
#
# extract_src :: char -> char
extract_src <- function(x) {
    ast <- rlang::parse_expr(substring(x, 3))
    if (rlang::is_call(ast, 'load_library')) {
        src(ast[[2]])
    } else if (rlang::is_call(ast, 'load_script')) {
        ast[[2]]
    } else {
        stop(glue::glue("Command '{deparse(ast[[1]])}' is not supported."))
    }
    # Potential future development:
    # - Handle optional arguments
    # - `convert_src` may need to be changed accordingly
    #
    # ast <- as.list(parse0(substring(x, 3)))
    # ret <- if (is.null(ast$src)) ast[[2]] else ast$src
    #
    # # Post-processing
    # if (!is.null(ast$cache)) {
    #     attr(ret, "cache") <- ast$cache
    # }
    # ret
}


#' Load JavaScript / CSS / R Sketch Script / CSV file
#'
#' @param x A character string; the link / path to the JS / CSS / R script / CSV file.
#' @param renderer function; the function to render the shiny tag object.
#' Use `htmltools::doRenderTags` for RMD, and `identity` for `html_template`.
#'
#' @keywords internal
#'
# convert_src :: char -> shiny.tag
convert_src <- function(x) {
    # JS, CSS: local or web,  R, CSV: local only.
    # web links are kept as-is; local are turned into URI (except CSS is kept in-line).
    script  <- htmltools::tags$script
    link    <- htmltools::tags$link
    style   <- htmltools::tags$style
    dataURI <- base64enc::dataURI

    if (is_web_link(x)) {
        if (is_javascript(x)) {
            script(src = x)

        } else if (is_css(x)) {
            link(href = x, rel = "stylesheet")

        } else if (is_font(x)) {
            link(href = x, rel = "preload")

        } else {
            stop("Web support only works for JavaScript, CSS and Web-fonts links.")
        }
    } else {
        if (is_javascript(x)) {
            URI <- dataURI(file = x)
            script(src = URI)

        } else if (is_css(x)) {
            content <- paste(readLines(x), collapse = "\n")
            style(content, rel = "stylesheet")

        } else if (is_r_script(x)) {
            index_js <- compile_r(x, tempfile())
            URI <- dataURI(file = index_js)
            script(src = URI)

        } else if (is_csv(x) || is_json(x)) {
            index_js <- compile_data(x, tempfile())
            URI <- dataURI(file = index_js)
            script(src = URI)

        } else {
            stop("Script must be one of JavaScript, CSS, sketch, JSON and CSV.")
        }
    }
}
