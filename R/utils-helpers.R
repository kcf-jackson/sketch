#' Get the source link of a JavaScript library
#'
#' @param x A character string; name of the JavaScript library
#' @return A character string; the path to the library.
#'
#' @examples
#' src("mathjs")
#' src("p5")
#'
#' @export
src <- function(x) {
    switch(x,
           # Math
           "mathjs" = "https://cdnjs.cloudflare.com/ajax/libs/mathjs/7.0.2/math.min.js",
           "tensorflow" = "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@2.0.0/dist/tf.min.js",
           # Visualisation
           "p5" = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/1.0.0/p5.min.js",
           "chart" = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js",
           "plotly" = "https://cdn.plot.ly/plotly-latest.min.js",
           "d3" = "https://d3js.org/d3.v5.min.js",
           "vegalite" = "https://cdn.jsdelivr.net/npm/vega-lite@4.15.0/build/vega-lite.min.js",
           # Style sheets and Icons
           "fontawesome" = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css",
           "ionicons" = "https://unpkg.com/ionicons@5.2.3/dist/ionicons.js",
           "tailwind" = "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css",
           # Utility
           "ramda" = "https://cdnjs.cloudflare.com/ajax/libs/ramda/0.25.0/ramda.min.js",
           # Modules
           "dom" = system.file("modules/dom.js", package = "sketch"),
           "io" = system.file("modules/io.js", package = "sketch"),
           "websocket" = system.file("modules/websocket.js", package = "sketch"),
           "testthat" = system.file("modules/testthat.js", package = "sketch"),
           stop(glue::glue("Library '{x}' does not exist."))
    )
}


#' License information
#'
#' @param x A character string; name of the library / assets.
#' @return A named list containing the license information and
#' the link from which the information is extracted.
#'
#' @examples
#' license_info("mathjs")
#' license_info("p5")
#'
#' @export
license_info <- function(x) {
    switch(x,
           "mathjs" = list(license = "Apache-2.0", url = "https://github.com/josdejong/mathjs/blob/develop/LICENSE"),
           "tensorflow" = list(license = "Apache-2.0", url = "https://github.com/tensorflow/tfjs/blob/master/LICENSE"),
           "p5" = list(license = "LGPL-2.1-only", url = "https://p5js.org/copyright.html"),
           "chart" = list(license = "MIT", url = "https://github.com/chartjs/Chart.js/blob/master/LICENSE.md"),
           "plotly" = list(license = "MIT", url = "https://github.com/plotly/plotly.js/blob/master/LICENSE"),
           "d3" = list(license = "BSD-3-Clause", url = "https://github.com/d3/d3/blob/master/LICENSE"),
           "vegalite" = list(license = "BSD-3-Clause", url = "https://github.com/vega/vega-lite/blob/master/LICENSE"),
           "fontawesome" = list(license = "MIT", url = "https://fontawesome.com/v4.7.0/license/"),
           "ionicons" = list(license = "MIT", url = "https://unpkg.com/browse/ionicons@5.2.3/LICENSE"),
           "tailwind" = list(license = "MIT", url = "https://github.com/tailwindlabs/tailwindcss/blob/master/LICENSE"),
           "ramda" = list(license = "MIT", url = "https://github.com/ramda/ramda/blob/master/LICENSE.txt")
    )
}


#' Empty functions
#'
#' @name let-declare-const
#' @rdname empty-functions
#' @description These functions do nothing. It is created to ensure
#' the keywords `let` and `declare` are defined.
#'
#' @param ... Any arguments
#' @return NULL
#'
#' @examples
#' let (x)
#' let (x = 1, y = 2)
#' declare (x1, x2, x3)
#'
#' @export
let <- function(...) { invisible(NULL) }


#' @rdname empty-functions
#' @export
declare <- function(...) { invisible(NULL) }


#' @rdname empty-functions
#' @export
const <- function(...) { invisible(NULL) }



#' Parse R code
#'
#' @inheritParams rlang::parse_expr
#' @note This function is imported from `rlang`.
#'
#' @examples
#' parse_expr("x <- 1 + 1")
#'
#' @export
parse_expr <- rlang::parse_expr


script  <- htmltools::tags$script
link    <- htmltools::tags$link
style   <- htmltools::tags$style
dataURI <- base64enc::dataURI



line_separator <- function(x = "-") {
    paste0(rep(x, getOption("width")), collapse = "")
}
yellow <- function(x) paste0("\033[33m", x, "\033[39m")
# red <- function(x) paste0("\033[31m", x, "\033[39m")
green <- function(x) paste0("\033[32m", x, "\033[39m")  # nocov
