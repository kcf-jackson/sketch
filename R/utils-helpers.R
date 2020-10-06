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
           "mathjs" = "https://cdnjs.cloudflare.com/ajax/libs/mathjs/7.0.2/math.min.js",
           "p5" = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/1.0.0/p5.min.js",
           "chart" = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js",
           "plotly" = "https://cdn.plot.ly/plotly-latest.min.js",
           "d3" = "https://d3js.org/d3.v5.min.js",
           "vegalite" = "https://cdn.jsdelivr.net/npm/vega-lite@4.15.0/build/vega-lite.min.js",
           "tensorflow" = "https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@2.0.0/dist/tf.min.js",
           "dom" = system.file("modules/dom.js", package = "sketch"),
           stop(glue::glue("Library '{x}' does not exist."))
    )
}


#' Empty functions
#'
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
