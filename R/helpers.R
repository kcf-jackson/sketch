#' Get the source link of a JavaScript library
#' @param x A character string; name of the JavaScript library
#' @export
src <- function(x) {
    switch(x,
           "dataframe" = "https://gmousse.github.io/dataframe-js/dist/dataframe.min.js",
           "math" = "https://cdnjs.cloudflare.com/ajax/libs/mathjs/6.2.2/math.min.js",
           "p5" = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js",
           "plotly" = "https://cdn.plot.ly/plotly-latest.min.js",
           "chart" = "https://cdn.jsdelivr.net/npm/chart.js@2.8.0",
           "d3" = "https://d3js.org/d3.v5.min.js",
           stop(glue::glue("Library '{x}' does not exist."))
    )
}


#' Empty functions
#' @description This function does nothing. It is created to ensure
#' the keywords `let` and `declare` are defined.
#' @param ... Any arguments
#' @export
let <- declare <- function(...) { invisible(NULL) }


#' Parse R code
#' @inheritParams rlang::parse_expr
#' @note This function is imported from `rlang`.
#' @export
parse_expr <- rlang::parse_expr
