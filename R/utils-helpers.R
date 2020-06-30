#' Get the source link of a JavaScript library
#' @param x A character string; name of the JavaScript library
#' @export
src <- function(x) {
    switch(x,
           # "math" = "https://cdnjs.cloudflare.com/ajax/libs/mathjs/7.0.2/math.min.js",
           "p5" = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/1.0.0/p5.min.js",
           "chart" = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js",
           "plotly" = "https://cdn.plot.ly/plotly-latest.min.js",
           "d3" = "https://d3js.org/d3.v5.min.js",
           stop(glue::glue("Library '{x}' does not exist."))
    )
}


#' Empty functions
#' @rdname empty-functions
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


script  <- htmltools::tags$script
link    <- htmltools::tags$link
style   <- htmltools::tags$style
dataURI <- base64enc::dataURI
