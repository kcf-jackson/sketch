#' Source active file
#' @export
source_active <- function() {
  source_p5_r(copy_active_to_tempfile())
}


copy_active_to_tempfile <- function() {
  x <- rstudioapi::getSourceEditorContext()$contents
  temp_file <- tempfile()
  write(x, file = temp_file)
  temp_file
}


#' Source a p5.R file
#' @param index_r A character string; path to the R file.
#' @param debug T or F; if T, print compiled code on screen.
#' @export
source_p5_r <- function(index_r, debug = F) {
  if (debug) {
    index_js <- compile_p5_r(index_r, output = "")  # print to console
    invisible(index_js)
  } else {
    index_js <- compile_p5_r(index_r, tempfile())
    source_p5_js(index_js, load_js_lib(index_r))
  }
}


#' Source a p5.js file
#' @param index_js A character string; path to the JS file.
#' @export
source_p5_js <- function(index_js, ...) {
  index_html <- htmltools::html_print(html_template(...), viewer = NULL)

  temp_dir <- tempdir()
  temp_html <- file.path(temp_dir, "index.html")
  temp_js <- file.path(temp_dir, "index.js")
  file.copy(
    from = c(index_html, index_js),
    to = c(temp_html, temp_js),
    overwrite = T
  )

  getOption("viewer")(temp_html)
}


html_template <- function(...) {
  htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$script(src = src("math")),
        htmltools::tags$script(src = src("dataframe")),
        htmltools::tags$script(src = src("p5")),
        ...,
        htmltools::tags$script(src = "http://127.0.0.1:8080/utils.js")
      ),
      htmltools::tags$body(
        htmltools::tags$div(id = "p5_canvas"),
        htmltools::tags$script(src = "./index.js")
      )
    )
  )
}


#' Get the source link of a JavaScript library
#' @param x A character string; name of the JavaScript library
#' @export
src <- function(x) {
  switch(x,
         "dataframe" = "https://gmousse.github.io/dataframe-js/dist/dataframe.min.js",
         "math" = "https://cdnjs.cloudflare.com/ajax/libs/mathjs/6.2.2/math.min.js",
         "p5" = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js"
  )
}
