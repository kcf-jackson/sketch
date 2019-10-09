#' Source active file
#' @export
source_active <- function() {
  source_r(copy_active_to_tempfile())
}


copy_active_to_tempfile <- function() {
  x <- rstudioapi::getSourceEditorContext()$contents
  temp_file <- tempfile()
  write(x, file = temp_file)
  temp_file
}


#' Source a p5.R file
#' @param file A character string; path to the R file.
#' @param debug T or F; if T, print compiled code on screen.
#' @export
source_r <- function(file, debug = F) {
  if (debug) {
    index_js <- compile_r(file, output = "")  # print to console
    invisible(index_js)
  } else {
    index_js <- compile_r(file, tempfile())
    source_js(index_js, js_lib(file))
  }
}


#' Source a p5.js file
#' @param file A character string; path to the JS file.
#' @param ... An optional list of shiny tags to be added to the <head> of
#' the html template.
#' @export
source_js <- function(file, ...) {
  index_html <- htmltools::html_print(html_template(...), viewer = NULL)

  temp_dir <- tempdir()
  temp_html <- file.path(temp_dir, "index.html")
  temp_js <- file.path(temp_dir, "index.js")
  file.copy(
    from = c(index_html, file),
    to = c(temp_html, temp_js),
    overwrite = T
  )

  getOption("viewer")(temp_html)
}
