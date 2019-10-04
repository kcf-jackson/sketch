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


#--------------------------------------------------------------------
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
    source_p5_js(index_js)
  }
}


#' Source a p5.js file
#' @param index_js A character string; path to the JS file.
#' @export
source_p5_js <- function(index_js) {
  index_html <- index_html()

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


index_html <- function() {
  system.file("template/index.html", package = "p5sketch")
}
