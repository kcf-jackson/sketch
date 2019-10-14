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
#' @param launch_browser A character string; "viewer" or "browser", which calls
#' `rstudioapi::viewer` and `utils::browserURL` respectively.
#' @export
source_r <- function(file, debug = F, launch_browser = "viewer") {
  if (debug) {
    index_js <- compile_r(file, output = "")  # print to console
    invisible(index_js)
  } else {
    index_js <- compile_r(file, tempfile())
    asset_tags <- assets(file)   # this line is needed to keep the working directory unchanged
    source_js(index_js, asset_tags, launch_browser = launch_browser)
  }
}


#' Source a p5.js file
#' @param file A character string; path to the JS file.
#' @param ... An optional list of shiny tags to be added to the <head> of
#' the html template.
#' @param launch_browser A character string; "viewer" or "browser", which calls
#' `rstudioapi::viewer` and `utils::browserURL` respectively.
#' @export
source_js <- function(file, ..., launch_browser = "viewer") {
  index_html <- htmltools::html_print(html_template(...), viewer = NULL)

  temp_dir <- tempdir()
  temp_html <- file.path(temp_dir, "index.html")
  temp_js <- file.path(temp_dir, "index.js")
  file.copy(
    from = c(index_html, file),
    to = c(temp_html, temp_js),
    overwrite = T
  )

  if (launch_browser == "viewer") {
    rstudioapi::viewer(temp_html)
  } else {
    utils::browseURL(temp_html)
  }
}
