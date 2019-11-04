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
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browserURL` respectively; use
#' `NULL` to suppress display.
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
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browserURL` respectively; use
#' `NULL` to suppress display.
#' @export
source_js <- function(file, ..., launch_browser = "viewer") {
  asset_tags <- c(...)
  fileURI <- base64enc::dataURI(file = file, mime = "application/javascript")

  asset_tags$body <- append(
    asset_tags$body,
    list(htmltools::tags$script(src = fileURI))
  )
  index_html <- htmltools::html_print(html_template(asset_tags), viewer = NULL)

  temp_dir <- tempdir()
  temp_html <- file.path(temp_dir, "index.html")
  file.copy(from = index_html, to = temp_html, overwrite = T)

  if (!is.null(launch_browser)) {
    if (launch_browser == "viewer") {
      rstudioapi::viewer(temp_html)
    } else {
      utils::browseURL(temp_html)
    }
  }

  invisible(list(html = temp_html))
}
