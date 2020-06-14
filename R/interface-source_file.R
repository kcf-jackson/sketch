#' Source active file in RStudio
#'
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browserURL` respectively; use
#' `NULL` to suppress display.
#'
#' @export
source_active <- function(launch_browser = "viewer") {
  source_r(copy_active_to_tempfile(), launch_browser = launch_browser)
}

copy_active_to_tempfile <- function() {
  x <- rstudioapi::getSourceEditorContext()$contents
  temp_file <- tempfile()
  write(x, file = temp_file)
  temp_file
}


#' Source a sketch R file
#'
#' @description This function compiles a sketch R file, resolves the
#' dependencies and serves it in the viewer.
#'
#' @param file A character string; path to the R file.
#' @param debug T or F; if T, print compiled code on screen.
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browserURL` respectively; use
#' `NULL` to suppress display.
#'
#' @export
source_r <- function(file, debug = F, launch_browser = "viewer") {
  if (debug) {
    index_js <- compile_r(file, output = "")  # print to console
    invisible(index_js)
  } else {
    index_js <- compile_r(file, tempfile())
    asset_tags <- assets(file)   # this line is needed to keep the working directory unchanged
    source_js(index_js, c(default_tags(), asset_tags), launch_browser = launch_browser)
  }
}


#' Source a sketch R file
#'
#' @param file A character string; path to the compiled JS file.
#' @param asset_tags An optional list of shiny tags to be added to the html
#' template. The list must have signature / structure of a named list:
#'     \code{[head = [shiny.tag], body = [shiny.tag]]},
#' containing the \code{head} and \code{body} elements, each of which is a
#' list of \code{shiny.tag} object.
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browserURL` respectively; use
#' "NULL" to suppress display.
#'
#' @export
source_js <- function(file, asset_tags = default_tags(), launch_browser = "viewer") {
  file_tag <- js_to_shiny_tag(file)
  html_doc <- html_builder(append_to_body(asset_tags, file_tag))

  viewer = list("viewer" = rstudioapi::viewer, "browser" = utils::browseURL, "NULL" = NULL)
  htmltools::html_print(html_doc, viewer = viewer[[launch_browser]])
}

default_tags <- function() {
  rjs <- system.file("assets/R-browser.js", package = "sketch")
  asset_list(
    head = list(
      htmltools::tags$script(src = src("dataframe")),
      js_to_shiny_tag(rjs)
    ),
    body = list(htmltools::tags$div(id = "sketch_1"))
  )
}

# Convert a JS file into a shiny tag
# script_to_shiny_tag :: file -> shiny.tag
js_to_shiny_tag <- function(file) {
  fileURI <- base64enc::dataURI(file = file, mime = "application/javascript")
  htmltools::tags$script(src = fileURI)
}
