#' Source active file in 'RStudio'
#'
#' @param ... Optional arguments to pass to \code{source_r}.
#'
#' @examples
#' \dontrun{
#' # At 'RStudio', opens a 'sketch' R file in the editor, then
#' # run the following:
#' source_active()  # This launches the default HTML viewer.
#' }
#'
#' @export
source_active <- function(...) {
  source_r(copy_active_to_tempfile(), ...)  # nocov
}

copy_active_to_tempfile <- function() {
  x <- rstudioapi::getSourceEditorContext()$contents  # nocov start
  temp_file <- tempfile()
  write(x, file = temp_file)
  temp_file                                           # nocov end
}


#' Source a 'sketch' R file
#'
#' @description This function compiles a 'sketch' R file, resolves the
#' dependencies and serves it in the viewer.
#'
#' @param file A character string; path to the R file.
#' @param debug TRUE or FALSE; if TRUE, a console for debugging is attached to your app.
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browseURL` respectively; use
#' NULL to suppress display.
#' @param asset_tags An optional list of 'shiny.tag' objects to be added to the html
#' template. The list must have signature / structure of a named list:
#'     \code{[head = [shiny.tag], body = [shiny.tag]]},
#' @param ... Additional arguments to pass to `compile_r`.
#'
#' @examples
#' \dontrun{
#' file <- system.file("test_files/test_source.R", package = "sketch")
#' # The next line launches the default HTML browser
#' source_r(file, debug = TRUE, launch_browser = "browser")
#' }
#'
#' @export
source_r <- with_config(
  "file",
  function(file, debug = FALSE, launch_browser,
           asset_tags = default_tags(), ...) {
    if (missing(launch_browser)) {
      launch_browser <- ifelse(rstudioapi::isAvailable(), "viewer", "browser")
    }
    index_js <- compile_r(file, tempfile(), ...)
    file_asset <- assets(file)   # this line is needed to keep the working directory unchanged
    asset_tags <- c(asset_tags, file_asset)
    source_js(index_js, debug = debug, asset_tags = asset_tags,
              launch_browser = launch_browser)
  }
)

#' Serve a compiled 'sketch' JavaScript file
#'
#' @param file A character string; path to the compiled JS file.
#' @param debug TRUE or FALSE; if TRUE, a console for debugging is attached to your app.
#' @param asset_tags An optional list of 'shiny.tag' objects to be added to the html
#' template. The list must have signature / structure of a named list:
#'     \code{[head = [shiny.tag], body = [shiny.tag]]},
#' containing the \code{head} and \code{body} elements, each of which is a
#' list of \code{shiny.tag} object.
#' @param launch_browser A character string; "viewer" or "browser", which
#' calls `rstudioapi::viewer` and `utils::browseURL` respectively; use
#' NULL to suppress display.
#'
#' @examples
#' \dontrun{
#' file <- system.file("test_files/test_source.js", package = "sketch")
#' # The next line launches the default HTML browser
#' source_js(file, debug = TRUE, launch_browser = "browser")
#' }
#' @export
source_js <- function(file, debug = FALSE, asset_tags = default_tags(),
                      launch_browser) {
  if (debug) {
    debug_attr <- attributes(debug)
    if (length(debug_attr) == 0 || debug_attr$local) {
      debugger_file <- system.file("assets/console-log-div.js", package = "sketch")
      debugger_js <- js_to_shiny_tag(debugger_file)
    } else {
      debugger_cdn <- "https://cdn.jsdelivr.net/gh/kcf-jackson/sketch/inst/assets/console-log-div.js"
      debugger_js <- htmltools::tags$script(src = debugger_cdn)
    }
    asset_tags <- append_to_body(asset_tags, debugger_js)
  }
  if (missing(launch_browser)) {
    launch_browser <- ifelse(rstudioapi::isAvailable(), "viewer", "browser")
  }

  file_tag <- js_to_shiny_tag(file)
  html_doc <- html_builder(append_to_body(asset_tags, file_tag))
  if (is.null(launch_browser)) {
    viewer <- NULL
  } else {
    viewer <- switch(
      launch_browser,
      "viewer" = rstudioapi::viewer,
      "browser" = utils::browseURL,
      NULL
    )
  }
  html_print(html_doc, viewer = viewer)
}


#' HTML templates
#'
#' @name html_tags
#'
#' @description A list of 'shiny.tag' objects describing a HTML template. The
#' list must have signature / structure of a named list:
#'     \code{[head = [shiny.tag], body = [shiny.tag]]}
#'
#' @param local TRUE / FALSE. If TRUE, the R base module is loaded from the
#' local file stored in the package, otherwise, the module is served via a
#' content delivery network (CDN).
#'
#' @examples
#' str(default_tags())
#'
#' @export
default_tags <- function(local = TRUE) {
  if (local) {
    rjs <- system.file("assets/browser-R_core.js", package = "sketch")
    script_tag <- js_to_shiny_tag(rjs)
  } else {
    cdn <- "https://cdn.jsdelivr.net/gh/kcf-jackson/sketch-js/bin/dist/browser-R_core.js"
    script_tag <- htmltools::tags$script(src = cdn)
  }

  asset_list(
    head = list(
      htmltools::tags$meta(charset = "utf-8"),
      script_tag
    ),
    body = list()
  )
}

#' @rdname html_tags
#'
#' @examples
#' str(basic_tags())
#'
#' @export
basic_tags <- function() {
  asset_list(
    head = list(htmltools::tags$meta(charset = "utf-8")),
    body = list()
  )
}


# Convert a JavaScript file into a 'shiny.tag' object
# script_to_shiny_tag :: file -> shiny.tag
js_to_shiny_tag <- function(file) {
  fileURI <- base64enc::dataURI(file = file, mime = "application/javascript")
  htmltools::tags$script(src = fileURI)
}
