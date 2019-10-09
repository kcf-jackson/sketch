#' Load js library into an RMD document
#' @param src A character string; the link to the js library.
#' @export
#' @examples
#' \dontrun{
#' load_js(src = src("p5"))
#' load_js(src = "https://cdn.plot.ly/plotly-latest.min.js")
#' }
load_js <- function(src) {
  htmltools::doRenderTags(htmltools::tags$script(src = src))
}


#' Insert a sketch into an RMD document
#' @param id A character string; the id for the <div> that contains
#' the sketch. This must match with the id used in the sketch file.
#' @param sketch_path A character string; the path to the sketch file.
#' @export
insert_sketch <- function(id = "new_sketch", sketch_path) {
    index_js <- compile_r(sketch_path, tempfile())
    htmltools::doRenderTags(
        list(
            htmltools::div(id = id),
            htmltools::tags$script(
                paste(readLines(index_js), collapse = "\n")
            )
        )
    )
}
