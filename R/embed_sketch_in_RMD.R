#' Load JavaScript / CSS / R Sketch Script / CSV file into an RMD document
#' @param x A character string; the link / path to the JS / CSS / R script / CSV file
#' @export
#' @examples
#' \dontrun{
#' load_script(src("p5"))
#' load_script("https://cdn.plot.ly/plotly-latest.min.js")
#' }
load_script <- function(x) {
  htmltools::doRenderTags(convert_src(x))
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
