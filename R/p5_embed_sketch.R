#' Load p5.js library into an RMD document
#' @export
load_p5js <- function() {
  htmltools::doRenderTags(htmltools::tags$script(
    src = "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js"
  ))
}


#' Insert a sketch into an RMD document
#' @param id A character string; the id for the <div> that contains
#' the canvas. This must match with the id used in the sketch file.
#' @param sketch_path A character string; the path to the sketch file.
#' @export
insert_sketch <- function(id = "p5_canvas", sketch_path) {
    index_js <- compile_p5_r(sketch_path, tempfile())
    htmltools::doRenderTags(
        list(
            htmltools::div(id = id),
            htmltools::tags$script(
                paste(readLines(index_js), collapse = "\n")
            )
        )
    )
}
