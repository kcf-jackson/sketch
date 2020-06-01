#' Insert a sketch into an RMD document
#'
#' @param file A character string; the path to the sketch file.
#' @param id A character string; an unique identifier for the sketch file.
#' @param ... (Optional) Other attributes to pass to iframes.
#'
#' @note This function creates a temporary folder at the working directory.
#'
#' @export
insert_sketch <- function(file, id, ...) {
    html_file <- source_r(file = file, launch_browser = NULL)

    temp_dir <- "./sketch_tmp/"
    if (!file.exists(temp_dir)) dir.create(temp_dir)

    temp_file <- file.path(temp_dir, paste0(id, ".html"))
    file.copy(html_file$html, temp_file, overwrite = F)

    htmltools::doRenderTags(
      htmltools::tags$iframe(src = temp_file, style="border: none;", ...)
    )
}
