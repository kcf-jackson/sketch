#' Insert a sketch into an RMD document
#'
#' @param file A character string; the path to the sketch file.
#' @param id A character string; an unique identifier for the sketch file.
#' @param output_dir A character string; a separate directory to save the
#' sketch app. Default to be NULL, which embeds the app in the Rmd file.
#' @param ... (Optional) Other attributes to pass to iframes.
#'
#' @note This function creates a temporary folder at the working directory.
#'
#' @export
insert_sketch <- function(file, id, output_dir = NULL, ...) {
    html_file <- source_r(file = file, launch_browser = NULL)
    if (is.null(output_dir)) {
        file_str <- paste(readLines(html_file), collapse = "\n")
        return(htmltools::doRenderTags(
            htmltools::tags$iframe(srcdoc = file_str, style="border: none;", ...)
        ))
    } else {
      temp_dir <- output_dir
      if (!dir.exists(temp_dir)) {
          stop(glue::glue("The output directory '{output_dir}' does not exist."))
      }

      temp_file <- file.path(temp_dir, paste0(id, ".html"))
      file.copy(html_file, temp_file, overwrite = F)

      return(htmltools::doRenderTags(
          htmltools::tags$iframe(src = temp_file, style="border: none;", ...)
      ))
    }
}
