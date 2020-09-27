#' Insert a 'sketch' app into an R Markdown document
#'
#' @param file A character string; the path to the 'sketch' file.
#' @param id A character string; an unique identifier for the 'sketch' file.
#' Needed only when \code{output_dir} is not \code{NULL}.
#' @param output_dir A character string; a separate directory to save the
#' 'sketch' app. Default to be NULL, which embeds the app in the Rmd file.
#' @param render TRUE or FALSE; if TRUE, call \link[htmltools]{doRenderTags};
#' if FALSE, return the 'shiny.tag' object.
#' @param ... (Optional) Other attributes to pass to iframes. Also supports
#' the `rules`, `deparsers` and `debug` options to pass to `source_r`.
#'
#' @examples
#' \dontrun{
#' # In an R code chunk of an R Markdown document
#' file <- system.file("test_files/test_RMD.R", package = "sketch")
#' insert_sketch(file, style = "width:500px; height:500px;")
#' }
#'
#' @export
insert_sketch <- function(file, id, output_dir = NULL, render = TRUE, ...) {
    opt_args <- capture_args(list(...), c("rules", "deparsers", "debug", "asset_tags"))
    html_file <- do_call(source_r, file = file, launch_browser = NULL,
                         extended_args = opt_args$keep)
    if (is.null(output_dir)) {
        file_str <- paste(readLines(html_file), collapse = "\n")
        res <- do_call(
          htmltools::tags$iframe,
          srcdoc = file_str, style="border: none;",
          extended_args = opt_args$left
        )
        if (render) return(htmltools::doRenderTags(res))
        return(res)
    } else {
        temp_dir <- output_dir
        if (!dir.exists(temp_dir)) {
            stop(glue::glue("The output directory '{output_dir}' does not exist."))
        }

        temp_file <- file.path(temp_dir, paste0(id, ".html"))
        file.copy(html_file, temp_file, overwrite = FALSE)
        res <- do_call(
          htmltools::tags$iframe,
          src = temp_file, style="border: none;",
          extended_args = opt_args$left
        )
        if (render) return(htmltools::doRenderTags(res))
        return(res)
    }
}


#' A language engine for 'sketch'
#'
#' @description This supports the use of 'sketch' code chunk in an R Markdown document.
#' @param options A list of chunk options.
#'
#' @examples
#' \dontrun{
#' # In the 'setup' code chunk of an R Markdown document
#' knitr::knit_engines$set(sketch = sketch::eng_sketch)
#' }
#'
#' @export
eng_sketch <- function(options) {
    out <- if (options$eval && knitr::is_html_output(excludes = 'markdown')) {
        src_file <- tempfile()
        write(options$code, file = src_file)

        opt_args <- capture_args(options, c("rules", "deparsers", "debug", "asset_tags", "style"))
        do_call(insert_sketch, file = src_file, extended_args = opt_args$keep)
    }
    options$results <- 'asis'
    knitr::engine_output(options, options$code, out)
}
