#' Bundle a list of files into a single JavaScript file
#'
#' @param fs A character vector; a list of R or JavaScript files.
#' The R files will be transpiled to JavaScript before bundling.
#'
#' @examples
#' library(sketch)
#' js <- bundle(c(src("dom"), src("io")))
#' cat(paste(readLines(js), collapse = "\n"))
#'
#' @export
bundle <- function(fs) {
    fs <- flatten_filelist(fs, "[.]((r)|(js))$", full.names = TRUE,
                           recursive = TRUE, ignore.case = TRUE)
    res_js <- file.path(tempdir(), "bundle.js")
    if (file.exists(res_js)) file.remove(res_js)
    for (file in fs) {
        file_extension <-  tolower(tools::file_ext(file))
        if (file_extension %in% c("r", "js")) {
            if (file_extension == "r") {
                temp <- tempfile()
                compile_r(file, temp)
            } else if (file_extension == "js") {
                temp <- file
            }
            lines <- readLines(temp)
            write(lines, file = res_js, append = TRUE)
            write("\n\n", file = res_js, append = TRUE)
        } else {
            warning("File '", file, " is not processed.")
        }
    }
    res_js
}


#' Flatten a list of files and directories into a list of files
#'
#' @param fs A character vector; a list of files.
#' @param pattern An optional regular expression to pass to `list.files` for
#' filtering files while expanding a directory into a list of files.
#' @param ... Additional parameters to pass to `list.files`.
#'
#' @export
flatten_filelist <- function(fs, pattern = NULL, ...) {
    fs %>%
        purrr::map(function(path) {
            if (!file.exists(path) && !dir.exists(path)) {
                stop("Path '", path, "' does not exist.")
            }
            if (dir.exists(path)) {
                return(list.files(path, pattern, ...))
            }
            if (file.exists(path)) {
                return(path)
            }
        }) %>%
        unlist()
}
