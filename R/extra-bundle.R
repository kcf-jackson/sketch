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
    res_js <- file.path(tempdir(), "bundle.js")
    for (file in fs) {
        if (!file.exists(file)) {
            stop("File '", file, "' does not exist.")
        }

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
