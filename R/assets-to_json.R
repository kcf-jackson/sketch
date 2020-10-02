#' Compile a data file into a JavaScript file
#'
#' @param input A character string; the path to the input file.
#' @param output A character string; the path to the output file.
#' @param ... Extra arguments to be passed to \link{to_json}.
#'
#' @examples
#' file <- system.file("test_files/test_csv.csv", package = "sketch")
#' readLines(compile_data(file))
#'
#' @export
compile_data <- function(input, output = tempfile(), ...) {
    if (file.exists(output)) {
        message(glue::glue("The output data file '{output}' already exists. I'll load it instead of re-compiling the input file."))
        return(output)
    }
    write(to_json(input, ...), file = output)
    output
}


#' Convert a file into a JavaScript expression
#'
#' @param input A character string; the path to the input file.
#' @param as_data_frame TRUE or FALSE; whether the data are loaded as a data-frame.
#' @param read_fun A function to load the input file. Default settings are provided for
#' CSV files and JSON files. The function has to load a data file into an object that can
#' be handled by `jsonlite::toJSON`. Possible choices include `utils::read_delim`,
#' `readr::read_csv2`, etc.
#' @param ... Extra arguments to be passed to `read_fun`.
#'
#' @description It supports csv and json by default and lets users provide
#' custom handlers if other file formats are used.
# to_json :: Char (file path) -> Char (JS expression)
to_json <- function(input, as_data_frame, read_fun, ...) {
    fname <- basename(input)
    sym <- gsub(x = fname, pattern = "[.]", replacement = "_")

    if (is_json(input)) {
        as_data_frame <- as_data_frame %||% FALSE
        json <- paste(readLines(input), collapse = "\n")
        return(embed_data(sym, json, as_data_frame))
    }

    if (is_csv(input)) {
        as_data_frame <- as_data_frame %||% TRUE
        read_fun <- read_fun %||% read.csv
    } else {
        as_data_frame <- as_data_frame %||% stop("'as_data_frame' (TRUE or FALSE) must be provided when the file is not a CSV file or a JSON file.")
        read_fun <- read_fun %||% stop("'read_fun' (function) must be provided when the file is not a CSV file or a JSON file.")
    }
    contents <- read_fun(input, ...)
    json <- jsonlite::toJSON(contents, dataframe = "columns")
    return(embed_data(sym, json, as_data_frame))
}

# Set default parameter
`%||%` <- function(x, y) if (missing(x)) y else x

embed_data <- function(sym, json, as_data_frame = FALSE) {
    json <- glue::single_quote(json)
    ifelse(as_data_frame,
           glue::glue({"const {sym} = R.data_frame(JSON.parse({json}))"}),
           glue::glue({"const {sym} = JSON.parse({json})"})) # use JSON.parse as a safeguard / linter
}
