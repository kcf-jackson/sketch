#' Compile a data file into a JS file
#' @param input A character string; the path to the input file.
#' @param output A character string; the path to the output file.
#' @export
compile_data <- function(input, output) {
    if (missing(output)) output <- tempfile()
    write(to_json(input), file = output)
    output
}

# Convert a file into JSON
to_json <- function(input) {
    fname <- basename(input)
    sym <- gsub(x = fname, pattern = "[.]", replacement = "_")

    if (is_csv(input)) {
        contents <- read.csv(input)
        json <- jsonlite::toJSON(contents, dataframe = "columns")
        glue::glue({"const {sym} = new dfjs.DataFrame({json})"})
    } else if (is_json(input)) {
        contents <- jsonlite::read_json(input)
        json <- jsonlite::toJSON(contents, dataframe = "columns", auto_unbox = T)
        glue::glue({"const {sym} = {json}"})
    } else {
        stop("The package only supports CSV and JSON data files at the moment.")
    }
}

# Everything must be turned into JSON before loading.
# -> read into dataframe first, then convert to JSON
# -> test readr support
