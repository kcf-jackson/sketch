# Take a sketch R file as input and extract the JS libraries resources
# links provided by the user with the '#!' header.
js_lib <- function(file) {
    file_lines <- readLines(file)
    keep <- purrr::map_lgl(file_lines, is_src_line)

    file_lines[keep] %>%
        purrr::map_chr(extract_src) %>%
        purrr::map(make_script_tag)
}


is_src_line <- function(x) {
    substr(x, 1, 2) == "#!"
}


# Takes string input:    "#! src = 'https://.../lib.js'"
# Returns string output: "https://.../lib.js"
extract_src <- function(x) {
    src0 <- strsplit(x, "=") %>%
        unlist() %>%
        tail(1) %>%
        trimws() %>%
        substr(., 2, nchar(.) - 1)
    if (!has_prefix(src0, "http")) {
        stop("Wrong input for 'src'.")
    } else {
        src0
    }
}


# Check if input 'x' has the 'y' prefix
has_prefix <- function(x, y) {
    substr(x, 1, nchar(y)) == y
}


make_script_tag <- function(x) {
    htmltools::tags$script(src = x)
}
