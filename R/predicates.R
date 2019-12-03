# Predicate functions

# Check if a character string is a weblink
is_web_link <- function(x) {
    has_prefix(x, "http://") || has_prefix(x, "https://")
}

# Check if input 'x' has the prefix 'y'
has_prefix <- function(x, y) substr(x, 1, nchar(y)) == y


# Extension-related predicates
is_javascript <- function(x) extname(x) == "js"
is_r_script   <- function(x) extname(x) == "r"
is_css        <- function(x) extname(x) == "css"
is_json       <- function(x) extname(x) == "json"
is_csv        <- function(x) extname(x) == "csv"
is_font       <- function(x) extname(x) %in% c("woff", "ttf", "eot", "otf")

# Get file extension
extname <- function(x) {
    filename <- basename(x)
    tolower(tail(unlist(strsplit(filename, "[.]")), 1))
}
