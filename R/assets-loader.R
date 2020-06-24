# @description Take a sketch R file as input and extract the resources links
# as provided by the user with the '#!' header.
#
# Instead of implementing 'async' loading of script, we move the sketch
# files to the bottom of the body.
#
# assets :: char -> asset_list
assets <- function(file, ..., trace = F) {
    headers <- extract_headers(file)
    if (purrr::is_empty(headers)) {
        return(asset_list(head = NULL, body = NULL))
    }
    headers %>%
        process_headers() %>%
        map(ifelse(trace, identity, convert_src), ...)
}

# extract_headers :: char -> [char]
extract_headers <- function(file) {
    is_header <- Vectorize(function(x) substr(x, 1, 2) == "#!")
    readLines(file) %>%
        filter(is_header) %>%
        purrr::map_dbl(~substring(.x, 3))  # Remove #!
}

# process_headers :: [char] -> asset_list
process_headers <- function(headers, ...) {
    first_arg <- function(x) rlang::parse_expr(x)[[2]]
    is_sketch <- Vectorize(purrr::compose(is_r_script, first_arg))

    parent_assets <- headers %>%
        sort_list(is_sketch) %>%
        rev() %>%  # Assets are moved to the top, R scripts are moved to the bottom
        as_asset_list()

    # Recursively build the assets dependencies in other sketch R files
    children_assets <- headers %>%
        filter(is_sketch) %>%
        purrr::map(assets, ...)
    if (purrr::is_empty(children_assets)) {
        return(parent_assets)
    }

    c(parent_assets, purrr::reduce(children_assets, c))
}

# Filter a vector according to a predicate function
filter <- function(x, predicate) x[predicate(x)]

# Sort a list into two lists according to a predicate function
sort_list <- function(x, predicate) list(x[predicate(x)], x[!predicate(x)])
