#' Process assets in headers
#'
#' @description Take a 'sketch' R file as input, extract and process the
#' resources links as provided by the user with the '#!' header.
#'
#' @param file Character string; the file path.
#' @param ... (Optional) List of processors to pass to \link{convert_src}.
#' @param trace TRUE or FALSE; if TRUE, assets are extracted, but not processed.
#'
#' @examples
#' file <- system.file("test_files/test_RMD.R", package = "sketch")
#' assets(file, trace = TRUE)
#' assets(file, trace = FALSE)
#'
#' @export
# assets :: char -> asset_list
assets <- function(file, ..., trace = FALSE) {
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
        purrr::map_chr(~substring(.x, 3))  # Remove #!
}

# process_headers :: [char] -> asset_list
process_headers <- function(headers, ...) {
    first_arg <- function(x) rlang::parse_expr(x)[[2]]
    is_sketch <- Vectorize(purrr::compose(is_r_script, first_arg))

    parent_assets <- headers %>%
        sort_list(is_sketch) %>%
        rev() %>%  # Assets are moved to the top, R scripts are moved to the bottom
        as_asset_list()

    # Recursively build the assets dependencies in other 'sketch' R files
    children_assets <- headers %>%
        filter(is_sketch) %>%
        purrr::map(first_arg) %>%
        purrr::map(assets, ..., trace = TRUE)
    if (purrr::is_empty(children_assets)) {
        return(parent_assets)
    }

    c(parent_assets, purrr::reduce(children_assets, c.asset_list))  # See Note 1 below
}
# Note 1: The dispatch of the second `c` needs to be explicitly stated, because
# at the moment,`c.asset_list` is not exported (and hence not registered with the
# S3 system). The issue is that if we pass the generic `c` down and call it at
# a lower level, it would looks up the S3 registry for dispatch (and fails to
# dispatch `c.asset_list`).
#
# It is different to the first `c`, which will be able to find `c.asset_list`
# since they are in the same frame of environment.
#
# In summary, local bindings (including package ones) are not persistent to the
# generics unless they are explicitly registered. This causes weird behaviour like
# the same symbol in the same line referring to different things, as in
#    `c(parent_assets, purrr::reduce(children_assets, c))`
# where the first dispatches `c.asset_list` properly while the second doesn't.

# Filter a vector according to a predicate function
filter <- function(x, predicate) x[predicate(x)]

# Sort a list into two lists according to a predicate function
sort_list <- function(x, predicate) list(x[predicate(x)], x[!predicate(x)])
