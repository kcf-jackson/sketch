#' Apply a list of functions sequentially
#' @param value initial value.
#' @param function_list a list of functions.
#' @param preprocess a function for preprocessing the value.
#' @param postprocess a function for postprocessing the value.
#' @keywords internal
freduce <- function(value, function_list,
                    preprocess = identity,
                    postprocess = identity) {
    value %>%
        preprocess() %>%
        magrittr::freduce(function_list) %>%
        postprocess()
}


f <- pryr::f
