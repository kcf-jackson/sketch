#' Split a list of arguments into two sets
#'
#' @param args A named list; the list of arguments given by \code{list(...)}.
#' @param capture A character vector; the variables to keep.
capture_args <- function(args, capture) {
    nargs <- names(args)
    keep <- intersect(nargs, capture)
    left <- setdiff(nargs, capture)
    list(keep = args[keep], left = args[left])
}
