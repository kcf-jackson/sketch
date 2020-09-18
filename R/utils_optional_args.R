#' Split a list of arguments into two sets
#'
#' @param args A named list; the list of arguments given by \code{list(...)}.
#' @param capture A character vector; the variables to keep.
#'
#' @examples
#' \dontrun{
#' f <- function(...) {
#'    capture_args(list(...), c("a", "b"))
#' }
#' f(a = 1, b = 2, c = 3, d = 4)
#' }
#'
#' @keywords internal
capture_args <- function(args, capture) {
    nargs <- names(args)
    keep <- intersect(nargs, capture)
    left <- setdiff(nargs, capture)
    list(keep = args[keep], left = args[left])
}


#' Execute a function call with extended arguments
#'
#' @param what A function.
#' @param ... Arguments to the function.
#' @param extended_args A list of arguments to the function call.
#'
#' @keywords internal
do_call <- function(what, ..., extended_args) {
    do.call(what, c(list(...), extended_args))
}
