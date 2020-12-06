#' Perform pre-transpilation check
#'
#' @param ast A language object.
#' @param rules A list of functions; the rewriting rules, each of which
#' is the output from `make_rule`.
#' @return TRUE when the check is complete.
#'
#' @export
safeguard <- function(ast, rules) {
    to <- purrr::map_chr(rules, ~attr(.x, "to"))
    from <- purrr::map_chr(rules, ~attr(.x, "from"))
    reserved_words <- c(to, from)

    check <- function(ast) {
        if (rlang::is_call(ast)) {
            check_assignment(ast, reserved_words)
            check_function_arg(ast, reserved_words)
            return(purrr::walk(ast, check))
        }
        return(ast)
    }
    check(ast)
    return(TRUE)
}

# Check that reserved words are not assigned any value
check_assignment <- function(ast, reserved_words) {
    if (rlang::is_call(ast, c("=", "<-"))) {
        lhs <- deparse(ast[[2]])
        if (lhs %in% reserved_words) {
            warning(glue::glue("You assigned a value to the reserved word '{lhs}' in the following expression: {deparse_sym(ast)}"))
        }
    }
}

# Check that reserved words are not used as function arguments
check_function_arg <- function(ast, reserved_words) {
    if (rlang::is_call(ast, c("function"))) {
        if (!is.null(ast[[2]])) {
            farg_name <- names(ast[[2]])
            purrr::walk(farg_name, function(farg) {
                if (farg %in% reserved_words) {
                    warning(glue::glue("You used the reserved word '{farg}' as the function argument name in the following expression:{deparse_sym(ast)}"))
                }
            })
        }
    }
}
