#' Perform pre-transpilation check
#'
#' @param ast A language object.
#' @param rules A list of functions; the rewriting rules, each of which
#' is the output from \link{make_rule}.
#' @param deparsers A list of functions; the deparsers, each of which
#' is the output from \link{make_deparser}.
#'
#' @return TRUE when the check is complete.
#'
#' @examples
#' # Expect no warning
#' safeguard(parse_expr("a <- 3"),
#'           rules = default_rules(),
#'           deparsers = default_deparsers())
#'
#' # Expect a warning (as `max` is reserved to be a function by the transpiler)
#' safeguard(parse_expr("max <- 3"),
#'           rules = default_rules(),
#'           deparsers = default_deparsers())
#'
#' @export
safeguard <- function(ast, rules, deparsers) {
    to <- rules %>%
        purrr::map(~attr(.x, "to")) %>%
        purrr::reduce(c)
    from <- rules %>%
        purrr::map(~attr(.x, "from")) %>%
        purrr::reduce(c)
    fun_name <- reserved_call(deparsers)
    reserved_words <- c(to, from, fun_name)

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

reserved_call <- function(deparsers) {
    res <- c()
    if ("dom" %in% names(deparsers)) {
        tags <-c("div", "span", "textarea",
                 "h1", "h2", "h3", "h4", "h5", "h6",
                 "em", "strong", "ul", "li", "blockquote", "hr",
                 "img", "script", "audio", "video", "canvas", "input", "link",
                 "section", "article", "header", "nav", "footer", "iframe",
                 "form", "option", "menu", "code", "pre", "style")
        res <- c(res, tags)
    }

    call_names <- c("lambda", "raw_str", "raw_string", "list",
                    "ifelse", "dataURI", "R6Class",
                    "new", "typeof", "let", "const",
                    "NULL", "NaN", "NA")
    for (call_name in call_names) {
        if (call_name %in% names(deparsers)) {
            res <- c(res, call_name)
        }
    }

    res
}


# Check that reserved words are not assigned any value
check_assignment <- function(ast, reserved_words) {
    if (rlang::is_call(ast, c("=", "<-"))) {
        lhs <- deparse(ast[[2]])
        if (lhs %in% reserved_words) {
            expr <- paste(deparse_sym(ast), collapse = "\n")
            warning(glue::glue("You assigned a value to the reserved word '{yellow(lhs)}' in the following expression:\n{yellow(expr)}"))
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
                    expr <- paste(deparse_sym(ast), collapse = "\n")
                    warning(glue::glue("You used the reserved word '{yellow(farg)}' as the function argument name in the following expression:\n{yellow(expr)}"))
                }
            })
        }
    }
}
