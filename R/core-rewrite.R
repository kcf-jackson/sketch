#' Interface for AST rewriting
#'
#' @param ast A language object.
#' @param rules A list of functions, each of which is the output from `make_rule`.
#' @return A language object.
#'
#' @examples
#' library(sketch)
#'
#' rewrite(
#'   ast = rlang::parse_expr("2 * (3 + pi)"),
#'   rules = list(make_rule("pi", "Math.PI"))
#' )
#'
#' rewrite(
#'   ast = rlang::parse_expr("2 + pi"),
#'   rules = list(
#'     make_rule("pi", "Math.PI"),
#'     make_rule("+", "Math.add")
#'   )
#' )
#'
#' @export
#
# rewrite :: language -> [function] -> language
rewrite <- function(ast, rules) {
  magrittr::freduce(ast, rules)
}


#' Make a AST transformation rule
#'
#' @param from A character string.
#' @param to A character string.
#' @return A function that takes a language object and returns a language object.
#'
#' @examples
#' library(sketch)
#'
#' rule_1 <- make_rule("pi", "Math.PI")
#' expr <- rlang::parse_expr("2 * (3 + pi)")
#'
#' rule_1(expr)  # this works but is not the preferred usage
#' rewrite(expr, list(rule_1))  # this is preferred
#'
#' rule_2 <- make_rule("+", "Math.add")
#' rewrite(expr, list(rule_1, rule_2))
#'
#' @export
#
# make_rule :: char -> char -> (language -> language)
make_rule <- function(from, to) {
  f <- function(x) subst(x, pattern = from, replacement = to)
  structure(f, class = "rule", from = from, to = to)  # facilitate custom print function
}

rm_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

# Implement a custom print function so that rewriting functions explain themselves
#' Print function for 'rule' objects
#'
#' @param x A 'rule' object.
#' @param ... (Unused) Optional arguments.
#'
#' @method print rule
#'
#' @examples
#' rule_1 <- make_rule("+", "Math.add")
#' print(rule_1)
#'
#' @export
print.rule <- function(x, ...) {
  from <- attr(x, 'from')
  to <- attr(x, 'to')
  print(glue::glue("Rule: Rewrite '{from}' to '{to}'."))
  print(rm_attributes(x))
  invisible(x)
}


# Rewriting AST by substitution
# subst :: language -> char -> char -> language
subst <- function(ast, pattern, replacement) {
    if (rlang::is_call(ast)) {
      # Exception handling with '.'. See Note 1 below for comments.
      if (rlang::is_call(ast, '.')) {
        ast[[2]] <- subst(ast[[2]], pattern, replacement)
        return(ast)
      }
      return(as.call(purrr::map(ast, ~subst(.x, pattern, replacement))))
    }

    if (rlang::is_symbol(ast)) {
      if (rlang::is_symbol(ast, pattern)) {
        return(as.symbol(replacement))
      } else {
        return(ast)
      }
    }

    if (rlang::is_syntactic_literal(ast)) {
      if (is.null(ast))   return(ast)  # this line is needed as NULL cannot be compared using `==`.
      if (is.na(ast))     return(ast)  # this line is needed as NA cannot be compared using `==`.
      if (is.character(ast))  return(ast)  # Quoted string should be kept as is
      if (ast == pattern) return(as.symbol(replacement))  # applies to syntactic literal TRUE and FALSE
      return(ast)
    }

    if (rlang::is_pairlist(ast)) {
      ast2 <- as.pairlist(purrr::map(ast, ~subst(.x, pattern, replacement)))
      names(ast2) <- names(ast)
      return(ast2)
    }

    # Handle reference to source code, as "srcref" is not a language object,
    # but it appears in function definitions. For more information, see
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/srcfile.html
    if (class(ast) == "srcref") {  # nocov start
      return(ast)
    }                              # nocov end

    # Other cases should return error for now.
    stop("The line should not be reached, please submit an issue with the input on github. Thanks!")  # nocov
}


# Note 1:
# '.' needs special handling because almost anything can go after '.'
# in JavaScript (including some keywords!). It is helpful to assume that
# things appeared after '.' should be treated as if they are quoted
# strings, and hence, no rewriting should be applied to them.
#
# Under this assumption, only the first part of a dot-chain gets rewritten,
# e.g. in the case of `obj_1.attr_l1.attr_l2.attr_l3', "obj_1" will get
# rewritten and all the attr_l* will not.
#
# Note that this does not rule out arguments in a function call, e.g.
# in "obj_1.fun(args)", both "obj_1" and "args" will get rewritten.
