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


#' Combine rules for fast transpilation
#'
#' @description This function turns an n-pass transpilation into k-pass, where n is the
#' number of rules and k is the number of precedence groups.
#'
#' @param rs A list of rewriting rules (each of which is an output from \link{make_rule}).
#' @param group A numeric vector; the precedence group. Rules with a higher precedence
#' come before the the ones with lower precedence, and they are processed by the transpiler
#' first. For rules with the same precedence, the natural order (in which they show up)
#' determines which rules get processed first.
#'
#' @note The key insight about optimising the transpilation is that rewriting passes that
#' do not interfere with each other can be combined, and it saves a full traversal of the
#' parse tree.
#'
#' @export
combine_rules <- function(rs, group = rep(1, length(rs))) {
  unique(group) %>%
    sort(decreasing = TRUE) %>%
    purrr::map(~rs[which(group == .x)]) %>%
    purrr::map(join_rules)
}


#' Split rules for customisation
#'
#' @description This function is the left-inverse of `combine_rules`, i.e.
#' \code{split_rules(combine_rules(rs, group)) = rs} for any variable `group`.
#' It is created to facilitate the addition or removal of rewriting rules.
#'
#' @param rs A list of (grouped) rewriting rules. Note that a list of n rules
#' without grouping is a list of n groups of single rule.
#'
#' @export
# split_rules :: [function] -> [function]
split_rules <- function(rs) {
  rs %>%
    purrr::map(unjoin_rules) %>%
    unlist()
}

# join_rules :: [function] -> function
join_rules <- function(rs) {
  make_rule(purrr::map_chr(rs, ~attr(.x, "from")),
            purrr::map_chr(rs, ~attr(.x, "to")))
}

# unjoin_rules :: function -> [function]
unjoin_rules <- function(r) {
  purrr::map2(attr(r, "from"), attr(r, "to"), make_rule)
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
        index <- min(which(ast == pattern))
        return(as.symbol(replacement[index]))
      } else {
        return(ast)
      }
    }

    if (rlang::is_syntactic_literal(ast)) {
      # See Note 2
      if (is.null(ast) || is.na(ast) || is.character(ast)) {
        return(ast)
      }
      if (ast %in% pattern) {
        index <- min(which(ast == pattern))
        return(as.symbol(replacement[index]))  # applies to syntactic literal TRUE and FALSE
      }
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


# Note 1 ----
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


# Note 2 ----
# a. `is.null` and `is.na` are needed because NULL and NA cannot be compared using `==`.
# b. Note that `is.na(NaN)` returns TRUE.
# c. Quoted string should be kept as is.
# d. The reason why `ast` is returned as-is rather than `as.symbol("null")` is
# that R inserts a `NULL` when one defines a function without an argument.
# This can be checked with `parse_expr("function(){}")[[2]]`. Hence, `NULL`
# cannot be rewritten, as there is no way to distinguish whether such symbol
# is provided by user or inserted by R. Clearly, it is undesirable to see
# `function() {}` being transpiled to `function(null) {}`. This shall be
# tackled with the deparser.
