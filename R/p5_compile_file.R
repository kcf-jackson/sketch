#' Compile p5 R file into a JS file
#' @param input A character string; the input file.
#' @param output A character string; the output file. When the
#' output is "", the result is printed to the standard output.
#' @export
compile_p5_r <- function(input, output = "") {
  parse(file = input) %>%
    purrr::map(rewrite) %>%
    purrr::map(deparse0) %>%
    unlist() %>%
    write(file = output)
  output
}


# Interface for AST rewriting
rewrite <- function(ast) {
  ast %>%
    rewrite_new() %>%
    rewrite_by_subst()
}


# Rewriting AST by simple substitution
rewrite_by_subst <- function(ast) {
  magrittr::freduce(ast, subst_rules())
}

subst_rules <- function() {
  make_rule <- function(from, to) {
    function(x) subst(x, pattern = from, replacement = to)
  }

  list(
    make_rule("<-", "="),
    make_rule("<<-", "="),
    make_rule("^", "**"),
    make_rule(":", "seq_by"),
    make_rule("pi", "Math.pi"),
    make_rule("self", "this"),
    make_rule("%%", "%"),
    make_rule("$", ".")
  )
}

subst <- function(ast, pattern, replacement) {
  if (is.call(ast)) {
    as.call(purrr::map(ast, ~subst(.x, pattern, replacement)))
  } else {
    if (rlang::is_symbol(ast, pattern)) {
      as.symbol(replacement)
    } else {
      ast
    }
  }
}


# Rewriting AST (special cases)
rewrite_new <- function(ast) {
  if (rlang::is_call(ast)) {
    if (rlang::is_call(ast, "$")) {
      cvar <- deparse(ast[[2]])
      cop <- deparse(ast[[3]])
      if (cop == "new") {
        return(as.symbol(glue::glue("new {cvar}")))
      }
    }
    as.call(purrr::map(ast, rewrite_new))
  } else {
    ast
  }
}


# # Unit test
# unit_test <- function(x) {
#   f <- function(x) rewrite(parse0(x))
#   g <- compose(deparse, f)
#   h <- compose(deparse0, f)
#   cat("==========Test==========", "\n")
#   cat("Input             : ", x, "\n")
#   cat("Output (deparse)  : ", g(x), "\n")
#   cat("Output (deparse0) : ", h(x), "\n")
# }
# unit_test("a <- x <<- 10")
# unit_test("a <<- b <- 3 + 4")
# unit_test("3^2 + 14")
# unit_test("1:10 + 120")
# unit_test("3 * pi - 3")
# unit_test("self$abc(123)")
# unit_test("123 %% 5 == 4")
# unit_test("obj_1$method_1(x)")
# unit_test("obj_1$new(x, y)")
