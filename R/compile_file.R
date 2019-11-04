#' Compile an R file into a JS file
#' @param input A character string; the input file.
#' @param output A character string; the output file. When the
#' output is "", the result is printed to the standard output.
#' @export
compile_r <- function(input, output = "") {
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
    rewrite_by_subst() %>%
    rewrite_by_cond_subst()   # order of the function calls matters
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
    # Binary operators
    make_rule("<-", "="),
    make_rule("<<-", "="),
    make_rule("^", "**"),
    make_rule("%%", "%"),
    make_rule("$", "."),
    make_rule(":", "R.seq_by"),
    make_rule("%instanceof%", "instanceof"),
    make_rule("%=>%", "=>"),
    make_rule("%+%", "+"),
    make_rule("self", "this")
  )
}

subst <- function(ast, pattern, replacement) {
    if (rlang::is_call(ast)) {
      return(as.call(purrr::map(ast, ~subst(.x, pattern, replacement))))
    }

    if (rlang::is_symbol(ast, pattern)) {
      return(as.symbol(replacement))
    }

    if (rlang::is_syntactic_literal(ast)) {
      if (is.null(ast))   return(ast)
      if (is.na(ast))     return(ast)
      if (ast == pattern) return(as.symbol(replacement))
    }

    # Handle the pairlist special case
    if (is.pairlist(ast)) {
      ast2 <- as.pairlist(purrr::map(ast, ~subst(.x, pattern, replacement)))
      names(ast2) <- names(ast)
      return(ast2)
    }

    ast
}


# Conditional substitute
rewrite_by_cond_subst <- function(ast) {
  magrittr::freduce(ast, cond_subst_rules())
}

cond_subst_rules <- function() {
  make_rule <- function(from, to) {
    function(x) cond_subst(x, pattern = from, replacement = to)
  }
  list(
    # Base Javascript
    make_rule("TRUE", "true"),
    make_rule("FALSE", "false"),
    make_rule("declare", "let"),
    # R-like functions
    make_rule(   "seq", "R.seq_by"),
    make_rule(     "c", "R.c"),
    make_rule("matrix", "R.matrix"),
    make_rule( "print", "R.print"),
    make_rule("length", "R.length"),
    make_rule(   "map", "R.map"),
    make_rule("reduce", "R.reduce"),
    make_rule(   "all", "R.all"),
    make_rule(   "any", "R.any"),
    # Math.js
    make_rule(  "pi", "Math.PI"),
    make_rule( "sin", "Math.sin"),
    make_rule( "cos", "Math.cos"),
    make_rule( "tan", "Math.tan"),
    make_rule( "sinh", "Math.sinh"),
    make_rule( "cosh", "Math.cosh"),
    make_rule( "tanh", "Math.tanh"),
    make_rule("asin", "Math.asin"),
    make_rule("acos", "Math.acos"),
    make_rule("atan", "Math.atan"),
    make_rule("asinh", "Math.asinh"),
    make_rule("acosh", "Math.acosh"),
    make_rule("atanh", "Math.atanh"),
    make_rule("min", "Math.min"),
    make_rule("max", "Math.max"),
    make_rule("round", "Math.round"),
    make_rule("abs", "Math.abs")
    # make_rule("NULL", "null"),   # doesn't work since R doesn't distinguish input NULL and empty NULL.
  )
}

cond_subst <- function(ast, pattern, replacement) {
  if (rlang::is_call(ast)) {
    if (rlang::is_call(ast, '.')) {
      return(ast)
    } else {
      return(as.call(purrr::map(ast, ~cond_subst(.x, pattern, replacement))))
    }
  }

  if (rlang::is_symbol(ast, pattern)) {
    return(as.symbol(replacement))
  }

  if (rlang::is_syntactic_literal(ast)) {
    if (is.null(ast))   return(ast)
    if (is.na(ast))     return(ast)
    if (ast == pattern) return(as.symbol(replacement))
  }

  # Handle the pairlist special case
  if (is.pairlist(ast)) {
    ast2 <- as.pairlist(purrr::map(ast, ~cond_subst(.x, pattern, replacement)))
    names(ast2) <- names(ast)
    return(ast2)
  }

  ast
}


# Rewriting AST (special form)
rewrite_new <- function(ast) {
  if (rlang::is_call(ast)) {
    if (rlang::is_call(ast, "$")) {
      cvar <- deparseR(ast[[2]])
      cop <- deparseR(ast[[3]])
      if (cop == "new") {
        return(as.symbol(glue::glue("new {cvar}")))
      }
    }
    as.call(purrr::map(ast, rewrite_new))
  } else {
    ast
  }
}


#====================================================================
#' Compile a data file into a JS file
#' @param input A character string; the path to the input file.
#' @param output A character string; the path to the output file.
#' @export
compile_data <- function(input, output) {
    if (missing(output)) output <- tempfile()
    write(to_json(input), file = output)
    output
}


to_json <- function(input) {
    fname <- basename(input)
    sym <- gsub(x = fname, pattern = "[.]", replacement = "_")

    if (is_csv(input)) {
      contents <- read.csv(input)
      json <- jsonlite::toJSON(contents, dataframe = "columns")
      glue::glue({"const {sym} = new dfjs.DataFrame({json})"})
    } else if (is_json(input)) {
      contents <- jsonlite::read_json(input)
      json <- jsonlite::toJSON(contents, dataframe = "columns", auto_unbox = T)
      glue::glue({"const {sym} = {json}"})
    } else {
      stop("The package only supports CSV and JSON data files at the moment.")
    }
}
