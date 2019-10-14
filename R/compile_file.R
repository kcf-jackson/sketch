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
    make_rule("pi", "Math.pi"),
    make_rule("self", "this"),
    make_rule("%%", "%"),
    make_rule("$", "."),
    make_rule(":", "R.seq_by"),
    make_rule("seq", "R.seq_by"),
    make_rule("c", "R.c"),
    make_rule("matrix", "R.matrix"),
    make_rule("print", "R.print"),
    make_rule("length", "R.length"),
    # make_rule("NULL", "null"),   # doesn't work since R doesn't distinguish input NULL and empty NULL.
    make_rule("TRUE", "true"),
    make_rule("FALSE", "false"),
    make_rule("declare", "let"),
    make_rule("%instanceof%", "instanceof")
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

    ast
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

    contents <- read_file(input)
    json <- jsonlite::toJSON(contents, dataframe = "columns")

    glue::glue({"{sym} = new dfjs.DataFrame({json})"})
}
# Unit test
# print(to_json(read.csv("mtcars.csv"), "", assign_to = "myVar"))
# print(to_json(read.table("mtcars.txt"), "", assign_to = "myVar"))


read_file <- function(x) {
    read.csv(x)
}
