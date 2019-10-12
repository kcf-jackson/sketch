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


#====================================================================
#' Compile a data file into a JS file
#' @param input A character string; the path to the input file.
#' @param output A character string; the path to the output file.
#' @export
compile_data <- function(input, output) {
    if (missing(output)) output <- tempfile()
    to_json(read_file(input), output)
}


to_json <- function(input, output) {
    if (missing(output)) output <- tempfile()
    sym <- gsub(x = basename(input), pattern = "[.]", replacement = "_")
    json <- jsonlite::toJSON(input, dataframe = "columns")
    data_js <- glue::glue({"{sym} = new dfjs.DataFrame({json})"})
    write(data_js, file = output)
    output
}
# Unit test
# print(to_json(read.csv("mtcars.csv"), "", assign_to = "myVar"))
# print(to_json(read.table("mtcars.txt"), "", assign_to = "myVar"))


read_file <- function(x) {
    read.csv(x)
}
