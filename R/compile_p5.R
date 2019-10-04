#' Compile p5 R file into a JS file
#' @param input A character string; the input file.
#' @param output A character string; the output file.
#' @keywords internal
compile_p5_r <- function(input, output = "") {
  parse(file = input) %>%
    purrr::map(rewrite_ast, rules = ast_rules()) %>%
    purrr::map(deparse, width.cutoff = 200L) %>%
    purrr::map(rewrite_str, rules = str_rules()) %>%
    unlist() %>%
    write(file = output)
  output
}


#' Rewrite an AST
# rewrite_ast :: AST -> [(AST -> AST)] -> AST
#' @param ast A language object
#' @param rules A list of functions
# @examples
# ast <- rlang::parse_expr("x <- y$z ^ 4")
# rewrite_ast(as_list(ast), ast_rules())
#' @keywords internal
rewrite_ast <- function(ast, rules) {
  freduce(ast, rules, as_list, as_call)
}

ast_rules <- function() {
  list(
    f(x, subst(x, pattern = "<-", replacement = "=")),
    f(x, subst(x, pattern = "<<-", replacement = "=")),
    f(x, subst(x, pattern = "^", replacement = "%**%")),
    f(x, subst(x, pattern = ":", replacement = "seq_by")),
    f(x, subst(x, pattern = "pi", replacement = "Math.PI")),
    f(x, subst(x, pattern = "self", replacement = "this"))
  )
}


#' Rewrite a string
# rewrite_ast :: AST -> [(AST -> AST)] -> AST
#' @param str A character string
#' @param rules A list of functions
# @examples
# x <- "x = y %.% z %**% 4"
# rewrite_str(x, str_rules())
#' @keywords internal
rewrite_str <- function(str, rules) {
  freduce(str, rules)
}

str_rules <- function() {
  list(
    f(x, gsub(x, pattern = "%[*][*]%", replacement = "**")),
    f(x, gsub(x, pattern = "%%", replacement = "%")),
    rewrite_new, rewrite_dot
  )
}
