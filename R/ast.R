#' Search a list for 'pattern' and replace each occurrence by 'replacement'
#' @param x A list. The list to search.
#' @param pattern A character string. The pattern to search for.
#' @param replacement A character string. The replacement.
# @examples
# ast <- parse(text = "x = 1 + 2")[[1]]
# asl <- as_list(ast)
# subst(asl, "=", "<-")
subst <- function(x, pattern, replacement) {
  if (is.pairlist(x)) {
    x
  } else if (is.list(x)) {
    purrr::map(x, subst, pattern = pattern, replacement = replacement)
  } else {
    if (rlang::is_symbol(x, pattern)) {
      as.symbol(replacement)
    } else {
      x
    }
  }
}


#' Turns a (nested-)list into an AST
# as_call :: list -> ast
#' @param x A list.
# @examples
# ast <- parse(text = "x = 1 + 2")[[1]]
# as_call(as_list(ast))
as_call <- function(x) {
  if (is.pairlist(x)) {
    if (is.null(x)) {  # pairlist can be NULL...
      x
    } else {
      as.call(purrr::map(names(x), as.symbol))
    }
  } else if (is.list(x)) {
    as.call(purrr::map(x, as_call))
  } else {
    x
  }
}


#' Turns an AST into a (nested-)list
# as_list :: ast -> list
#' @param ast A language object.
# @examples
# ast <- parse(text = "x = 1 + 2")[[1]]
# as_list(ast)
as_list <- function(ast) {
  if (is.call(ast)) {
    purrr::map(ast, as_list)
  } else {
    ast
  }
}


f <- pryr::f
