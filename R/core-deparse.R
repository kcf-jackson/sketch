# typed-deparser := (predicate, deparse)
# where
#   predicate :: ast -> bool
#   deparse   :: ast -> string
#
# The corresponding types in R are:
#   ast <--> lang; bool <--> logical; string <--> character.
#
# A list of typed-deparser must satisfy that
# 1. it is non-empty,
# 2. each member is a typed-deparser


#' Expression Deparsing for JS
#'
#' @param ast language object.
#' @param deparsers A list of "typed" deparsers.
#' @return A character string.
#'
#' @export
# deparse_js :: ast -> [typed-deparser] -> char
deparse_js <- function(ast, deparsers) {
  for (deparser in deparsers) { # use for-loop to avoid nested recursion
    if (deparser$predicate(ast)) {
      return(deparser$deparse(ast, deparsers))
    }
  }
  stop("The program reaches the end of the list of deparsers you provided; it means you have provided an input that no deparser in the list can handle. You should check your input or add the relevant deparser to the list of deparsers.")
}


#' A minimal list of deparsers for deparsing JavaScript
#' @export
basic_deparsers <- function() {
  list(
    # JavaScript object literal
    "list"    = make_deparser(is_call_list, deparse_list),
    # Keywords
    "pipe" = make_deparser(is_call_pipe, deparse_pipe),
    "lambda" = make_deparser(is_call_lambda, deparse_lambda),
    "ifelse" = make_deparser(is_call_ifelse, deparse_ifelse),
    "dataURI" = make_deparser(is_call_dataURI, deparse_dataURI),
    "new"  = make_deparser(is_call_new, deparse_new),
    "let"    = make_deparser(is_call_let, deparse_let),
    "for"    = make_deparser(is_call_for, deparse_for),
    "if"     = make_deparser(is_call_if, deparse_if),
    "while"  = make_deparser(is_call_while, deparse_while),
    "function" = make_deparser(is_call_function, deparse_function),
    # Operators
    "infix"  = make_deparser(is_call %&&% is_infix, deparse_infix),
    "wrap"   = make_deparser(is_call %&&% is_wrap, deparse_wrap),
    # Basic
    "call"   = make_deparser(is_call, deparse_call),
    "symbol" = make_deparser(is_sym, deparse_sym)
  )
}


#' A list of default deparsers for deparsing JavaScript
#' @export
default_deparsers <- function() {
  # Order is strict and the deparsers must be arranged such that the
  # specialised ones are at the top and the general ones are at the
  # bottom. This list acts like a sieve, and only inputs that do not
  # get caught at the top will fall to the bottom.
  list(
    # Library functions
    "R.add" = make_deparser(is_call_add, deparse_add),
    "R.subtract" = make_deparser(is_call_subtract, deparse_subtract),
    "R.extract2Assign" = make_deparser(is_call_extract2Assign, deparse_extract2Assign),
    "R.extractAssign" = make_deparser(is_call_extractAssign, deparse_extractAssign),
    "R.extract2" = make_deparser(is_call_extract2, deparse_extract2),
    "R.extract" = make_deparser(is_call_extract, deparse_extract),
    # Data structure
    "data.frame" = make_deparser(is_call_df, deparse_df),
    "R.summarise" = make_deparser(is_call_df_summarise, deparse_df_summarise),
    "R.mutate" = make_deparser(is_call_df_mutate, deparse_df_mutate),
    "list" = make_deparser(is_call_list, deparse_list),
    # Special forms
    "pipe" = make_deparser(is_call_pipe, deparse_pipe),
    "lambda" = make_deparser(is_call_lambda, deparse_lambda),
    "ifelse" = make_deparser(is_call_ifelse, deparse_ifelse),
    "dataURI" = make_deparser(is_call_dataURI, deparse_dataURI),
    "new"  = make_deparser(is_call_new, deparse_new),
    "let"  = make_deparser(is_call_let, deparse_let),
    # Keywords
    "for"    = make_deparser(is_call_for, deparse_for),
    "if"     = make_deparser(is_call_if, deparse_if),
    "while"  = make_deparser(is_call_while, deparse_while),
    "function" = make_deparser(is_call_function, deparse_function),
    # Operators
    "infix"  = make_deparser(is_call %&&% is_infix, deparse_infix),
    "wrap"   = make_deparser(is_call %&&% is_wrap, deparse_wrap),
    # Basic
    "call"   = make_deparser(is_call, deparse_call),
    "symbol" = make_deparser(is_sym, deparse_sym)
  )
}


#' A constructor for a "typed" deparser
#'
#' @param predicate_fun A function that takes a "lang" object and return a logical.
#' @param deparse_fun A function that takes a "lang" object and return a character string.
#' @return A list; a deparser ready to be dispatched by "type".
#'
#' @export
make_deparser <- function(predicate_fun, deparse_fun) {
  list(predicate = predicate_fun, deparse = deparse_fun)
}


`%&&%` <- function(p1, p2) {
  function(ast) p1(ast) && p2(ast)
}
