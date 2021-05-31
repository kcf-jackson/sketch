# === Deparsers for symbols and calls ========================
# To construct a "master"-deparser, at the most basic level
# one must provide a deparser for each of symbols and calls.


# Deparser for symbols --------------------------------------
#' Predicate for symbols, i.e. symbols or syntactic literals
# For completeness, we use the negation of `is_call` to include
# also the pairlist case.
# Reference: https://adv-r.hadley.nz/expressions.html#summary
#' @name is_Family
#' @rdname predicate_component
#' @param ast A language object.
is_sym <- function(ast) !is_call(ast)

#' Deparsers (specialised)
#' @name deparse_Family
#' @rdname deparsers_component
#' @param ast A language object.
#' @param ... The contextual information to be passed on to
#' the next call.
#' @return A character string.
deparse_sym <- function(ast, ...) {
  deparse(ast, width.cutoff = 500L)
}


# Deparser for syntactic literal ---------------------------------------
#' Predicate for syntactic literal
#' @inheritParams rlang::is_syntactic_literal
#' @note This function is imported from `rlang`.
is_syntactic_literal <- rlang::is_syntactic_literal

#' Deparser for NULL
#' @rdname deparsers_component
deparse_NULL <- function(ast, ...) return("null")

#' Deparser for NA
#' @rdname deparsers_component
deparse_NA <- function(ast, ...) return("undefined")

#' Deparser for NaN
#' @rdname deparsers_component
deparse_NaN <- function(ast, ...) return("NaN")


# Deparser for calls ---------------------------------------
#' Predicate for calls
#' @inheritParams rlang::is_call
#' @note This function is imported from `rlang`.
is_call <- rlang::is_call

#' Deparser for calls
#' @rdname deparsers_component
deparse_call <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  fun <- sym_ls[1]
  args <- paste0(sym_ls[-1], collapse = ", ")
  paste0(fun, "(", args, ")")
}


# === Deparsers for infix and wrap operators ==================
# Continuing from above, one could further distinguish special
# cases of calls. Here we consider the infix operators and
# wrap operators ("{", "[", "[[", "(").


# Deparser for infix operators ------------------------------
#' Predicate for infix operators
#' @rdname predicate_component
is_infix <- function(ast) {
  infix_ops <- c("+", "-", "*", "**", "/", "%",
                 "=", "==", "!=", "<", ">", "<=", ">=",
                 "!", "&&", "||", "instanceof",
                 "&", "|", ":", "?", ".", "=>")
  sym <- deparse_sym(ast[[1]])
  isTRUE(sym %in% infix_ops)
}

#' Deparser for infix operators
#' @rdname deparsers_component
deparse_infix <- function(ast, ...) {
  is_unary <- length(ast) == 2
  if (is_unary) {
    # Unary operators ("-", "!")
    operator <- deparse_js(ast[[1]], ...)
    operand <- deparse_js(ast[[2]], ...)
    paste0(operator, operand)
  } else {
    # Binary operators
    op <- deparse_js(ast[[1]], ...)
    lhs <- deparse_js(ast[[2]], ...)
    rhs <- deparse_js(ast[[3]], ...)
    paste(lhs, op, rhs, sep = space_symbol(op))
  }
}

# Symbols table / mapping
space_symbol <- function(chr) {
  # infix operators
  space <- c("=", "+", "-", "*",
             "%", "**", "instanceof", "=>",
             "==", "!=", "<", ">", "<=", ">=",
             "&&", "||", "&", "|",
             "?", ":")
  no_space <- c(".", "/", "!")

  if (chr %in% space) return(" ")
  if (chr %in% no_space) return("")
  return(" ")  # custom infix operator
}


# Deparser for wraps / brackets operators -------------------
#' Predicate for brackets
#' @rdname predicate_component
is_wrap <- function(ast) {
  wrap_ops <- c("{", "(", "[", "[[")
  sym <- deparse_sym(ast[[1]])
  isTRUE(sym %in% wrap_ops)
}

#' Deparser for brackets
#' @rdname deparsers_component
deparse_wrap <- function(ast, ...) {
  sym <- deparse_js(ast[[1]], ...)
  switch(sym,
         "["  = deparse_wrap_sb(ast, ...),
         "[[" = deparse_wrap_sb(ast, ...),
         "("  = deparse_wrap_rb(ast, ...),
         "{"  = deparse_wrap_cb(ast, ...),
         stop(glue::glue("Haven't implemented deparse for symbol '{sym}'"))   # nocov
  )
}

deparse_wrap_sb <- function(ast, ...) {
  # case "[" and "[["
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  sym <- sym_ls[1]
  paste0(
    sym_ls[2],
    sym,
    paste0(sym_ls[-(1:2)], collapse = sep_symbol(sym)),
    close_symbol(sym)
  )
}

deparse_wrap_rb <- function(ast, ...) {
  # Case '('
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  sym <- sym_ls[1]
  paste0(
    sym,
    paste0(sym_ls[-1], collapse = sep_symbol(sym)),
    close_symbol(sym)
  )
}

deparse_wrap_cb <- function(ast, ...) {
  # Case '{'
  increase_indent <- function(x) {
    indent <- "   "
    indent_nl <- paste0("\n", indent, " ")
    paste(
      indent,
      gsub(x = x, pattern = "\n", replacement = indent_nl)
    )   # `gsub` is fine here since '\n' in quoted string becomes '\\n'
  }

  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  sym <- sym_ls[1]
  paste(
    sym,
    paste0(increase_indent(sym_ls[-1]), collapse = sep_symbol(sym)),
    close_symbol(sym),
    sep = "\n"
  )
}

# Symbols table / mapping
close_symbol <- function(chr) {
  if (chr == "(") return(")")
  if (chr == "{") return("}")
  if (chr == "[") return("]")
  if (chr == "[[") return("]]")
}

sep_symbol <- function(chr) {
  if (chr == "(") return(", ")
  if (chr == "{") return("\n")
  if (chr == "[") return(", ")
  if (chr == "[[") return(", ")
}


# === Deparsers for control flow and function definition ======
# Continuing from above, one could further distinguish special
# cases of calls. Here we consider the control flow and
# function definition.


# Deparser for 'for' ----------------------------------------
#' Predicate for the 'for' keyword
#' @rdname predicate_component
is_call_for <- function(ast) is_call(ast, "for")

#' Deparser for the 'for' keyword
#' @rdname deparsers_component
deparse_for <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  paste(
    sym_ls[1], # for
    glue::glue("(let {sym_ls[2]} of {sym_ls[3]})"),
    sym_ls[4] # body
  )
}


# Deparser for 'if' -----------------------------------------
#' Predicate for the 'if' keyword
#' @rdname predicate_component
is_call_if <- function(ast) is_call(ast, "if")

#' Deparser for the 'if' keyword
#' @rdname deparsers_component
deparse_if <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  has_else <- function(x) length(x) == 4
  out <- paste(
    sym_ls[1], # if
    glue::glue("({sym_ls[2]})"),  # test-condition
    sym_ls[3] # then-body
  )

  if (has_else(sym_ls)) {
    paste(out, "else", sym_ls[4]) # else-body
  } else {
    out
  }
}


# Deparser for 'while' --------------------------------------
#' Predicate for the 'while' keyword
#' @rdname predicate_component
is_call_while <- function(ast) is_call(ast, "while")

#' Deparser for the 'while' keyword
#' @rdname deparsers_component
deparse_while <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  paste(
    sym_ls[1], # while
    glue::glue("({sym_ls[2]})"), # test-condition
    sym_ls[3] # while-body
  )
}


# Deparser for function definition --------------------------
#' Predicate for the "function" keyword
#' @rdname predicate_component
is_call_function <- function(ast) is_call(ast, "function")

#' Deparser for the "function" keyword
#' @rdname deparsers_component
deparse_function <- function(ast, ...) {
  deparse_arg <- function(alist0, ...) {
    alist1 <- purrr::map(alist0, deparse_js, ...)
    alist2 <- purrr::map2_chr(
      .x = names(alist1), .y = alist1,
      function(x, y) {
        glue::glue(if (y == "") "{x}" else "{x} = {y}")
      }
    )
    paste(alist2, collapse = ", ")
  }

  fun_body_str <- deparse_js(ast[[3]], ...)
  if (!is_call(ast[[3]], "{")) {
    fun_body_str <- paste0("{ ", fun_body_str, " }")
  }

  paste0(
    deparse_js(ast[[1]], ...), # function
    "(", deparse_arg(ast[[2]], ...), ") ", # function-args
    fun_body_str # function-body
  )
}

#' Deparser for the "function" keyword with explicit return
#' @rdname deparsers_component
deparse_function_with_return <- function(ast, ...) {
  deparse_arg <- function(alist0, ...) {
    alist1 <- purrr::map(alist0, deparse_js, ...)
    alist2 <- purrr::map2_chr(
      .x = names(alist1), .y = alist1,
      function(x, y) {
        glue::glue(if (y == "") "{x}" else "{x} = {y}")
      }
    )
    paste(alist2, collapse = ", ")
  }

  last <- function(x) x[[length(x)]]

  add_return <- function(ast) {
    template_ast <- parse_expr("return(x)")
    template_ast[2] <- list(ast)  # insert tree at x
    ast <- template_ast
    return(ast)
  }

  is_return <- function(ast) is_call(ast, "return")
  is_assignment <- function(ast) is_call(ast, c("=", "<-", "<<-"))
  # Handle special forms, e.g. if, for, while, as JavaScript only
  # allows returning values.
  is_keyword <- function(ast) is_call(ast, c("if", "for", "while"))
  is_exception <- function(ast) is_call(ast, c("throw", "try", "tryCatch"))
  is_valid_add <- function(ast, ...) {
    # Provide more informative message
    ast_string <- deparse_js(ast, ...)
    if (is_assignment(ast)) {
      warning("You have used an assignment statement as the final expression in:", immediate. = TRUE)
      message(yellow(ast_string))
      message("Note that automatic explicit return only applies to standalone values but not statements.")
    }
    if (is_keyword(ast)) {
      warning("You have used a control-flow statement as the final expression in:", immediate. = TRUE)
      message(yellow(ast_string))
      message("Note that automatic explicit return only applies to standalone values but not statements.")
    }
    if (is_exception(ast)) {
      warning("You have used an error/exception statement as the final expression in:", immediate. = TRUE)
      message(yellow(ast_string))
      message("Note that automatic explicit return only applies to standalone values but not statements.")
    }

    # The actual check
    !is_return(ast) && !is_assignment(ast) && !is_keyword(ast) && !is_exception(ast)
  }

  # TODO: Consider adding support to return of assignment.
  # Note that it may not play well with the subset assignment implementation
  # because that always returns the full object. While I think this is a
  # sensible default, it differs from the R behaviour.
  # warning_of_assignment <- function(ast) {
  #   fun_line <- deparse(ast)
  #   if (is_assignment(ast)) {
  #     warning(glue::glue("You have used an variable assignment as the final expression of the function: \n\t{fun_line}\nThis is currently not supported. Please end the function by putting the variable or a value `JS_NULL` on a standalone line."))
  #   }
  # }

  fun_body <- ast[[3]]
  if (is_call(fun_body, "{")) {
    last_expr <- last(fun_body)
    # Add explicit return if it is not there and the expression
    # is non-empty
    if (is_valid_add(last_expr, ...) && length(fun_body) > 1) {
      ast[[3]][[length(fun_body)]] <- add_return(last(fun_body))
    }
  } else {
    # function body is an atom
    if (is_valid_add(fun_body, ...)) {
      ast[[3]] <- add_return(fun_body)
    }
  }

  # Return the resulting string
  fun_body_str <- deparse_js(ast[[3]], ...)
  # Wrap with { . } if function uses shorthand notation
  if (!is_call(ast[[3]], "{")) {
    fun_body_str <- paste0("{ ", fun_body_str, " }")
  }
  paste0(
    deparse_js(ast[[1]], ...), # function
    "(", deparse_arg(ast[[2]], ...), ") ", # function-args
    fun_body_str # function-body
  )
}


# Deparser for return ----------------------------------
#' Predicate for return
#' @rdname predicate_component
is_call_return <- function(ast) is_call(ast, "return")

#' Deparser for return
#' @rdname deparsers_component
deparse_return <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  glue::glue("return {sym_ls[[2]]}")
}



# Deparser for assignments ----------------------------------
#' Predicate for assignments
#' @rdname predicate_component
is_call_assignment <- function(ast) is_call(ast, c("<-", "<<-"))


#' Deparser for assignments
#' @rdname deparsers_component
# Replace arrow sign by equal sign
deparse_assignment <- function(ast, ...) {
  ast[[1]] <- as.symbol("=")
  deparse_js(ast, ...)
}


#' Predicate for assignments
#' @rdname predicate_component
is_call_assignment_auto <- function(ast) {
  is_call(ast, c("<-", "=", "<<-")) &&
    # 'var' is added only when LHS is a symbol (i.e. not a call)
    rlang::is_symbol(ast[[2]])
}

#' Deparser for assignments (automatic variable declaration)
#' @rdname deparsers_component
deparse_assignment_auto <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  if (!is_call(ast, "<<-")) {
    return(glue::glue("var {sym_ls[[2]]} = {sym_ls[[3]]}"))
  }
  glue::glue("{sym_ls[[2]]} = {sym_ls[[3]]}")
}



# Deparser for the "break" keyword --------------------------
#' Predicate for the "break" keyword
#' @rdname predicate_component
is_call_break <- function(ast) is_call(ast, "break")

# "break" uses `deparse_sym` as deparser


# Deparser for the "next" keyword --------------------------
#' Predicate for the "next" keyword
#' @rdname predicate_component
is_call_next <- function(ast) is_call(ast, "next")

#' Deparser for the "next" keyword
#' @rdname deparsers_component
deparse_next <- function(ast, ...) {
  return("continue")
}


# === Deparser for Error handling =============================
# Deparser for the "try" keyword --------------------------
#' Predicate for the "try" keyword
#' @rdname predicate_component
is_call_try <- function(ast) is_call(ast, "try")

#' Deparser for the "try" keyword
#' @rdname deparsers_component
deparse_try <- function(ast, ...) {
  arg <- deparse_js(ast[[2]], ...)
  if (!is_call(ast[[2]], "{")) {
    arg <- add_curly_bracket(arg)
  }
  glue::glue("try <arg> catch(error) {\n    console.log(error)\n}",
             .open = "<", .close = ">")
}

add_curly_bracket <- function(x) {
  paste0("{\n    ", x, "\n}")
}

# Deparser for the "tryCatch" keyword --------------------------
#' Predicate for the "tryCatch" keyword
#' @rdname predicate_component
is_call_tryCatch <- function(ast) is_call(ast, "tryCatch")

#' Deparser for the "tryCatch" keyword
#' @rdname deparsers_component
deparse_tryCatch <- function(ast, ...) {
  try_arg <- deparse_js(ast[[2]], ...)
  catch_fun <- deparse_js(ast[[3]], ...) %>%
    gsub(pattern = "\n", replacement = "\n    ")  # increase indent

  if (!is_call(ast[[2]], "{")) {
    try_arg <- add_curly_bracket(try_arg)
  }
  res <- glue::glue(
    "try <try_arg> catch(error) {\n    (<catch_fun>)(error)\n}",
    .open = "<", .close = ">"
  )
  if (length(ast) < 4) {
    return(res)
  }

  fin_arg <- deparse_js(ast[[4]], ...)
  if (!is_call(ast[[4]], "{")) {
    fin_arg <- add_curly_bracket(fin_arg)
  }
  fin <- glue::glue("finally <fin_arg>",
                    .open = "<", .close = ">")
  paste(res, fin)
}


# Deparser for the "throw" keyword --------------------------
#' Predicate for the "throw" keyword
#' @rdname predicate_component
is_call_throw <- function(ast) is_call(ast, "throw")

#' Deparser for the "throw" keyword
#' @rdname deparsers_component
# Reference: https://stackoverflow.com/questions/9156176/what-is-the-difference-between-throw-new-error-and-throw-someobject
deparse_throw <- function(ast, ...) {
  err_msg <- deparse_js(ast[[2]], ...)
  glue::glue("throw new Error({err_msg})")
}



# === R data structure =======================================
# Continuing from above, one could further distinguish special
# cases of calls. Here we consider the R data structure.


#' Predicate for the "list" operator
#' @rdname predicate_component
is_call_list <- function(ast) is_call(ast, "list")

#' Deparser for the "list" operator
#' @rdname deparsers_component
deparse_list <- function(ast, ...) {
  err_msg <- "All elements in a list must be named to convert into JavaScript properly."
  paste0("{ ", deparse_list_arg(ast[-1], err_msg, ...), " }")
}

# Deparser for the arguments in a "list" call
# @keywords internal
# This function is not in `deparse_list`, because it needs to be reused
# by `deparse_df`
deparse_list_arg <- function(list0, err_msg, ...) {
  list1 <- purrr::map_chr(list0, deparse_js, ...)
  labels <- names(list1)

  if (is.null(labels)) {   # all labels are missing
    if (length(list1) != 0) {
      warning(err_msg)
    }
    paste(list1, collapse = ", ")

  } else {
    purrr::map2_chr(
      .x = labels,
      .y = list1,
      .f = function(x, y) {
        if (x == "") {     # some labels are missing
          warning(err_msg)
          glue::glue("{y}")
        } else {
          glue::glue("\"{x}\": {y}")
        }
      }
    ) %>%
      paste(collapse = ", ")
  }
}


# Dataframe related deparsers ----
#' Predicate for the "data.frame" operators
#' @rdname predicate_component
is_call_df <- function(ast) is_call(ast, "R.data_frame")

#' Deparser for the "data.frame" operators
#' @rdname deparsers_component
deparse_df <- function(ast, ...) {
  err_msg <- "All columns in a dataframe must be named to convert into JavaScript properly."
  paste0("R.data_frame({ ", deparse_df_arg(ast[-1], err_msg, ...), " })")
}

# Deparser for the arguments in a "data.frame" call
# @keywords internal
# A duplicate function is created to maintain code consistency throughout
# the package. The rule here is that each deparser must have its own
# argument deparser (if it needs one).
deparse_df_arg <- deparse_list_arg


#' Predicate for the "summarise" operators
#' @rdname predicate_component
is_call_df_summarise <- function(ast) is_call(ast, "R.summarise")

#' Deparser for the "summarise" operators
#' @rdname deparsers_component
deparse_df_summarise <- function(ast, ...) {
  args <- purrr::map_chr(ast, deparse_js, ...)
  fname <- args[[1]]
  df_arg <- args[[2]]
  fun_args <- args[-c(1:2)]
  label_args <- names(fun_args)
  if (is.null(label_args) || any(label_args == "")) {
    stop("Names must be provided for the new summary column.")
  }

  if (length(label_args) == 1) {
    glue::glue("{fname}({df_arg}, '{label_args}', {fun_args})")
  } else { # length > 1
    fun_args <- paste(fun_args, collapse = ", ")
    label_args <- paste(sQuote(label_args, "'"), collapse = ", ")
    glue::glue("{fname}({df_arg}, [{label_args}], [{fun_args}])")
  }
}


#' Predicate for the "mutate" operators
#' @rdname predicate_component
is_call_df_mutate <- function(ast) is_call(ast, "R.mutate")

#' Deparser for the "mutate" operators
#' @rdname deparsers_component
deparse_df_mutate <- deparse_df_summarise



# Deparser for the "R6Class" function --------------------------
#' Predicate for the "R6Class" function
#' @rdname predicate_component
is_call_R6Class <- function(ast) is_call(ast, "R6Class")

#' Deparser for the "R6Class" function
#' @rdname deparsers_component
deparse_R6Class <- function(ast, ...) {
  public <- if (length(ast) < 3) NULL else ast[[3]]
  private <- if (length(ast) < 4) NULL else ast[[4]]

  if (!is.null(public) && !is_call(public, "list")) {
    stop("The argument 'public' must be a list.")
  }
  if (!is.null(private) && !is_call(private, "list")) {
    stop("The argument 'private' must be a list.")
  }

  const_arg <- get_constructor_arg(public, ...)
  const_arg_wo_default <- get_constructor_arg_no_default(public, ...)
  public_list <- deparse_public_list(public, ...)
  private_list <- deparse_private_list(private, ...)

  return(glue::glue("function(<const_arg>) {
        // public variables and methods
        let self = this
        <public_list>
        // private variables and methods
        let private = {}
        <private_list>
        if (self.initialize) {
            self.initialize(<const_arg_wo_default>)
        }
    }", .open = "<", .close = ">"))
}

get_constructor_arg <- function(ast, ...) {
  deparse_arg <- function(alist0, ...) {
    alist1 <- purrr::map(alist0, deparse_js, ...)
    alist2 <- purrr::map2_chr(
      .x = names(alist1), .y = alist1,
      function(x, y) {
        glue::glue(if (y == "") "{x}" else "{x} = {y}")
      }
    )
    paste(alist2, collapse = ", ")
  }

  if (!"initialize" %in% names(ast)) {
    return("")
  }

  if (!is_call(ast$initialize, "function")) {
    stop("The constructor must be a function.")
  }

  deparse_arg(ast$initialize[[2]], ...)
}

get_constructor_arg_no_default <- function(ast, ...) {
  deparse_arg <- function(alist0, ...) {
    alist1 <- purrr::map(alist0, deparse_js, ...)
    alist2 <- names(alist1)
    paste(alist2, collapse = ", ")
  }

  if (!"initialize" %in% names(ast)) {
    return("")
  }

  if (!is_call(ast$initialize, "function")) {
    stop("The constructor must be a function.")
  }

  deparse_arg(ast$initialize[[2]], ...)
}

deparse_public_list <- function(ast, ...) {
  if (is.null(ast) || length(ast) == 1) {
    return("")
  }

  args <- ast[-1]
  public_vars <- names(args)
  if (is.null(public_vars) || any(public_vars == "")) {
    stop("All (public) variables / methods of an R6 object must be named.")
  }

  rhs <- purrr::map_chr(args, deparse_js, ...)
  purrr::map2_chr(
    public_vars, rhs, function(x, y) {
      glue::glue("self.<x> = <y>", .open = "<", .close = ">")
    }) %>%
    paste(collapse = "\n") %>%
    gsub(pattern = "\n", replacement = "\n    ")  # increase indent
}

deparse_private_list <- function(ast, ...) {
  if (is.null(ast) || length(ast) == 1) {
    return("")
  }

  args <- ast[-1]
  private_vars <- names(args)
  if (is.null(private_vars) || any(private_vars == "")) {
    stop("All (private) variables / methods of an R6 object must be named.")
  }

  # Reference: http://crockford.com/javascript/private.html
  rhs <- args %>%
    purrr::map_chr(deparse_js, ...)

  purrr::map2_chr(
    private_vars, rhs, function(x, y) {
      glue::glue("private.<x> = <y>", .open = "<", .close = ">")
    }) %>%
    paste(collapse = "\n") %>%
    gsub(pattern = "\n", replacement = "\n    ")  # increase indent
}


# === Special forms ===========================================
# Continuing from above, one could further distinguish special
# cases of calls. Here we consider the special forms.


# Deparser for "new" ------------------------------------------------------
#' Predicate for the "new" operator
#' @rdname predicate_component
is_call_new <- function(ast) {
  is_call(ast, ".") && (length(ast) == 3) && rlang::is_symbol(ast[[3]], "new")
}

#' Deparser for the "new" operator
#' @rdname deparsers_component
deparse_new <- function(ast, ...) {
  args <- deparse_js(ast[[2]], ...)
  glue::glue("new {args}")
}


# Deparser for "typeof" ------------------------------------------------------
#' Predicate for the "typeof" operator
#' @rdname predicate_component
is_call_typeof <- function(ast) {
  is_call(ast, "typeof")
}

#' Deparser for the "typeof" operator
#' @rdname deparsers_component
deparse_typeof <- function(ast, ...) {
  args <- deparse_js(ast[[2]], ...)
  glue::glue("typeof {args}")
}


# Deparser for "export" ------------------------------------------------------
#' Predicate for the "export" operator
#' @rdname predicate_component
is_call_export <- function(ast) {
  is_call(ast, "export")
}

#' Deparser for the "export" operator
#' @rdname deparsers_component
deparse_export <- function(ast, ...) {
  args <- deparse_js(ast[[2]], ...)
  glue::glue("export {args}")
}


# Deparser for "async" and "await" ------------------------------------------
#' Predicate for the "async" and "await" operators
#' @rdname predicate_component
is_call_async_await <- function(ast) {
  is_call(ast, c("async", "await"))
}

#' Deparser for the ""async" and "await" operators
#' @rdname deparsers_component
deparse_async_await <- function(ast, ...) {
  keyword <- deparse_sym(ast[[1]])
  args <- deparse_js(ast[[2]], ...)
  glue::glue("{keyword} {args}")
}


# Deparser for "let" and "const" -----------------------------------------------
#' Predicate for the "let" operator
#' @rdname predicate_component
is_call_let <- function(ast) is_call(ast, "let")

#' Deparser for the "let" operator
#' @rdname deparsers_component
deparse_let <- function(ast, ...) {
  deparse_arg <- function(list0) {
    list1 <- purrr::map_chr(list0, deparse_js, ...)
    labels <- names(list1)
    if (is.null(labels)) {
      paste(list1, collapse = ", ")
    } else {
      list2 <- purrr::map2_chr(
        .x = names(list1), .y = list1,
        function(x, y) {
          glue::glue(if (x == "") "{y}" else "{x} = {y}")
        }
      )
      paste(list2, collapse = ", ")
    }
  }

  paste(deparse_sym(ast[[1]]), deparse_arg(ast[-1]))
}


#' Predicate for the "const" operator
#' @rdname predicate_component
is_call_const <- function(ast) is_call(ast, "const")

#' Deparser for the "const" operator
#' @rdname deparsers_component
deparse_const <- deparse_let


#' Predicate for the "var" operator
#' @rdname predicate_component
is_call_var <- function(ast) is_call(ast, "var")

#' Deparser for the "var" operator
#' @rdname deparsers_component
deparse_var <- deparse_let


# Deparser for "dataURI" --------------------------------------------------
#' Predicate for the "dataURI" operator
#' @rdname predicate_component
is_call_dataURI <- function(ast) is_call(ast, "dataURI")

#' Deparser for the "dataURI" operator
#' @rdname deparsers_component
deparse_dataURI <- function(ast, ...) {
  # dataURI(asset_path, mime_type)
  fname <- ast[[2]]
  if (!is.character(fname)) {
    stop("The first argument of 'dataURI' must be a character string.
         Note that variable path is not supported.")
  }
  mime_type <- ifelse(length(ast) == 3, ast[[3]], detect_mime(fname))
  paste0('"', base64enc::dataURI(file = fname, mime = mime_type), '"')
}

detect_mime <- function(fname) {
  # reference:
  # 1. https://www.iana.org/assignments/media-types/media-types.xhtml
  # 2. https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
  # 3. https://www.freeformatter.com/mime-types-list.html
  # TODO: Add a full mime-type list?
  # A partial list is available at https://github.com/yihui/mime, but it is GPL-protected.
  switch(extname(fname),
         "svg" = "image/svg+xml",
         "bmp" = "image/bmp",
         "jpeg" = "image/jpeg",
         "jpg" = "image/jpeg",
         "tiff" = "image/tiff",
         "gif" = "image/gif",
         "png" = "image/png",
         stop(glue::glue("Please provide the mime-type for extension \"{extname(fname)}\"")) # default case
  )
}


# Deparser for "ifelse ----------------------------------------------------
#' Predicate for the "ifelse" operator
#' @rdname predicate_component
is_call_ifelse <- function(ast) is_call(ast, "ifelse")

#' Deparser for the "ifelse" operator
#' @rdname deparsers_component
deparse_ifelse <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  glue::glue("{sym_ls[2]} ? {sym_ls[3]} : {sym_ls[4]}")
}


# Deparser for "lambda" ---------------------------------------------------
#' Predicate for the "lambda" operator
#' @rdname predicate_component
is_call_lambda <- function(ast) is_call(ast, "lambda")

#' Deparser for the "lambda" operator
#' @rdname deparsers_component
deparse_lambda <- function(ast, ...) {
  sym_ls <- purrr::map_chr(ast, deparse_js, ...)
  l <- length(sym_ls)
  args <- sym_ls[-c(1, l)]
  body <- sym_ls[[l]]
  if (purrr::is_empty(args)) {
    glue::glue("function() {{ return {body}; }}")
  } else {
    if (!is.null(names(args))) {
      args <- purrr::map2_chr(
        .x = names(args), .y = args,
        .f = function(x, y) {
          if (x == '') y else glue::glue('{x} = {y}')
        }
      )
    }
    args <- paste(args, collapse = ", ")
    glue::glue("function({args}) {{ return {body}; }}")
  }
}


# Deparser for "pipe" ---------------------------------------------------
#' Predicate for the "pipe" operator
#' @rdname predicate_component
is_call_pipe <- function(ast) is_call(ast, "pipe")

#' Deparser for the "pipe" operator
#' @rdname deparsers_component
deparse_pipe <- function(ast, ...) {
  if (rlang::is_symbol(ast[[3]])) {
    arg <- deparse_js(ast[[2]], ...)
    fname <- deparse_js(ast[[3]], ...)
    glue::glue("{fname}({arg})")
  } else {
    new_ast <- ast[[3]] %>%
      as.list() %>%
      append(ast[[2]], 1) %>%
      as.call()
    deparse_js(new_ast, ...)
  }
}


# Template literal in JavaScript
# Deparser for the raw string operator "r" -----------------------------------------
#' Predicate for the raw string operator
#' @rdname predicate_component
is_call_raw_string <- function(ast) {
  is_call(ast, c("raw_str", "raw_string"))
}

#' Deparser for the raw string operator
#' @rdname deparsers_component
deparse_raw_string <- function(ast, ...) {
  arg <- ast[[2]]   # must be a character string
  # Safeguard
  if (!is.character(arg)) {
    stop("The argument of the raw string function 'r' must be a character string.")
  }
  arg  # parsing naturally removes the outer quotation marks
}


# === Deparser for formula ======================================
#' Predicate for formula
#' @rdname predicate_component
is_call_formula <- function(ast) {
  is_call(ast, "~")
}

#' Deparser for formula
#' @rdname deparsers_component
deparse_formula <- function(ast, ...) {
  fun_body <- replace_dot_variable(ast[[2]])
  fun_args <- get_all_symbols(ast[[2]]) %>%
    paste() %>% unique() %>%
    filter(begin_with_dot) %>%
    purrr::map_chr(replace_dot)

  fun_body_str <- deparse_js(fun_body, ...)
  fun_args_str <- paste(fun_args, collapse = ", ")
  glue::glue("function({fun_args_str}) {{ return {fun_body_str} }}")
}

# replace_dot_variable :: ast -> ast
replace_dot_variable <- function(ast) {
  if (is_call(ast)) {
    for (i in seq_along(ast)) {
      ast[[i]] <- replace_dot_variable(ast[[i]])
    }
    return(ast)
  }

  if (!begin_with_dot(deparse1(ast))) {
    return(ast)
  }

  as.symbol(replace_dot(deparse1(ast)))
}

# replace_dot :: character -> character
replace_dot <- function(x) {
  gsub("([.])([a-zA-Z0-9]+)", "dot_\\2", x)
}

# begin_with_dot :: character -> logical
begin_with_dot <- function(x) {
  substring(x, 1, 1) == "."
}

# Get all the symbols at the leaf nodes
get_all_symbols <- function(ast, res = list()) {
  if (is_call(ast)) {
    for (i in seq_along(ast)) {
        if (i == 1 && rlang::is_symbol(ast[[i]])) next  # See note below.
        res <- append(res, get_all_symbols(ast[[i]], res))
    }
    return(res)
  }

  if (rlang::is_symbol(ast)) {
    return(ast)
  }

  NULL
}

# `get_all_symbols` note: The first element in the AST needs to be
# skipped when it is a symbol since we want only leaf nodes. However,
# sometimes the first node itself can be another call which has its
# own leaves, and it needs to be processed as well.


# === Library functions and Exceptions ======================
# R module --------------------------------------------------
#' Predicate for the "add" operator
#' @rdname predicate_component
is_call_add <- function(ast) is_call(ast, "R.add")

#' Deparser for the "add" operator
#' @rdname deparsers_component
deparse_add <- function(ast, ...) {
  if (length(ast) == 2) {
    num <- deparse_js(ast[[2]], ...)
    glue::glue("R.unaryPlus({num})")
  } else {
    lhs <- deparse_js(ast[[2]], ...)
    rhs <- deparse_js(ast[[3]], ...)
    glue::glue("R.add({lhs}, {rhs})")
  }
}

#' Predicate for the "subtract" operator
#' @rdname predicate_component
is_call_subtract <- function(ast) is_call(ast, "R.subtract")

#' Deparser for the "subtract" operator
#' @rdname deparsers_component
deparse_subtract <- function(ast, ...) {
  if (length(ast) == 2) {
    num <- deparse_js(ast[[2]], ...)
    if (is.numeric(ast[[2]])) {
      glue::glue("-{num}")
    } else {
      glue::glue("R.unaryMinus({num})")
    }
  } else {
    lhs <- deparse_js(ast[[2]], ...)
    rhs <- deparse_js(ast[[3]], ...)
    glue::glue("R.subtract({lhs}, {rhs})")
  }
}

#' Predicate for the "extract" operator
#' @rdname predicate_component
is_call_extract <- function(ast) is_call(ast, "R.extract")

#' Deparser for the "extract" operator
#' @rdname deparsers_component
deparse_extract <- function(ast, ...) {
  obj <- deparse_js(ast[[2]], ...)
  ind <- purrr::map_chr(ast[-c(1,2)], deparse_js, ...)
  ind <- replace_empty_index(ind, obj)
  if (length(ind) > 1) {
    ind <- paste(ind, collapse = ", ")
  }
  glue::glue("R.extract({obj}, {ind})")
}

replace_empty_index <- function(x, obj) {
  ind <- which(x == "")
  if (!purrr::is_empty(ind)) {
    x[ind] <- purrr::map_chr(ind, function(i) {
      glue::glue("R.emptyIndex({obj}, {i - 1})")  # -1 for JavaScript
    })
  }
  x
}


#' Predicate for the "extractAssign" operator
#' @rdname predicate_component
is_call_extractAssign <- function(ast) {
  is_call(ast, c("<-", "=")) && is_call(ast[[2]], "R.extract")
}

#' Deparser for the "extractAssign" operator
#' @rdname deparsers_component
deparse_extractAssign <- function(ast, ...) {
  obj <- deparse_js(ast[[2]][[2]], ...)
  val <- deparse_js(ast[[3]], ...)
  # browser()
  ind <- purrr::map_chr(ast[[2]][-c(1,2)], deparse_js, ...)
  ind <- replace_empty_index(ind, obj)
  if (length(ind) > 1) {
    ind <- paste(ind, collapse = ", ")
  }
  glue::glue("{obj} = R.extractAssign({obj}, {val}, {ind})")
}


#' Predicate for the "extract2" operator
#' @rdname predicate_component
is_call_extract2 <- function(ast) is_call(ast, "R.extract2")

#' Deparser for the "extract2" operator
#' @rdname deparsers_component
deparse_extract2 <- function(ast, ...) {
  if (length(ast) > 3) {
    # extract2 only takes one index argument (can be scalar or vector)
    stop("Incorrect number of subscripts")
  }
  obj <- deparse_js(ast[[2]], ...)
  ind <- deparse_js(ast[[3]], ...)
  if (ind == "") return("null")

  glue::glue("R.extract2({obj}, {ind})")
}


#' Predicate for the "extract2Assign" operator
#' @rdname predicate_component
is_call_extract2Assign <- function(ast) {
  is_call(ast, c("<-", "=")) && is_call(ast[[2]], "R.extract2")
}

#' Deparser for the "extract2Assign" operator
#' @rdname deparsers_component
deparse_extract2Assign <- function(ast, ...) {
  if (length(ast[[2]]) > 3) {
    # extract2 only takes one index argument (can be scalar or vector)
    stop("Currently, `[[<-` only supports one argument.")
  }
  ind <- deparse_js(ast[[2]][[3]], ...)
  if (ind == "") stop("[[ ]] with missing subscript")

  obj <- deparse_js(ast[[2]][[2]], ...)
  val <- deparse_js(ast[[3]], ...)
  glue::glue("{obj} = R.extract2Assign({obj}, {val}, {ind})")
}


# DOM module --------------------------------------------------------
#' Predicate for the HTML tags
#' @rdname predicate_component
is_html_tags <- function(ast) {
  tags <- c("div", "span", "textarea",
    "h1", "h2", "h3", "h4", "h5", "h6",
    "em", "strong", "ul", "li", "blockquote", "hr",
    "img", "script", "audio", "video", "canvas", "input", "link",
    "section", "article", "header", "nav", "footer", "iframe",
    # "table", "tbody", "thead", "td", "tr", "th",
    "form", "option", "menu", "code", "pre", "style", "button")
  # tags <- c("a", "abbr", "address", "area", "article", "aside",
  #           "audio", "b", "base", "bdi", "bdo", "blockquote", "body",
  #           "br", "button", "canvas", "caption", "cite", "code", "col",
  #           "colgroup", "data", "datalist", "dd", "del", "details",
  #           "dfn", "dialog", "div", "dl", "dt", "em", "embed",
  #           "fieldset", "figure", "footer", "form", "h1", "h2", "h3",
  #           "h4", "h5", "h6", "head", "header", "hgroup", "hr", "html",
  #           "i", "iframe", "img", "input", "ins", "kbd", "keygen",
  #           "label", "legend", "li", "link", "main", "map", "mark",
  #           "menu", "menuitem", "meta", "meter", "nav", "noscript",
  #           "object", "ol", "optgroup", "option", "output", "p",
  #           "param", "pre", "progress", "q", "rb", "rp", "rt", "rtc",
  #           "ruby", "s", "samp", "script", "section", "select", "small",
  #           "source", "span", "strong", "style", "sub", "summary",
  #           "sup", "table", "tbody", "td", "template", "textarea",
  #           "tfoot", "th", "thead", "time", "title", "tr", "track",
  #           "u", "ul", "var", "video", "wbr")
  is_call(ast, tags)
}

#' Deparser for the HTML tags
#' @rdname deparsers_component
deparse_html_tags <- function(ast, ...) {
  tag <- deparse_sym(ast[[1]])
  # Empty argument
  if (length(ast) == 1) {
    return(glue::glue("dom(\"{tag}\")"))
  }

  # No named element
  args <- ast[-1]
  if (is.null(names(args))) {
    args_str <- args %>%
      purrr::map_chr(deparse_js, ...) %>%
      paste0(collapse = ", ")
    return(glue::glue("dom(\"{tag}\", {{}}, {args_str})"))
  }

  # Has named elements
  named <- names(args) != ""
  named_ast <- ast
  named_ast[[1]] <- as.symbol("list")
  named_ast[1 + which(!named)] <- NULL
  named_args <- deparse_js(named_ast, ...)
  if (all(named)) {
    return(glue::glue("dom(\"{tag}\", {named_args})"))
  }

  unnamed_args <- args[!named] %>%
    purrr::map_chr(deparse_js, ...) %>%
    paste0(collapse = ", ")
  return(glue::glue("dom(\"{tag}\", {named_args}, {unnamed_args})"))
}


# d3 module --------------------------------------------------
#' Predicate for the d3.js `attr` function
#' @rdname predicate_component
is_d3_attr <- function(ast) {
  (length(ast) >= 2) &&
    is_call(ast[[1]], ".") &&
    (length(ast[[1]]) >= 3) &&
    rlang::is_symbol(ast[[1]][[3]], "d3_attr")
}

#' Predicate for the d3.js `style` function
#' @rdname predicate_component
is_d3_style <- function(ast) {
  (length(ast) >= 2) &&
    is_call(ast[[1]], ".") &&
    (length(ast[[1]]) >= 3) &&
    rlang::is_symbol(ast[[1]][[3]], "d3_style")
}

#' Deparser for the d3.js `attr` function
#' @rdname deparsers_component
deparse_d3_attr <- function(ast, ...) {
  lhs <- ast[[1]][[2]]
  lhs_chr <- deparse_js(lhs, ...)

  fname <- ast[[1]][[3]] %>%
    deparse_js(...) %>%
    strsplit("_") %>%
    unlist() %>%
    tail(1)

  rhs <- ast[-1]
  args <- rhs
  arg_names <- names(args)
  arg_chr <- purrr::map_chr(args, deparse_js, ...)
  if (is.null(arg_names) || any(arg_names == "")) {
    stop("All attributes / styles of a SVG element must be named.")
  }
  rhs_chr <- list(arg_names, arg_chr) %>%
    purrr::pmap_chr(~glue::glue("  .{fname}(\"{..1}\", {..2})")) %>%
    paste(collapse = "\n")

  glue::glue("{lhs_chr}\n{rhs_chr}")
}

#' Deparser for the d3.js `style` function
#' @rdname deparsers_component
deparse_d3_style <- deparse_d3_attr
