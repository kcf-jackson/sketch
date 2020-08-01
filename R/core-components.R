# Deparsers for symbols and calls ===========================
# To construct a "master"-deparser, at the most basic level
# one must provide a deparser for each of symbols and calls.


# Deparser for symbols --------------------------------------
#' Predicate for symbols, i.e. symbols or syntactic literals
# For completeness, we use the negation of `is_call` to include
# also the pairlist case.
# Reference: https://adv-r.hadley.nz/expressions.html#summary
#' @rdname predicate_component
#' @param ast A language object.
is_sym <- function(ast) !is_call(ast)

#' Deparsers (specialised)
#' @rdname deparsers_component
#' @param ast A language object.
#' @param ... The contextual information to be passed on to
#' the next call.
#' @return A character string.
deparse_sym <- function(ast, ...) {
  deparse(ast, width.cutoff = 500L)
}

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


# Deparsers for infix and wrap operators =====================
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


# Deparsers for control flow and function definition =========
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
#' Predicate for the 'function' keyword
#' @rdname predicate_component
is_call_function <- function(ast) is_call(ast, "function")

#' Deparser for the 'function' keyword
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

  paste0(
    deparse_js(ast[[1]], ...), # function
    "(", deparse_arg(ast[[2]], ...), ") ", # function-args
    deparse_js(ast[[3]], ...) # function-body
  )
}


# R data structure ===========================================
# Continuing from above, one could further distinguish special
# cases of calls. Here we consider the R data structure.


#' Predicate for the "list" operators
#' @rdname predicate_component
is_call_list <- function(ast) is_call(ast, "list")

#' Deparser for the "list" operators
#' @rdname deparsers_component
deparse_list <- function(ast, ...) {
  err_msg <- "All elements in a list must be named to convert into JavaScript properly."
  paste0("{ ", deparse_list_arg(ast[-1], err_msg, ...), " }")
}

#' Deparser for the arguments in a "list" call
#' @keywords internal
# This function is not in `deparse_list`, because it needs to be reused
# by `deparse_df`
deparse_list_arg <- function(list0, err_msg, ...) {
  list1 <- purrr::map_chr(list0, deparse_js, ...)
  labels <- names(list1)

  if (is.null(labels)) {   # all labels are missing
    warning(err_msg)
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
          glue::glue("{x}: {y}")
        }
      }
    ) %>%
      paste(collapse = ", ")
  }
}


#' Predicate for the "data.frame" operators
#' @rdname predicate_component
is_call_df <- function(ast) is_call(ast, "data.frame")

#' Deparser for the "data.frame" operators
#' @rdname deparsers_component
deparse_df <- function(ast, ...) {
  err_msg <- "All columns in a dataframe must be named to convert into JavaScript properly."
  paste0("R.data_frame({ ", deparse_df_arg(ast[-1], err_msg, ...), " })")
}

#' Deparser for the arguments in a "data.frame" call
#' @keywords internal
# A duplicate function is created to maintain code consistency throughout
# the package. The rule here is that each deparser must have its own
# argument deparser (if it needs one).
deparse_df_arg <- deparse_list_arg


# Special forms ==========================================================
# Continuing from above, one could further distinguish special cases of
# calls. Here we consider the special forms.


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


# Deparser for "let" ------------------------------------------------------
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

  paste("let", deparse_arg(ast[-1]))
}


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


# Library functions and Exceptions -------------------------------------------------
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
  glue::glue("R.extract({obj}, [{ind}])")
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
  glue::glue("R.extractAssign({obj}, {val}, [{ind}])")
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
    stop("Incorrect number of subscripts")
  }
  ind <- deparse_js(ast[[2]][[3]], ...)
  if (ind == "") stop("[[ ]] with missing subscript")

  obj <- deparse_js(ast[[2]][[2]], ...)
  val <- deparse_js(ast[[3]], ...)
  glue::glue("R.extract2Assign({obj}, {val}, {ind})")
}
