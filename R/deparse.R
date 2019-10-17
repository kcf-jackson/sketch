#' Parse R Expressions
#' @param x A character string. The text to parse.
parse0 <- function(x) parse(text = x)[[1]]


#' Expression Deparsing for JS
#' @param ast A language object.
deparse0 <- function(ast) {
  if (is.call(ast)) {

    if (is_infix(ast)) {
      deparse_infix(ast)
    }
    else if (is_wrap(ast)) {
      deparse_wrap(ast)
    }
    else {
      deparse_prefix(ast)
    }

  } else {
    deparse(ast)
  }
}

is_infix <- function(ast) {
  infix_ops <- c("=", "+", "-", "*", "/",
                 #"%%", "^", "$", "@",                # R specific
                 "%", "**", ".", "instanceof", "=>",  # Javascript specific
                 "==", "!=", "<", ">", "<=", ">=", "!",
                 "&&", "||", "&", "|", ":")
  is_custom_infix <- function(x) {
    grepl(pattern = "%[^%]+%", x = x)
  }

  sym <- deparse(ast[[1]])
  (sym %in% infix_ops) || is_custom_infix(sym)
}

is_wrap <- function(ast) {
  wrap_ops <- c("{", "(", "[", "[[")

  sym <- deparse(ast[[1]])
  (sym %in% wrap_ops)
}


# Deparse functions
deparse_infix <- function(ast) {
  sym <- as.character(ast[[1]])

  # handle negative sign as unary operator
  if (length(ast) == 2) {
    return(paste0(
      sym,
      deparse0(ast[[2]])
    ))
  }

  paste(
    deparse0(ast[[2]]),
    sym,
    deparse0(ast[[3]]),
    sep = space_symbol(sym)
  )
}


deparse_wrap <- function(ast) {
  sym <- as.character(ast[[1]])
  switch(sym,
         "["  = deparse_wrap_sb(ast),
         "[[" = deparse_wrap_sb(ast),
         "("  = deparse_wrap_rb(ast),
         "{"  = deparse_wrap_cb(ast),
         stop(glue::glue("Haven't implemented deparse for symbol '{sym}'"))
  )
}

deparse_wrap_sb <- function(ast) {
  # case "[" and "[["
  sym_ls <- purrr::map_chr(ast, deparse0)
  sym <- sym_ls[1]
  paste0(
    sym_ls[2],
    sym,
    paste0(sym_ls[-(1:2)], collapse = sep_symbol(sym)),
    close_symbol(sym)
  )
}

deparse_wrap_rb <- function(ast) {
  # Case '('
  sym_ls <- purrr::map_chr(ast, deparse0)
  sym <- sym_ls[1]
  paste0(
    sym,
    paste0(sym_ls[-1], collapse = sep_symbol(sym)),
    close_symbol(sym)
  )
}

deparse_wrap_cb <- function(ast) {
  # Case '{'
  sym_ls <- purrr::map_chr(ast, deparse0)
  sym <- sym_ls[1]
  increase_indent <- function(x) {
    indent <- "   "
    indent_nl <- paste0("\n", indent, " ")
    paste(
      indent,
      gsub(x = x, pattern = "\n", replacement = indent_nl)
    )   # `gsub` is fine here since '\n' in quoted string becomes '\\n'
  }

  paste(
    sym,
    paste0(increase_indent(sym_ls[-1]), collapse = sep_symbol(sym)),
    close_symbol(sym),
    sep = "\n"
  )
}


deparse_prefix <- function(ast) {
  sym <- deparse(ast[[1]])
  switch(sym,
         "for" = deparse_for(ast),
         "if" = deparse_if(ast),
         "function" = deparse_function(ast),
         "while" = deparse_while(ast),
         "list" = deparse_list(ast),
         "data.frame" = deparse_df(ast),
         "let" = deparse_let(ast),
         "dataURI" = deparse_dataURI(ast),
         deparse_default(ast)
  )
}

deparse_default <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse0)
  paste0(
    sym_ls[1],
    "(",
    paste0(sym_ls[-1], collapse = ", "),
    ")"
  )
}

deparse_for <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse0)
  paste(
    sym_ls[1],
    glue::glue("(let {sym_ls[2]} of {sym_ls[3]})"),
    sym_ls[4]
  )
}

deparse_if <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse0)
  has_else <- function(x) length(x) == 4
  out <- paste(
    sym_ls[1],
    glue::glue("({sym_ls[2]})"),
    sym_ls[3]
  )

  if (has_else(sym_ls)) {
    paste(out, "else", sym_ls[4])
  } else {
    out
  }
}

deparse_function <- function(ast) {
  deparse_arg <- function(alist0) {
    alist1 <- purrr::map(alist0, deparse)
    alist2 <- purrr::map2_chr(
      .x = names(alist1),
      .y = alist1,
      .f = function(x, y) {
        if (y == "") {
          glue::glue("{x}")
        } else {
          glue::glue("{x} = {y}")
        }
      }
    )
    paste(alist2, collapse = ", ")
  }

  paste0(
    deparse0(ast[[1]]),
    "(", deparse_arg(ast[[2]]), ") ",
    deparse0(ast[[3]])
  )
}

deparse_while <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse0)
  paste(
    sym_ls[1],
    glue::glue("({sym_ls[2]})"),
    sym_ls[3]
  )
}

deparse_list <- function(ast) {
  deparse_arg <- function(list0) {
    list1 <- purrr::map(list0, deparse0)

    labels <- names(list1)
    if (is.null(labels)) {
      list2 <- purrr::map_chr(list1, function(y) { glue::glue("{y}") })
      warning("All elements in a list must be named to convert into JavaScript properly.")
    } else {
      list2 <- purrr::map2_chr(
        .x = labels,
        .y = list1,
        .f = function(x, y) {
          if (x == "") {
            warning("All elements in a list must be named to convert into JavaScript properly.")
            glue::glue("{y}")
          } else {
            glue::glue("{x}: {y}")
          }
        }
      )
    }
    paste(list2, collapse = ", ")
  }

  paste0("{ ", deparse_arg(ast[-1]), " }")
}

deparse_df <- function(ast) {
  deparse_arg <- function(list0) {
    list1 <- purrr::map(list0, deparse)

    labels <- names(list1)
    if (is.null(labels)) {
      list2 <- purrr::map_chr(list1, function(y) { glue::glue("{y}") })
      warning("All elements in a list must be named to convert into JavaScript properly.")
    } else {
      list2 <- purrr::map2_chr(
        .x = names(list1),
        .y = list1,
        .f = function(x, y) {
          if (x == "") {
            warning("All columns in a dataframe must be named to convert into JavaScript properly.")
            glue::glue("{y}")
          } else {
            glue::glue("{x}: {y}")
          }
        })
    }
    paste(list2, collapse = ", ")
  }

  paste0("new dfjs.DataFrame({ ", deparse_arg(ast[-1]), " })")  # should dfjs be hard coded? interface needed?
}

deparse_let <- function(ast) {
  deparse_arg <- function(list0) {
    list1 <- purrr::map(list0, deparse0)
    labels <- names(list1)
    if (is.null(labels)) {
      list2 <- purrr::map_chr(list1, function(y) { glue::glue("{y}") })
    } else {
      list2 <- purrr::map2_chr(
        .x = names(list1),
        .y = list1,
        .f = function(x, y) {
          if (x == "") {
            glue::glue("{y}")
          } else {
            glue::glue("{x} = {y}")
          }
        }
      )
    }
    paste(list2, collapse = ", ")
  }

  paste("let", deparse_arg(ast[-1]))  # should dfjs be hard coded? interface needed?
}

deparse_dataURI <- function(ast) {
  if (!is.character(ast[[2]])) {
    stop("The argument inside the \"dataURI\" call is not a character string.
         Note that our \"compiler\" only does static code analysis.")
  }
  paste0('"', base64enc::dataURI(file = ast[[2]]), '"')
}


# Symbols table / mapping
space_symbol <- function(chr) {
  space <- c("=", "+", "-", "*",
             #"%%", "^",                      # R specific
             "%", "**", "instanceof", "=>",   # Javascript specific
             "==", "!=", "<", ">", "<=", ">=",
             "&&", "||", "&", "|")
  no_space <- c(
    #"$", "@",   # R specific
    ".",         # Javascript specific
    ":", "/", "!"
  )

  if (chr %in% space) return(" ")
  if (chr %in% no_space) return("")
  return(" ")  # custom infix operator
}

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
