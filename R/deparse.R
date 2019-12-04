#' Parse R Expressions
#' @param x A character string. The text to parse.
parse0 <- function(x) parse(text = x)[[1]]

#' Deparse R Expressions
#' @param expr any R expression.
deparse_r <- function(expr) deparse(expr, width.cutoff = 500L)

#' Expression Deparsing for JS
#' @param ast A language object.
deparse_js <- function(ast) {
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
    deparse_r(ast)
  }
}

is_infix <- function(ast) {
  infix_ops <- c("=", "+", "-", "*", "/",
                 #"%%", "^", "$", "@",                # R specific
                 "%", "**", ".", "instanceof", "=>",  # Javascript specific
                 "==", "!=", "<", ">", "<=", ">=", "!",
                 "&&", "||", "&", "|", ":")
  is_custom_infix <- function(x) {
    grepl(pattern = "^%[^%]+%$", x = x)
  }

  sym <- deparse_r(ast[[1]])
  isTRUE(sym %in% infix_ops) || is_custom_infix(sym)
}

is_wrap <- function(ast) {
  wrap_ops <- c("{", "(", "[", "[[")

  sym <- deparse_r(ast[[1]])
  isTRUE(sym %in% wrap_ops)
}


# Deparse functions
deparse_infix <- function(ast) {
  sym <- deparse_js(ast[[1]])

  # Not needed anymore since `-` maps to `math.subtract`, this is
  # fixed in the prefix case.
  # # Special case 1: handle negative sign as unary operator
  # if (length(ast) == 2) {
  #   return(paste0(
  #     sym,
  #     deparse_js(ast[[2]])
  #   ))
  # }

  lhs <- deparse_js(ast[[2]])
  rhs <- deparse_js(ast[[3]])

  # Special case 2: handle `new` operator
  if (rlang::is_call(ast, ".") && (rhs == "new")) {
    return(glue::glue("new {lhs}"))
  }

  paste(lhs, sym, rhs, sep = space_symbol(sym))
}


deparse_wrap <- function(ast) {
  sym <- deparse_js(ast[[1]])
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
  sym_ls <- purrr::map_chr(ast, deparse_js)
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
  sym_ls <- purrr::map_chr(ast, deparse_js)
  sym <- sym_ls[1]
  paste0(
    sym,
    paste0(sym_ls[-1], collapse = sep_symbol(sym)),
    close_symbol(sym)
  )
}

deparse_wrap_cb <- function(ast) {
  # Case '{'
  sym_ls <- purrr::map_chr(ast, deparse_js)
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
  sym <- deparse_js(ast[[1]])
  if (length(sym) > 1) return(deparse_default(ast)) # guard

  switch(sym,
         "for" = deparse_for(ast),
         "if" = deparse_if(ast),
         "function" = deparse_function(ast),
         "while" = deparse_while(ast),
         "list" = deparse_list(ast),
         "data.frame" = deparse_df(ast),
         # Special forms
         "let" = deparse_let(ast),
         "dataURI" = deparse_dataURI(ast),
         "ifelse" = deparse_ifelse(ast),
         "lambda" = deparse_lambda(ast),
         "math.subtract" = deparse_math_subtract(ast),
         "math.subset" = deparse_math_subset(ast),
         deparse_default(ast)
  )
}

deparse_default <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
  paste0(
    sym_ls[1],
    "(",
    paste0(sym_ls[-1], collapse = ", "),
    ")"
  )
}

deparse_for <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
  paste(
    sym_ls[1],
    glue::glue("(let {sym_ls[2]} of {sym_ls[3]})"),
    sym_ls[4]
  )
}

deparse_if <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
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
    alist1 <- purrr::map(alist0, deparse_r)
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
    deparse_js(ast[[1]]),
    "(", deparse_arg(ast[[2]]), ") ",
    deparse_js(ast[[3]])
  )
}

deparse_while <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
  paste(
    sym_ls[1],
    glue::glue("({sym_ls[2]})"),
    sym_ls[3]
  )
}

deparse_list <- function(ast) {
  deparse_arg <- function(list0) {
    list1 <- purrr::map(list0, deparse_js)

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
    list1 <- purrr::map(list0, deparse_r)

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
    list1 <- purrr::map(list0, deparse_js)
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
  fname <- ast[[2]]
  mime_type <- detect_mime(fname)
  paste0('"', base64enc::dataURI(file = fname, mime = mime_type), '"')
}

detect_mime <- function(fname) {
  # reference: https://www.freeformatter.com/mime-types-list.html
  switch(extname(fname),
         "svg"  = "image/svg+xml",
         "bmp"  = "image/bmp",
         "jpeg" = "image/jpeg",
         "jpg"  = "image/jpeg",
         "tiff" = "image/tiff",
         "gif"  = "image/gif",
         "png"  = "image/png",
         "")   # default case
}

deparse_ifelse <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
  glue::glue("{sym_ls[2]} ? {sym_ls[3]} : {sym_ls[4]}")
}

deparse_lambda <- function(ast) {
  sym_ls <- purrr::map_chr(ast, deparse_js)
  l <- length(sym_ls)
  args <- sym_ls[-c(1, l)]
  body <- sym_ls[[l]]
  if (purrr::is_empty(args)) {
    glue::glue("function() {{ return {body}; }}")
  } else {
    args <- paste(args, collapse = ", ")
    glue::glue("function({args}) {{ return {body}; }}")
  }
}

deparse_math_subtract <- function(ast) {
  if (length(ast) == 2) {
    num <- deparse_js(ast[[2]])
    glue::glue("-{num}")
  } else {
    deparse_default(ast)
  }
}

deparse_math_subset <- function(ast) {
  obj <- deparse_js(ast[[2]])
  ind <- deparse_js(ast[[3]])
  glue::glue("math.subset({obj}, math.index({ind}))")
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
