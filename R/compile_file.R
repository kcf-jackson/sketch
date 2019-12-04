#' Compile an R file into a JS file
#' @param input A character string; the input file.
#' @param output A character string; the output file. When the
#' output is "", the result is printed to the standard output.
#' @export
compile_r <- function(input, output = "") {
  parse(file = input) %>%
    purrr::map(rewrite) %>%
    purrr::map(deparse_js) %>%
    unlist() %>%
    write(file = output)
  output
}


# Interface for AST rewriting
rewrite <- function(ast) {
  ast %>%
    rewrite_by_subst() %>%
    rewrite_by_cond_subst()  # order of the function calls matters
}


# Rewriting AST by simple substitution
rewrite_by_subst <- function(ast) {
  make_rule <- function(from, to) {
    function(x) subst(x, pattern = from, replacement = to)
  }

  subst_rules <- list(
    # Binary operators
    make_rule("<-", "="),
    make_rule("<<-", "="),
    make_rule("+", "math.add"),
    make_rule("-", "math.subtract"),
    make_rule("*", "math.multiply"),
    make_rule("/", "math.divide"),
    make_rule("[", "math.subset"),
    make_rule("^", "math.pow"),
    make_rule("%%", "math.mod"),
    make_rule("$", "."),
    make_rule(":", "R.seq_by"),
    make_rule("%instanceof%", "instanceof"),
    make_rule("%=>%", "=>"),
    make_rule("%+%", "+")
  )

  magrittr::freduce(ast, subst_rules)
}

subst <- function(ast, pattern, replacement) {
    if (rlang::is_call(ast)) {
      return(as.call(purrr::map(ast, ~subst(.x, pattern, replacement))))
    }

    if (rlang::is_symbol(ast)) {
      if (rlang::is_symbol(ast, pattern)) {
        return(as.symbol(replacement))
      } else {
        return(ast)
      }
    }

    if (rlang::is_syntactic_literal(ast)) {
      return(ast)
    }

    # Handle the pairlist special case
    if (rlang::is_pairlist(ast)) {
      ast2 <- as.pairlist(purrr::map(ast, ~subst(.x, pattern, replacement)))
      names(ast2) <- names(ast)
      return(ast2)
    }

    # Handle reference to source code
    if (class(ast) == "srcref") {
      return(ast)
    }

    stop("The line should not be reached, please submit an issue with the input on github. Thanks!")
}


# Conditional substitute
rewrite_by_cond_subst <- function(ast) {
  make_rule <- function(from, to) {
    function(x) cond_subst(x, pattern = from, replacement = to)
  }

  cond_subst_rules <- list(
    # Base Javascript
    make_rule("TRUE", "true"),
    make_rule("FALSE", "false"),
    make_rule("declare", "let"),
    make_rule("self", "this"),
    # R-like functions
    make_rule(   "seq", "R.seq_by"),
    make_rule(     "c", "R.c"),
    make_rule("matrix", "R.matrix"),
    make_rule( "print", "R.print"),
    make_rule("length", "R.length"),
    make_rule(   "map", "R.map"),
    make_rule("reduce", "R.reduce"),

    make_rule( "runif", "R.runif"),
    make_rule(    "pi", "math.pi"),
    # base::groupGeneric
    # Group "Math" ----
    make_rule(    "abs", "math.abs"),
    make_rule(   "sign", "math.sign"),
    make_rule(   "sqrt", "math.sqrt"),
    make_rule(  "floor", "math.floor"),
    make_rule("ceiling", "math.ceil"),
    make_rule(  "trunc", "math.fix"),
    make_rule(  "round", "math.round"),
    make_rule( "signif", "R.signif"),

    make_rule( "exp", "math.exp"),
    make_rule( "log", "math.log"),
    make_rule( "expm1", "math.expm1"),
    make_rule( "log1p", "math.log1p"),
    make_rule( "cos", "math.cos"),
    make_rule( "sin", "math.sin"),
    make_rule( "tan", "math.tan"),
    make_rule( "cos", "R.cospi"),
    make_rule( "sin", "R.sinpi"),
    make_rule( "tan", "R.tanpi"),
    make_rule("acos", "math.acos"),
    make_rule("asin", "math.asin"),
    make_rule("atan", "math.atan"),

    make_rule( "cosh", "math.cosh"),
    make_rule( "sinh", "math.sinh"),
    make_rule( "tanh", "math.tanh"),
    make_rule("acosh", "math.acosh"),
    make_rule("asinh", "math.asinh"),
    make_rule("atanh", "math.atanh"),

    make_rule("lgamma", "R.lgamma"),
    make_rule("gamma", "math.gamma"),
    # Missing digamma, trigamma
    # Missing cumsum, cumprod, cummax, cummin

    # Group "Summary" ----
    make_rule(  "all", "R.all"),
    make_rule(  "any", "R.any"),
    make_rule(  "sum", "math.sum"),
    make_rule( "prod", "math.prod"),
    make_rule(  "min", "math.min"),
    make_rule(  "max", "math.max"),
    make_rule("range", "R.range"),

    # Extra ----
    make_rule( "log10", "math.log10"),
    make_rule( "log2", "math.log2"),
    # Missing beta, lbeta, psigamma
    make_rule("choose", "math.combinations"),
    make_rule("lchoose", "R.lchoose"),
    make_rule("factorial", "math.factorial"),
    make_rule("lfactorial", "R.lfactorial"),

    # JavaScript ----
    # make_rule("NULL", "null"),   # doesn't work since R doesn't distinguish input NULL and empty NULL.
    make_rule("JS_NULL", "null"),
    make_rule("JS_UNDEFINED", "undefined"),
    make_rule("JS_NA", "NaN"),

    # jQuery ----
    make_rule("jQuery", "$")
  )

  magrittr::freduce(ast, cond_subst_rules)
}

cond_subst <- function(ast, pattern, replacement) {
  if (rlang::is_call(ast)) {
    if (rlang::is_call(ast, '.')) {
      # '.' needs special handling because almost anything can go after '.' in JavaScript (including keywords!)
      # It is helpful to assume that things appeared after '.' should be treated as if they are quoted strings,
      # and hence, no rewriting should be applied to them.
      ast[[2]] <- cond_subst(ast[[2]], pattern, replacement)
      return(ast)
    } else {
      return(as.call(purrr::map(ast, ~cond_subst(.x, pattern, replacement))))
    }
  }

  if (rlang::is_symbol(ast)) {
    if (rlang::is_symbol(ast, pattern)) {
      return(as.symbol(replacement))
    } else {
      return(ast)
    }
  }

  if (rlang::is_syntactic_literal(ast)) {
    if (is.null(ast))   return(ast)
    if (is.na(ast))     return(ast)
    if (ast == pattern) return(as.symbol(replacement))
    return(ast)
  }

  # Handle the pairlist special case
  if (rlang::is_pairlist(ast)) {
    ast2 <- as.pairlist(purrr::map(ast, ~cond_subst(.x, pattern, replacement)))
    names(ast2) <- names(ast)
    return(ast2)
  }

  # Handle reference to source code
  if (class(ast) == "srcref") {
    return(ast)
  }

  stop("The line should not be reached, please submit an issue with the input on github. Thanks!")
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
