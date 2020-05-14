#' Mapping R operators into JavaScript operators
#'
#' @name r-to-js-rules
#'
#' @references
#' R operators: https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Operators
#' R infix and prefix operators: https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Infix-and-prefix-operators
#' JavaScript operators: https://www.w3schools.com/js/js_operators.asp
#'
#' @export
basic_rules <- function() {
    list(
        make_rule("<-", "="),
        make_rule("<<-", "="),
        make_rule("^", "**"),
        make_rule("%%", "%"),
        make_rule("$", "."),
        make_rule("%instanceof%", "instanceof"),
        make_rule("%=>%", "=>"),
        make_rule("T", "true"),
        make_rule("F", "false"),
        make_rule("TRUE", "true"),
        make_rule("FALSE", "false"),
        make_rule("declare", "let"),
        make_rule("self", "this"),
    )
}


#' @rdname r-to-js-rules
#' @export
default_rules <- function() {
    list(
        # Binary operators
        make_rule("<-", "="),
        make_rule("<<-", "="),
        make_rule("+", "R.add"),
        make_rule("-", "R.subtract"),
        make_rule("*", "R.multiply"),
        make_rule("/", "R.divide"),
        make_rule("[", "R.subset"),
        make_rule("^", "R.pow"),
        make_rule("%%", "R.mod"),
        make_rule("$", "."),
        make_rule(":", "R.seq_by"),
        make_rule("%instanceof%", "instanceof"),
        make_rule("%=>%", "=>"),
        make_rule("%+%", "+"),
        make_rule("%-%", "-"),

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
        make_rule(    "pi", "R.pi"),
        # base::groupGeneric
        # Group "math" ----
        make_rule(    "abs", "R.abs"),
        make_rule(   "sign", "R.sign"),
        make_rule(   "sqrt", "R.sqrt"),
        make_rule(  "floor", "R.floor"),
        make_rule("ceiling", "R.ceil"),
        make_rule(  "trunc", "R.fix"),
        make_rule(  "round", "R.round"),
        make_rule( "signif", "R.signif"),

        make_rule( "exp", "R.exp"),
        make_rule( "log", "R.log"),
        make_rule( "expm1", "R.expm1"),
        make_rule( "log1p", "R.log1p"),
        make_rule( "cos", "R.cos"),
        make_rule( "sin", "R.sin"),
        make_rule( "tan", "R.tan"),
        make_rule( "cospi", "R.cospi"),
        make_rule( "sinpi", "R.sinpi"),
        make_rule( "tanpi", "R.tanpi"),
        make_rule("acos", "R.acos"),
        make_rule("asin", "R.asin"),
        make_rule("atan", "R.atan"),

        make_rule( "cosh", "R.cosh"),
        make_rule( "sinh", "R.sinh"),
        make_rule( "tanh", "R.tanh"),
        make_rule("acosh", "R.acosh"),
        make_rule("asinh", "R.asinh"),
        make_rule("atanh", "R.atanh"),

        make_rule("lgamma", "R.lgamma"),
        make_rule("gamma", "R.gamma"),
        # Missing digamma, trigamma
        # Missing cumsum, cumprod, cummax, cummin

        # Group "Summary" ----
        make_rule(  "all", "R.all"),
        make_rule(  "any", "R.any"),
        make_rule(  "sum", "R.sum"),
        make_rule( "prod", "R.prod"),
        make_rule(  "min", "R.min"),
        make_rule(  "max", "R.max"),
        make_rule("range", "R.range"),

        # Extra ----
        make_rule( "log10", "R.log10"),
        make_rule( "log2", "R.log2"),
        # Missing beta, lbeta, psigamma
        make_rule("choose", "R.combinations"),
        make_rule("lchoose", "R.lchoose"),
        make_rule("factorial", "R.factorial"),
        make_rule("lfactorial", "R.lfactorial"),

        # JavaScript ----
        # make_rule("NULL", "null"),   # doesn't work since R doesn't distinguish input NULL and empty NULL.
        make_rule("JS_NULL", "null"),
        make_rule("JS_UNDEFINED", "undefined"),
        make_rule("JS_NA", "NaN"),
        make_rule("JS_Array", "["),

        # jQuery ----
        make_rule("jQuery", "$")
    )
}
