#' Mapping R operators into JavaScript operators
#'
#' @name r-to-js-rules
#'
#' @references R operators: \url{https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Operators}
#' @references R infix and prefix operators: \url{https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Infix-and-prefix-operators}
#' @references JavaScript operators: \url{https://www.w3schools.com/js/js_operators.asp}
#'
#' @note These functions are used as inputs to \link{compile_r} and \link{compile_exprs}.
#'
#' @examples
#' basic_rules()
#'
#' @export
basic_rules <- function() {
    list(
        make_rule("<-", "="),
        make_rule("<<-", "="),
        make_rule("^", "**"),
        make_rule("%%", "%"),
        make_rule("$", "."),
        make_rule("::", "."),
        make_rule("%instanceof%", "instanceof"),
        make_rule("%+%", "+"),
        make_rule("%=>%", "=>"),
        make_rule("%>%", "pipe"),
        make_rule("T", "true"),
        make_rule("F", "false"),
        make_rule("TRUE", "true"),
        make_rule("FALSE", "false"),
        make_rule("declare", "let"),
        make_rule("self", "this"),
        make_rule("stop", "throw"),
        make_rule("JS_NULL", "null"),
        make_rule("JS_UNDEFINED", "undefined"),
        make_rule("JS_NAN", "NaN"),
        make_rule("JS_ARRAY", "Array")
    )
}


#' @rdname r-to-js-rules
#'
#' @examples
#' default_rules()
#'
#' @export
default_rules <- function() {
    list(
        # Binary operators
        make_rule("<-", "="),
        make_rule("<<-", "="),
        make_rule("::", "."),
        make_rule("$",  "."),
        make_rule("+",  "R.add"),
        make_rule("-",  "R.subtract"),
        make_rule("*",  "R.multiply"),
        make_rule("%*%","R.matMultiply"),
        make_rule("/",  "R.divide"),
        make_rule("^",  "R.pow"),
        make_rule("%%", "R.mod"),
        make_rule("identical", "R.identical"),
        make_rule("==", "R.EQ"),
        make_rule("<=", "R.LEQ"),
        make_rule(">=", "R.GEQ"),
        make_rule("!=", "R.NEQ"),
        make_rule("<",  "R.LT"),
        make_rule(">",  "R.GT"),
        make_rule("&&", "R.and"),
        make_rule("||", "R.or"),
        make_rule("!",  "R.not"),
        make_rule("xor", "R.xor"),
        make_rule("[[", "R.extract2"),
        make_rule("[",  "R.extract"),
        make_rule(":",  "R.seq"),
        make_rule("%instanceof%", "instanceof"),
        make_rule("%=>%", "=>"),
        make_rule("%+%", "+"),
        make_rule("%-%", "-"),
        make_rule("%/%", "R.intDivide"),
        make_rule("%o%", "R.compose"),
        make_rule("%>%", "pipe"),

        # Base Javascript
        make_rule("TRUE", "true"),
        make_rule("T", "true"),
        make_rule("FALSE", "false"),
        make_rule("F", "false"),
        make_rule("declare", "let"),
        make_rule("self", "this"),
        make_rule("stop", "throw"),

        # Basic R functions
        make_rule(    "pi", "R.pi"),
        make_rule(   "seq", "R.seq"),
        make_rule( "print", "R.print"),
        make_rule("length", "R.length"),
        make_rule("walk", "R.walk"),
        make_rule("first", "R.first"),
        make_rule("typeof", "R.typeof"),
        make_rule("which", "R.which"),

        # Data structure
        make_rule("c", "R.c"),
        make_rule("rep", "R.rep"),
        make_rule("matrix", "R.matrix2"),
        make_rule("array", "R.array"),
        make_rule("dim", "R.dim"),
        make_rule("list", "list"),
        make_rule("names", "R.names"),
        make_rule("append", "R.append"),
        make_rule("data.frame", "R.data_frame"),
        make_rule("filter", "R.filter"),
        make_rule("mutate", "R.mutate"),
        make_rule("arrange", "R.arrange"),
        make_rule("select", "R.select"),
        make_rule("summarise", "R.summarise"),
        make_rule("count", "R.count"),
        make_rule("rbind", "R.rbind"),
        make_rule("colnames", "R.colnames"),

        # High order functions
        make_rule(   "map",  "R.map"),
        make_rule(  "map2",  "R.map2"),
        make_rule(  "pmap",  "R.pmap"),
        make_rule("reduce",  "R.reduce"),
        make_rule("reduce_right", "R.reduce_right"),
        make_rule("compose", "R.compose"),

        # Optimisation
        make_rule("uniroot", "R.uniroot"),

        # Set functions
        make_rule("unique", "R.unique"),
        make_rule("union", "R.union"),
        make_rule("intersect", "R.intersect"),
        make_rule("setdiff", "R.setdiff"),
        make_rule("setequal", "R.setequal"),
        make_rule("is.element", "R.is_element"),
        make_rule("is.subset", "R.is_subset"),   # not in R
        make_rule("setsymdiff", "R.setsymdiff"), # not in R

        # Statistics functions
        make_rule("mean", "R.mean"),
        make_rule("median", "R.median"),
        make_rule("sd", "R.sd"),
        make_rule("var", "R.var"),
        make_rule("quantile", "R.quantile"),

        # base::groupGeneric
        # Group "math" ----
        make_rule(    "abs", "R.abs"),
        make_rule(   "sign", "R.sign"),
        make_rule(   "sqrt", "R.sqrt"),
        make_rule(  "floor", "R.floor"),
        make_rule("ceiling", "R.ceiling"),
        make_rule(  "trunc", "R.fix"),
        make_rule(  "round", "R.round"),
        make_rule( "signif", "R.signif"),

        make_rule( "exp", "R.exp"),
        make_rule( "expm1", "R.expm1"),
        make_rule( "log", "R.log"),
        make_rule( "log1p", "R.log1p"),
        make_rule( "log10", "R.log10"),
        make_rule( "log2", "R.log2"),

        # Trigonometric functions
        make_rule( "cos", "R.cos"),
        make_rule( "sin", "R.sin"),
        make_rule( "tan", "R.tan"),
        make_rule( "cospi", "R.cospi"),
        make_rule( "sinpi", "R.sinpi"),
        make_rule( "tanpi", "R.tanpi"),
        make_rule("acos", "R.acos"),
        make_rule("asin", "R.asin"),
        make_rule("atan", "R.atan"),
        make_rule("atan2", "R.atan2"),
        make_rule( "cosh", "R.cosh"),
        make_rule( "sinh", "R.sinh"),
        make_rule( "tanh", "R.tanh"),
        make_rule("acosh", "R.acosh"),
        make_rule("asinh", "R.asinh"),
        make_rule("atanh", "R.atanh"),

        make_rule("cot", "R.cot"),
        make_rule("csc", "R.csc"),
        make_rule("sec", "R.sec"),
        make_rule("acot", "R.acot"),
        make_rule("acsc", "R.acsc"),
        make_rule("asec", "R.asec"),
        make_rule("coth", "R.coth"),
        make_rule("csch", "R.csch"),
        make_rule("sech", "R.sech"),
        make_rule("acoth", "R.acoth"),
        make_rule("acsch", "R.acsch"),
        make_rule("asech", "R.asech"),

        # Special functions
        make_rule("erf", "R.erf"),
        make_rule("gamma",    "R.gamma"),
        make_rule("lgamma",   "R.lgamma"),
        make_rule("digamma",  "R.digamma"),
        make_rule("trigamma", "R.trigamma"),
        # TODO: beta, lbeta, psigamma
        make_rule("choose", "R.choose"),
        make_rule("lchoose", "R.lchoose"),
        make_rule("factorial", "R.factorial"),
        make_rule("gammainc", "R.ingamma"),
        # make_rule("lfactorial", "R.lfactorial"),

        make_rule("cumsum",  "R.cumsum"),
        make_rule("cumprod", "R.cumprod"),
        make_rule("cummax",  "R.cummax"),
        make_rule("cummin",  "R.cummin"),

        # Group "Summary" ----
        make_rule(  "all", "R.all"),
        make_rule(  "any", "R.any"),
        make_rule(  "sum", "R.sum"),
        make_rule( "prod", "R.prod"),
        make_rule(  "min", "R.min"),
        make_rule(  "max", "R.max"),
        make_rule("range", "R.range2"),

        # Group "Complex" ----
        make_rule("complex", "R.complex"),
        make_rule("Re", "R.Re"),
        make_rule("Im", "R.Im"),
        make_rule("Mod", "R.Mod"),
        make_rule("Arg", "R.Arg"),
        make_rule("Conj", "R.Conj"),

        # JavaScript ----
        # make_rule("NULL", "null"),   # doesn't work since R doesn't distinguish input NULL and empty NULL.
        make_rule("JS_NULL", "null"),
        make_rule("JS_UNDEFINED", "undefined"),
        make_rule("JS_NAN", "NaN"),
        make_rule("JS_ARRAY", "Array"),

        # jQuery ----
        make_rule("jQuery", "$"),

        # Distributions functions
        # make_rule("discrete_inverse", "R.discrete_inverse"),
        make_rule("dbinom", "R.dbinom"),
        make_rule("pbinom", "R.pbinom"),
        make_rule("qbinom", "R.qbinom"),
        make_rule("rbinom", "R.rbinom"),
        make_rule("dchisq", "R.dchisq"),
        make_rule("pchisq", "R.pchisq"),
        make_rule("qchisq", "R.qchisq"),
        make_rule("rchisq", "R.rchisq"),
        make_rule("dexp", "R.dexp"),
        make_rule("pexp", "R.pexp"),
        make_rule("qexp", "R.qexp"),
        make_rule("rexp", "R.rexp"),
        make_rule("dgamma", "R.dgamma"),
        make_rule("pgamma", "R.pgamma"),
        make_rule("qgamma", "R.qgamma"),
        make_rule("rgamma", "R.rgamma"),
        make_rule("dgeom", "R.dgeom"),
        make_rule("pgeom", "R.pgeom"),
        make_rule("qgeom", "R.qgeom"),
        make_rule("rgeom", "R.rgeom"),
        make_rule("dlnorm", "R.dlnorm"),
        make_rule("plnorm", "R.plnorm"),
        make_rule("qlnorm", "R.qlnorm"),
        make_rule("rlnorm", "R.rlnorm"),
        make_rule("dnorm", "R.dnorm"),
        make_rule("pnorm", "R.pnorm"),
        make_rule("qnorm", "R.qnorm"),
        make_rule("rnorm", "R.rnorm"),
        make_rule("dpois", "R.dpois"),
        make_rule("ppois", "R.ppois"),
        make_rule("qpois", "R.qpois"),
        make_rule("rpois", "R.rpois"),
        make_rule("dunif", "R.dunif"),
        make_rule("punif", "R.punif"),
        make_rule("qunif", "R.qunif"),
        make_rule("runif", "R.runif")
    )
}


#' @rdname r-to-js-rules
#'
#' @examples
#' basic_rules()
#'
#' @note lifecycle: experimental
#'
#' @export
basic_2_rules <- function() {
    basic_rules()[-c(1,2)]
}


#' @rdname r-to-js-rules
#'
#' @examples
#' default_2_rules()
#'
#' @note lifecycle: experimental
#'
#' @export
default_2_rules <- function() {
    default_rules()[-c(1,2)]
}
