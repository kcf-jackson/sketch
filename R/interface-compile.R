#' Compile an R file into a JavaScript file
#'
#' @param input A character string; the input file.
#' @param output A character string; the output file. When the
#' output is "", the result is printed to the standard output.
#' @param rules A list of rewriting rules. See [make_rule] for more detail.
#' @param deparsers A list of deparsers. See [make_deparser] for more detail.
#'
#' @return A character string; the output file path.
#'
#' @examples
#' file <- system.file("test_files/test_source.R", package = "sketch")
#' readLines(file)
#' compile_r(input = file)
#'
#' @export
compile_r <- with_config(
    "input",
    function(input, output = "",
             rules = default_rules(),
             deparsers = default_deparsers()) {
        file(input) %>%
            compile_exprs(rules, deparsers) %>%
            write(file = output)
        invisible(output)
    }
)


#' Compile R code into JavaScript code
#'
#' @param x A character string; the expression to transpile to JS.
#' @param rules A list of rewriting rules. See [make_rule] for more detail.
#' @param deparsers A list of deparsers. See [make_deparser] for more detail.
#'
#' @return A character string.
#'
#' @examples
#' compile_exprs("R + Cpp", list(make_rule('Cpp', 'JS')))
#' compile_exprs("math.add(a, b)", list(make_rule('math.add', '+')))
#'
#' @export
compile_exprs <- function(x, rules = default_rules(),
                          deparsers = default_deparsers()) {
    rlang::parse_exprs(x) %T>%
        purrr::map(safeguard, rules = rules) %>%
        purrr::map(rewrite, rules = rules) %>%
        purrr::map_chr(deparse_js, deparsers = deparsers)
}


#' Compile active file in 'RStudio'
#'
#' @param ... Optional arguments to pass to \code{compile_r}.
#'
#' @examples
#' \dontrun{
#' # At 'RStudio', opens a 'sketch' R file in the editor, then
#' # run the following:
#' compile_active()
#' }
#'
#' @export
compile_active <- function(...) {
    compile_r(copy_active_to_tempfile(), ...)  # nocov
}
