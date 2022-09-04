compile_sym <- deparse_sym

compile_call <- function(ast, ...) {
    sym_ls <- purrr::map(ast, deparse_js, ...)
    fun <- sym_ls[[1]]
    if (length(sym_ls) == 2) {
        args <- sym_ls[-1]
    } else {
        args <- vector("list", 2 * length(sym_ls[-1]) - 1)
        args[seq(1, length(args), 2)] <- sym_ls[-1]
        args[seq(2, length(args), 2)] <- ", "
    }
    res <- c(list(fun, "("), args, list(")"))
    attributes(res) <- attributes(ast)
    res
}
