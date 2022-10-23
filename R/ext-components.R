# This is exactly the same as `deparse_js`, but it is reused as a
# R-AST-to-JavaScript-AST rewriter. This function is created to avoid
# confusion about the return type (as `deparse_js` said it returns
# character string, not AST).
# As a technical note, the return type of `deparse_js` depends on the
# return type of the `deparsers` provided. In the standard usage, this
# would be character string. But in the case of source map generation,
# we need to preserve the AST structure after the transpilation, so
# the return type is a list instead.
rewrite_annotated_exprs <- deparse_js

compile_sym <- deparse_sym

compile_call <- function(ast, ...) {
    sym_ls <- purrr::map(ast, rewrite_annotated_exprs, ...)
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

barebone <- function() {
    list(
        "call"   = make_deparser(is_call, compile_call),
        "symbol" = make_deparser(is_sym, compile_sym)
    )
}
