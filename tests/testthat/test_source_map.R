# barebone <- list(
#     "call"   = make_deparser(is_call, compile_call),
#     "symbol" = make_deparser(is_sym, compile_sym)
# )
#
# # Main
# input <- "f(a + b)"
# # input <- "f(g(a, b + c))"
# # input <- "f(g(a, b + c, b + c))"
# # input <- "f(a+1, b+1, c+1)"
# print(input)
#
# compiled_ast <- deparse_js(annotate_exprs(input)[[1]], barebone)
# print(compiled_ast)
# print(deparse_js_ast(compiled_ast))
#
# src_map <- source_map(compiled_ast)
# print(source_map_table(src_map))
# print(verify_source_map(compiled_ast, src_map))
#
# # turn R source map -> .json --source-map--> .map -> embed in browser
# src_map
