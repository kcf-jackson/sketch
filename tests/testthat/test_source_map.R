testthat::context("Test source map")

testthat::test_that("Test source map", {
    # input <- "f(a + b)"
    # input <- "f(g(a, b + c))"
    # input <- "f(g(a, b + c, b + c))"
    # input <- "f(a+1, b+1, c+1)"
    # print(input)

    test_input <- c("f(a + b)", "f(g(a, b + c))",
                    "f(g(a, b + c, b + c))", "f(a+1, b+1, c+1)")

    # Start with an R string
    for (input in test_input) {
        # Parse the R string in to AST and annotate the nodes with the lines and
        # columns information
        input_ast <- annotate_exprs(input)[[1]]
        # print(input_ast)


        # Rewrite R AST into JavaScript AST
        compiled_ast <- rewrite_annotated_exprs(input_ast, deparsers = barebone())
        # print(compiled_ast)
        # print(deparse_js_ast(compiled_ast))  # inspect the deparsed string


        # Generate source map
        src_map <- source_map(compiled_ast)
        src_map_table <- source_map_table(src_map)
        check_src_map_table <- verify_source_map(compiled_ast, src_map)
        # print(src_map_table)
        # print(check_src_map_table)
        testthat::expect_true(all(check_src_map_table$pass_test))


        # R source map -> .json --[source-map]--> .map -> embed in browser
        input_R <- tempfile()
        write(input, file = input_R)

        output_JS <- compile_r(input = input_R, output = tempfile())
        output_src_map <- source_map_from_files(input_R, output_JS,
                                                deparsers = barebone())
    }
})
