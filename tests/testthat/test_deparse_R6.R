testthat::context("Test R6Class transpilation")

testthat::test_that("Test component functions", {
    # deparse_public_list
    expr <- parse_expr('list(x = 1, y = 2, z = function(x) {x})')
    expect_equal(
        deparse_public_list(expr, default_deparsers()),
        "self.x = 1\n    self.y = 2\n    self.z = function(x) {\n        x\n    }"
    )

    # deparse_private_list
    expr <- parse_expr('list(x = 1, y = 2, z = function(x) {x})')
    expect_equal(
        deparse_private_list(expr, default_deparsers()),
        "private.x = 1\n    private.y = 2\n    private.z = function(x) {\n        x\n    }"
    )
    expr <- parse_expr('list(x = 1, y = 2, z = function(x) {this$x})')
    expect_equal(
        deparse_private_list(expr, default_deparsers()),
        "private.x = 1\n    private.y = 2\n    private.z = function(x) {\n        $(that, x)\n    }"
    )

    # get_constructor_arg
    expr <- parse_expr('list(x = 1, y = 2, z = function(x) {x})')
    expect_equal(
        get_constructor_arg(expr, default_deparsers()),
        ""
    )
    expr <- parse_expr('list(initialize = function(x) {x})')
    expect_equal(
        get_constructor_arg(expr, default_deparsers()),
        "x"
    )
    expr <- parse_expr('list(initialize = function(x = 1) {x})')
    expect_equal(
        get_constructor_arg(expr, default_deparsers()),
        "x = 1"
    )
    expr <- parse_expr('list(initialize = function(x = 1, y) {x})')
    expect_equal(
        get_constructor_arg(expr, default_deparsers()),
        "x = 1, y"
    )
    expr <- parse_expr('list(initialize = function(x = 1, y = 3) {x})')
    expect_equal(
        get_constructor_arg(expr, default_deparsers()),
        "x = 1, y = 3"
    )
})
