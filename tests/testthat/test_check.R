testthat::context("Test pre-transpilation check")

test_that("Test reserved words are not assigned any value and not
          used as function arguments", {
    # basic_rules
    unit_test_basic <- function(expr) {
        safeguard(parse_expr(expr), rules = basic_rules(), deparsers = basic_deparsers())
    }
    expect_silent(unit_test_basic("max <- 3"))
    expect_warning(unit_test_basic("pipe <- 3"))

    # default_rules
    unit_test_default <- function(expr) {
        safeguard(parse_expr(expr), rules = default_rules(), deparsers = basic_deparsers())
    }
    expect_silent(unit_test_default("list(max = 3)"))
    expect_warning(unit_test_default("max <- 3"))
    expect_warning(unit_test_default("max = 3"))
    expect_warning(unit_test_default("function(min = 0, max = 1) {}"))
})
