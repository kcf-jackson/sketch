testthat::context("Extra tests for rewriter")

testthat::test_that("'print.rule' is dispatched correctly", {
    rule <- make_rule("R", "JS")
    testthat::expect_equal(
        capture.output(print(rule))[1],
        "Rule: Rewrite 'R' to 'JS'."
    )
})

testthat::test_that("NULL and NA are not rewritten", {
    testthat::expect_true(
        is.na(subst(parse_expr("NA"), "NA", "123"))
    )
    testthat::expect_true(
        is.null(subst(parse_expr("NULL"), "NULL", "123"))
    )
})