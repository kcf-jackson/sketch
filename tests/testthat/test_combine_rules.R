testthat::context("Test rewriter optimisation")

testthat::test_that("Test `combine_rules` and `split_rules`", {
    rules <- list(
        make_rule("$", "."),
        make_rule("%+%", "+"),
        make_rule("T", "true")
    )
    # One group
    res <- combine_rules(rules, rep(1, 3))
    testthat::expect_length(res, 1)
    testthat::expect_true(is.list(res))

    res2 <- split_rules(res)
    testthat::expect_true(is.list(res2))
    testthat::expect_length(res2, 3)

    # Two groups
    res <- combine_rules(rules, c(1, 1, 2))
    testthat::expect_true(is.list(res))
    testthat::expect_length(res, 2)
    testthat::expect_equal(attr(res[[1]], "to"), "true")  # Check precedence

    res2 <- split_rules(res)
    testthat::expect_true(is.list(res2))
    testthat::expect_length(res2, 3)
})
