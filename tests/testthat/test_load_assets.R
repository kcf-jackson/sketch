testthat::context("Extra tests for the 'asset list'-class")

testthat::test_that("Test predicate function", {
    x <- asset_list(list(1,2,3), list('a','b','c'))
    testthat::expect_true(is.asset_list(x))
})

testthat::test_that("Test getter and setter for 'head'", {
    x <- asset_list(list(1,2,3), list('a','b','c'))
    testthat::expect_equal(ahead(x), list(1,2,3))

    y <- asset_list(list(1,2,3,4), list('a','b','c'))
    testthat::expect_equal(append_to_head(x, 4), y)
})

testthat::test_that("Test getter and setter for 'body'", {
    x <- asset_list(list(1,2,3), list('a','b','c'))
    testthat::expect_equal(abody(x), list('a','b','c'))

    y <- asset_list(list(1,2,3), list('a','b','c','d'))
    testthat::expect_equal(append_to_body(x, 'd'), y)
})

testthat::test_that("Test conversion to asset list", {
    x <- asset_list(list(1,2,3), list('a','b','c'))
    y <- list(list(1,2,3), list('a','b','c'))
    testthat::expect_equal(x, as_asset_list(y))

    y <- list(list(1,2,3))
    testthat::expect_error(as_asset_list(y))
})
