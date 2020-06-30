testthat::context("Test helpers")

testthat::test_that("src", {
    testthat::expect_true(is.character(src("p5")))
    testthat::expect_true(is.character(src("d3")))
    testthat::expect_true(is.character(src("chart")))
    testthat::expect_true(is.character(src("plotly")))
    testthat::expect_error(src("d4"))
})

testthat::test_that("let / declare", {
    testthat::expect_null(let(x))
    testthat::expect_null(declare(x))
    testthat::expect_null(let(x, y))
    testthat::expect_null(declare(x, y))
    testthat::expect_null(let(x = 3))
    testthat::expect_null(declare(x = 3))
    testthat::expect_null(let(x = 3, y))
    testthat::expect_null(declare(x = 3, y))
})
