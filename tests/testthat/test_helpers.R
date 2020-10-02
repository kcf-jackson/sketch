testthat::context("Test helpers")

testthat::test_that("`src` returns correct type: character.", {
    testthat::expect_true(is.character(src("p5")))
    testthat::expect_true(is.character(src("d3")))
    testthat::expect_true(is.character(src("chart")))
    testthat::expect_true(is.character(src("plotly")))
    testthat::expect_true(is.character(src("mathjs")))
    testthat::expect_true(is.character(src("vegalite")))
    testthat::expect_true(is.character(src("tensorflow")))
    testthat::expect_true(is.character(src("dom")))
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

testthat::test_that("do_call", {
    f <- function(x, y, z = 0) x + y + z
    args <- list(a = 1, b = 2, z = 3)
    testthat::expect_equal(
        do.call(f, list(x = 1, y = 2, z = 3)),
        do_call(f, x = 1, y = 2,
                extended_args = capture_args(args, "z")$keep)
    )
})
