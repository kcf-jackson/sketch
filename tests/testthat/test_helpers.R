testthat::context("Test helpers")

testthat::test_that("`src` returns correct type: character.", {
    testthat::expect_true(is.character(src("p5")))
    testthat::expect_true(is.character(src("d3")))
    testthat::expect_true(is.character(src("chart")))
    testthat::expect_true(is.character(src("plotly")))
    testthat::expect_true(is.character(src("mathjs")))
    testthat::expect_true(is.character(src("vegalite")))
    testthat::expect_true(is.character(src("tensorflow")))
    testthat::expect_true(is.character(src("websocket")))
    testthat::expect_true(is.character(src("dom")))
    testthat::expect_true(is.character(src("io")))
    testthat::expect_true(is.character(src("testthat")))
    testthat::expect_true(is.character(src("fontawesome")))
    testthat::expect_true(is.character(src("ionicons")))
    testthat::expect_true(is.character(src("tailwind")))
    testthat::expect_true(is.character(src("ramda")))
    testthat::expect_true(is.character(src("purrr")))
    # Modules CDN
    testthat::expect_true(is.character(src("cdn-dom")))
    testthat::expect_true(is.character(src("cdn-io")))
    testthat::expect_true(is.character(src("cdn-websocket")))
    testthat::expect_true(is.character(src("cdn-testthat")))
    testthat::expect_true(is.character(src("cdn-purrr")))
    testthat::expect_error(src("d4"))
})

testthat::test_that("let / declare / var / const", {
    testthat::expect_null(let(x))
    testthat::expect_null(const(x))
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

testthat::test_that("Test `license_info`", {
    testthat::expect_true(is.list(license_info("p5")))
    testthat::expect_true(is.list(license_info("d3")))
    testthat::expect_true(is.list(license_info("chart")))
    testthat::expect_true(is.list(license_info("plotly")))
    testthat::expect_true(is.list(license_info("mathjs")))
    testthat::expect_true(is.list(license_info("vegalite")))
    testthat::expect_true(is.list(license_info("tensorflow")))
    testthat::expect_true(is.list(license_info("fontawesome")))
    testthat::expect_true(is.list(license_info("ionicons")))
    testthat::expect_true(is.list(license_info("tailwind")))
    testthat::expect_true(is.list(license_info("ramda")))
})
