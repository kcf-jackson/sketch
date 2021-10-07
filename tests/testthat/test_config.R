testthat::context("Test support of configuration")

testthat::test_that("Test `with_config` on files", {
    # Configured to use the `basic_rules` =============================
    file <- system.file("test_files/test_config.R", package = "sketch")
    # Check the default is `default_rules()`
    testthat::expect_equal(
        deparse1(formals(compile_r)$rules),
        "default_rules()"
    )
    # Check that the configuration is actually used
    testthat::expect_equal(
        capture.output(compile_r(file)),
        "print(12345)"
    )
    # Check that the configuration can be further overridden
    testthat::expect_equal(
        capture.output(compile_r(file, rules = default_rules())),
        "R.print(12345)"
    )

    # Two configurations ==============================================
    file2 <- system.file("test_files/test_config_2.R", package = "sketch")
    testthat::expect_error(compile_r(file2))
    testthat::expect_error(compile_r(file2, rules = default_rules()))
})


testthat::test_that("Test that `with_config` returns a function with the same interface as the input function", {
    f <- function(a = 1, b = 1, file_input) {}
    config_f <- with_config("file_input", f)
    testthat::expect_equal(
        formalArgs(f),
        formalArgs(config_f)
    )
})


testthat::test_that("Test `merge_alist`", {
    # No optional argument - update only the named variable
    x <- formals(function(a = 1, b = 2) {})
    y <- list(b = 999, c = 3)
    res <- list(a = 1, b = 999)
    testthat::expect_equal(
        merge_alist(x, y),
        res
    )

    x <- formals(function(a, b = 2) {})
    y <- list(b = 999, c = 3)
    res <- list(b = 999)
    testthat::expect_equal(
        merge_alist(x, y)$b,
        res$b
    )

    # Has optional argument
    # - Update only the named variable
    x <- formals(function(a = 1, b = 2, ... = 12) {})
    y <- list(b = 999)
    res <- list(a = 1, b = 999, 12)
    testthat::expect_equal(
        expand_dots(merge_alist(x, y)),
        res
    )
    # - Update both named and optional variables
    x <- formals(function(a = 1, b = 2, ...) {})
    y <- list(b = 999, c = 4)
    res <- list(a = 1, b = 999, c = 4)
    testthat::expect_equal(
        expand_dots(merge_alist(x, y)),
        res
    )
    # - Update both named and optional variables with default
    x <- formals(function(a = 1, b = 2, ... = 12) {})
    y <- list(b = 999, c = 4)
    res <- list(a = 1, b = 999, c = 4)
    testthat::expect_equal(
        expand_dots(merge_alist(x, y)),
        res
    )
    x <- formals(function(a = 1, b = 2, ... = 12) {})
    y <- list(b = 999, c = 4, d = 5)
    res <- list(a = 1, b = 999, c = 4, d = 5)
    testthat::expect_equal(
        expand_dots(merge_alist(x, y)),
        res
    )
})


testthat::test_that("Test `expand_dots`", {
    input <- list(x = 1, y = 2, "..." = list(z = 1, a = 10))
    result <- expand_dots(input)
    expected <- list(x = 1, y = 2, z = 1, a = 10)
    testthat::expect_equal(result, expected)

    f <- function(x = 1, y, ...) {
        as.list(match.call(expand.dots = FALSE))[-1]
    }
    testthat::expect_null(
        expand_dots(formals(f))[["..."]]
    )
    # All arguments (dots without name)
    testthat::expect_equal(
        expand_dots(f(1, 2, 3)),
        list(x = 1, y = 2, 3)
    )
    # No dots
    testthat::expect_equal(
        expand_dots(f(1, 2)),
        list(x = 1, y = 2)
    )
    # All arguments (dots with name)
    testthat::expect_equal(
        expand_dots(f(1, z = 3)),
        list(x = 1, z = 3)
    )

    f2 <- function(x = 1, y, ... = 999) {
        as.list(match.call(expand.dots = FALSE))[-1]
    }
    testthat::expect_equal(
        expand_dots(formals(f2))[[3]],
        999
    )
    # All arguments (dots without name)
    testthat::expect_equal(
        expand_dots(f2(1, 2, 3)),
        list(x = 1, y = 2, 3)
    )
    # No dots
    testthat::expect_equal(
        expand_dots(f2(1, 2)),
        list(x = 1, y = 2)
    )
    # All arguments (dots with name)
    testthat::expect_equal(
        expand_dots(f2(1, z = 3)),
        list(x = 1, z = 3)
    )
})
