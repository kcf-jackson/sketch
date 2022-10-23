testthat::context("Test the 'testthat' module")

testthat::test_that("Test 'testthat' helper functions", {
    testthat::expect_false(is_websocket("load_library(\"dom\")"))
    testthat::expect_true(is_websocket("load_library(\"websocket\")"))

    temp <- tempfile()
    write("#! load_library('websocket')", temp)
    testthat::expect_true(has_websocket(temp))
    write("#! load_library('dom')", temp)
    testthat::expect_false(has_websocket(temp))

    temp <- tempfile()
    write("#| load_library('websocket')", temp)
    testthat::expect_true(has_websocket(temp))
    write("#| load_library('dom')", temp)
    testthat::expect_false(has_websocket(temp))
})

testthat::test_that("Test 'testthat' report", {
    app_file <- system.file("test_files/test_testthat_app.R", package = "sketch")
    test_file <- system.file("test_files/test_testthat_test.R", package = "sketch")

    # Running without a browser only checks that the code runs
    # successfully to the end, but it does not perform the tests
    # in the `test_file`.
    res <- test_sketch(app_file, test_file, launch_browser = NULL)
    # Clean up without waiting
    if (res$started) {
        has_started <- TRUE
        res$stopServer()
    }
    # Add one to the test counter
    testthat::expect_true(has_started)

    # The following test needs to run interactively in R, hence
    # it is skipped on cran and ci.
    testthat::skip_on_cran()
    testthat::skip_on_ci()

    # The following command will launch a browser.
    res <- test_sketch(app_file, test_file, launch_browser = "browser")

    # test_file_2 <- system.file("test_files/test_testthat_test_2.R", package = "sketch")
    # res <- test_sketch(app_file, test_file_2)
})

# Note 1. At the websocket end, it takes some time for tasks to complete and the
# connection to close. This would interfere with "testthat/test_websocket.R" if the
# connection is not closed in time. As there is no way to block a thread while
# requiring it to be available (to listen) at the same time, the test (for the
# `in_handler` portion) needs to be run interactively.
