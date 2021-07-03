testthat::context("Test bundle function")

testthat::test_that("Bundling JavaScript and sketch R files - normal case", {
    js_file <- tempfile(fileext = ".js")
    r_file <- tempfile(fileext = ".R")

    writeLines("x = 1", js_file)
    writeLines("y <- 2", r_file)

    bundle_file <- bundle(c(js_file, r_file))
    bundle_lines <- readLines(bundle_file)

    # Note that 2 empty lines are inserted between two files by `bundle`
    expect_equal(bundle_lines[1], "x = 1")
    expect_equal(bundle_lines[5], "y = 2")
})

testthat::test_that("Bundling JavaScript and sketch R files - warning about skipping", {
    js_file <- tempfile(fileext = ".js")
    r_file <- tempfile(fileext = ".R")
    skip_file <- tempfile(fileext = ".md")

    writeLines("x = 1", js_file)
    writeLines("y <- 2", r_file)
    writeLines("#Readme", skip_file)

    expect_warning(bundle(c(js_file, r_file, skip_file)))
})

testthat::test_that("Bundling JavaScript and sketch R files - error when a file is not found", {
    js_file <- tempfile(fileext = ".js")
    r_file <- tempfile(fileext = ".R")

    writeLines("x = 1", js_file)
    writeLines("y <- 2", r_file)

    expect_error(bundle(c(js_file, r_file, "random_file.Cpp")))
})
