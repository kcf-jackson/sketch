testthat::context("Test R Markdown support")


testthat::test_that("Test inserting an sketch app into an R Markdown document", {
    file <- system.file("test_files/test_RMD.R", package = "sketch")
    dir <- tempdir()

    # Return an HTML string when `render` is TRUE
    x <- insert_sketch(file, render = TRUE)
    testthat::expect_true(is.character(x))
    testthat::expect_true("html" %in% class(x))

    x <- insert_sketch(file, id = "sketch_1", output_dir = dir, render = TRUE)
    testthat::expect_true(is.character(x))
    testthat::expect_true("html" %in% class(x))
    testthat::expect_error(
        insert_sketch(file, id = "sketch_1", output_dir = "ERROR_DIR", render = TRUE)
    )

    # Return a 'shiny.tag' when `render` is FALSE
    x <- insert_sketch(file, render = FALSE)
    testthat::expect_equal(class(x), "shiny.tag")

    x <- insert_sketch(file, id = "sketch_1", output_dir = dir, render = FALSE)
    testthat::expect_equal(class(x), "shiny.tag")
    testthat::expect_error(
        insert_sketch(file, id = "sketch_1", output_dir = "ERROR_DIR", render = FALSE)
    )
})


testthat::test_that("Test sketch language engine", {
    knitr::knit_engines$set(sketch = sketch::eng_sketch)
    options <- list(
        eval = FALSE, echo = FALSE, code = NULL,
        engine = "sketch"
    )
    testthat::expect_equal(eng_sketch(options), "")
})



