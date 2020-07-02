testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    testthat::expect_equal(
        capture.output(source_r(file, debug = T)),
        "x = R.sin(10)"
    )

    testthat::skip_if_offline()
    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "d71f3efd2b8c3a2aba38e34c32f13790"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "699bef32ed7cf9e2b12b21e72e43ac3e"
    )
    setwd(current_wd)
})
