testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    testthat::expect_equal(
        capture.output(source_r(file, debug = T)),
        "x = R.sin(10)"
    )

    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "936f7729da1dcee343f3cf3b368a5f4f"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "7cd25ac544d727b25bf6396b2528efe2"
    )
    setwd(current_wd)
})
