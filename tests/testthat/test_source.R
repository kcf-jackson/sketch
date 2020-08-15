testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = T, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "2bf63af28e42c10198dbb469d7108fc2"
    )

    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "1b30eb33d37f87a934e01f7e9af99f38"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = "NULL")
    testthat::expect_equal(
        md5hash(output_file),
        "300bee07f61774af1b632a1d57aafcb5"
    )
    setwd(current_wd)
})
