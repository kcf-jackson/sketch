testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    testthat::skip_on_os("windows") # see notes at the bottom of "test_assets.R"

    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "ca765abcb55f5332acc8d8f245de5b75"
    )

    output_file <- source_r(file, launch_browser = "NULL") # NULL in string is intentional here
    testthat::expect_equal(
        md5hash(output_file),
        "0e360fe3d2e326c35e2884dbd2999f70"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "b41c189682f2eed86422581a44e6bc8a"
    )
    setwd(current_wd)
})
