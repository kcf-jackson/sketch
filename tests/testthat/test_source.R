testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    testthat::skip_on_os("windows") # see notes at the bottom of "test_assets.R"

    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "4662b4a55f4d6dd121c3c75800f7359f"
    )

    output_file <- source_r(file, launch_browser = "NULL") # NULL in string is intentional here
    testthat::expect_equal(
        md5hash(output_file),
        "9b313f58f7368bbac4dcc5a21d6fb5ca"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "d9eec0f1b4f01df4d225d629e1a005bf"
    )
    setwd(current_wd)
})
