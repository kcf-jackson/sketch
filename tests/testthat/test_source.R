testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    testthat::skip_on_os("windows") # see notes at the bottom of "test_assets.R"

    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "f0f211e785688075cd594285e43ac9be"
    )

    output_file <- source_r(file, launch_browser = "NULL") # NULL in string is intentional here
    testthat::expect_equal(
        md5hash(output_file),
        "ba464b41ce469fe2c32893c7890162ed"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "8806c1ee07d391aaebec40067b757df4"
    )
    setwd(current_wd)
})
