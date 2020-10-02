testthat::context("Test source files")

md5hash <- function(x) as.character(tools::md5sum(x))

testthat::test_that("Source sketch R script", {
    testthat::skip_on_os("windows") # see notes at the bottom of "test_assets.R"

    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "ae5f920ca9ecb7f10170b33a5fe7a731"
    )

    output_file <- source_r(file, launch_browser = "NULL") # NULL in string is intentional here
    testthat::expect_equal(
        md5hash(output_file),
        "15763ea4da134d970053aaf7a9f83098"
    )

    # Need to set path so that referencing to another file would work
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    output_file <- source_r(file, launch_browser = NULL)
    testthat::expect_equal(
        md5hash(output_file),
        "bad479460110bd35996edd73940f4c8a"
    )
    setwd(current_wd)
})


testthat::test_that("basic_tags()", {
    # Simple file structure
    file <- system.file("test_files/test_sketch_basic.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    output_file_2 <- source_r(file, debug = TRUE, launch_browser = NULL,
                              asset_tags = basic_tags())
    testthat::expect_gt(
        file.size(output_file),   # with R functions
        file.size(output_file_2)  # without R functions
    )

    # With recursive dependencies
    current_wd <- getwd()
    dir <- system.file("test_files", package = "sketch")
    file <- "test_sketch.R"
    setwd(dir)
    file <- system.file("test_files/test_sketch.R", package = "sketch")
    output_file <- source_r(file, debug = TRUE, launch_browser = NULL)
    output_file_2 <- source_r(file, debug = TRUE, launch_browser = NULL,
                              asset_tags = basic_tags())
    testthat::expect_gt(
        file.size(output_file),   # with R functions
        file.size(output_file_2)  # without R functions
    )
    setwd(current_wd)
})
