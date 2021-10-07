testthat::context("Test predicate functions for file extensions")

testthat::test_that("Test predicates", {
    testthat::expect_true(is_web_link("https://unimelb.edu.au/"))
    testthat::expect_true(is_web_link("http://unimelb.edu.au/"))
    testthat::expect_false(is_web_link("image.png"))

    testthat::expect_false(is_local("http://unimelb.edu.au/"))
    testthat::expect_true(is_local("image.png"))

    testthat::expect_true(always_true())
    testthat::expect_true(always_true(123))

    testthat::expect_true(is_javascript("file.js"))
    testthat::expect_true(is_javascript("file.JS"))
    testthat::expect_true(is_javascript("https://unimelb.edu.au/file.js"))
    testthat::expect_false(is_javascript("file.R"))

    testthat::expect_true(is_r_script("file.r"))
    testthat::expect_true(is_r_script("https://unimelb.edu.au/file.R"))
    testthat::expect_false(is_r_script("file.JS"))

    testthat::expect_true(is_css("file.css"))
    testthat::expect_true(is_css("file.CSS"))
    testthat::expect_true(is_css("https://unimelb.edu.au/file.CsS"))
    testthat::expect_false(is_css("file.R"))

    testthat::expect_true(is_json("file.json"))
    testthat::expect_true(is_json("file.JsOn"))
    testthat::expect_true(is_json("https://unimelb.edu.au/file.JSON"))
    testthat::expect_false(is_json("file.R"))

    testthat::expect_true(is_csv("file.csv"))
    testthat::expect_true(is_csv("https://unimelb.edu.au/file.CSV"))
    testthat::expect_false(is_csv("file.R"))

    testthat::expect_true(is_font("file.woff"))
    testthat::expect_true(is_font("file.TTF"))
    testthat::expect_true(is_font("file.EOT"))
    testthat::expect_true(is_font("file.OTF"))
    testthat::expect_true(is_font("https://unimelb.edu.au/file.WOFF"))
    testthat::expect_false(is_font("file.R"))
})
