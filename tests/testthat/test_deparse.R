testthat::context("Extra tests for deparser")

testthat::test_that("Reaching end of deparser_js", {
    testthat::expect_error(
        deparse_js(parse_expr("123"), list())
    )
})

testthat::test_that("Deparse dataURI", {
    path <- system.file("test_files/test_image.png", package = "sketch")
    expr <- parse_expr(glue::glue("dataURI('{path}')"))
    testthat::expect_equal(
        deparse_dataURI(expr),
        "\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAAMklEQVQYVwEnANj/AQAAAP/0QzYADHmeAAGLw0r/dNW2AJ8GngAB5Tk1/xertwADppQAEOQOdHCkRuAAAAAASUVORK5CYII=\""
    )

    path <- system.file("test_files/test_image.gif", package = "sketch")
    expr <- parse_expr(glue::glue("dataURI('{path}')"))
    testthat::expect_equal(
        deparse_dataURI(expr),
        "\"data:image/gif;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAAMklEQVQYVwEnANj/AQAAAP/0QzYADHmeAAGLw0r/dNW2AJ8GngAB5Tk1/xertwADppQAEOQOdHCkRuAAAAAASUVORK5CYII=\""
    )

    path <- system.file("test_files/test_image", package = "sketch")
    expr <- parse_expr("dataURI(path)")
    testthat::expect_error(deparse_dataURI(expr))
})

testthat::test_that("Detect mimetype", {
    testthat::expect_equal(detect_mime("file.svg"), "image/svg+xml")
    testthat::expect_equal(detect_mime("file.bmp"), "image/bmp")
    testthat::expect_equal(detect_mime("file.jpeg"), "image/jpeg")
    testthat::expect_equal(detect_mime("file.jpg"), "image/jpeg")
    testthat::expect_equal(detect_mime("file.tiff"), "image/tiff")
    testthat::expect_equal(detect_mime("file.gif"), "image/gif")
    testthat::expect_equal(detect_mime("file.png"), "image/png")
    testthat::expect_error(detect_mime("file.abc"))
})

testthat::test_that("Space symbol", {
    testthat::expect_equal(space_symbol("%x%"), " ")
    testthat::expect_equal(space_symbol("."), "")
})

