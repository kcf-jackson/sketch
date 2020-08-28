testthat::context("Test asset loaders")

testthat::test_that("Convert asset to shiny.tag", {
    # Load library
    testthat::expect_identical(
        convert_src("load_library('p5')"),
        htmltools::tags$script(src = src("p5"))
    )

    # Load data
    path <- system.file("test_files/test_json.json", package = "sketch")
    x <- tempfile()
    if (.Platform$OS.type == 'windows') {
        x <- normalizePath(x, winslash = "/")
    }
    testthat::expect_identical(
        convert_src(glue::glue("load_data('{path}', cache = '{x}')")),
        htmltools::tags$script(src = dataURI(file = x, mime = "text/javascript"))
    )

    testthat::expect_error(
        convert_src("load_UNDEFINED('p5')")
    )

    # Load script
    # Web
    testthat::expect_identical(
        convert_src("load_script(src('p5'))"),
        htmltools::tags$script(src = src("p5"))
    )

    testthat::expect_identical(
        convert_src("load_script('https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css', integrity='sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk', crossorigin='anonymous')"),
        htmltools::tags$link(
            href = 'https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css',
            rel = "stylesheet",
            integrity = "sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk",
            crossorigin = "anonymous"
        )
    )

    testthat::expect_identical(
        to_shiny_tag("https://fonts.eot"),
        htmltools::tags$link(href = "https://fonts.eot", rel = "preload")
    )

    # Local
    path <- system.file("test_files/test_js.js", package = "sketch")
    testthat::expect_identical(
        convert_src(glue::glue("load_script('{path}')")),
        htmltools::tags$script(src = dataURI(file = path, mime = "text/javascript"))
    )

    path <- system.file("test_files/test_styles.css", package = "sketch")
    content <- paste(readLines(path), collapse = "\n")
    testthat::expect_identical(
        convert_src(glue::glue("load_script('{path}')")),
        htmltools::tags$style(content, rel = "stylesheet")
    )

    path <- system.file("test_files/test_image.png", package = "sketch")
    testthat::expect_error(
        convert_src(glue::glue("load_script('{path}')"))
    )

    testthat::skip_on_os("windows")
    # The following test is skipped on Windows because I think
    # `dataURI` produces different results when '\r\n' is used in
    # place of '\n'.
    #
    # Online dataURI decoder shows that the Windows result from
    # CRAN win-builder (in plain-text) looks the same as the Unix
    # result, but they differ by 7 bytes. I believe the difference
    # corresponds to the number of newlines in the file.
    #
    # PR on this issue is welcomed.
    path <- system.file("test_files/test_sketch_2.R", package = "sketch")
    testthat::expect_identical(
        convert_src(glue::glue("load_script('{path}')")),
        htmltools::tags$script(src = "data:text/javascript;base64,ZmliID0gZnVuY3Rpb24obikgewogICAgaWYgKFIuTFQobiwgMikpIHsKICAgICAgICByZXR1cm4obikKICAgIH0gZWxzZSB7CiAgICAgICAgcmV0dXJuKFIuYWRkKGZpYihSLnN1YnRyYWN0KG4sIDEpKSwgZmliKFIuc3VidHJhY3QobiwgMikpKSkKICAgIH0KfQo=")
    )
})

testthat::test_that("Compile data with caching", {
    path <- system.file("test_files/test_json.json", package = "sketch")
    x <- tempfile()
    y <- compile_data(path, x)
    testthat::expect_equal(x, y)
    testthat::expect_message(compile_data(path, x))
})

testthat::test_that("Convert data to JSON", {
    csv <- system.file("test_files/test_csv.csv", package = "sketch")
    testthat::expect_equal(
        to_json(csv),
        "const test_csv_csv = R.data_frame(JSON.parse('{\"x\":[1,2],\"y\":[2,4]}'))"
    )

    tsv <- system.file("test_files/test_tsv.tsv", package = "sketch")
    testthat::expect_error(to_json(tsv))
    testthat::expect_error(to_json(tsv, as_data_frame = T))
})
