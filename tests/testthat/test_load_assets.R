context("Test asset loaders")

testthat::test_that("Test header processor", {
    headers <- c(
        "https://abc/def.js",
        "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js"
    )
    testthat::expect_equal(
        process_headers(headers),
        asset_list(
            head = Map(convert_src, headers[c(T, T)]),
            body = Map(convert_src, headers[c(F, F)])
        )
    )
})

testthat::test_that("Test source extractor", {
    testthat::expect_equal(
        extract_src("#! load_script('https://abc/def.js')"),
        "https://abc/def.js"
    )
    testthat::expect_equal(
        extract_src("#! load_library('p5')"),
        "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.9.0/p5.js"
    )
})
