testthat::context("Test WebSocket")

testthat::test_that("Test WebSocket server", {
    ws <- websocket$new()
    testthat::expect_length(ws$listServers(), 0)

    testthat::expect_message(ws$startServer())
    testthat::expect_length(ws$listServers(), 1)

    testthat::expect_message(ws$stopServer())
    testthat::expect_length(ws$listServers(), 0)

    ws$stopAllServers()
    testthat::expect_length(ws$listServers(), 0)
})
