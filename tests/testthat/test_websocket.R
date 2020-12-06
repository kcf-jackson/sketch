testthat::context("Test WebSocket")

testthat::test_that("Test WebSocket server", {
    ws <- websocket$new()
    testthat::expect_length(ws$listServers(), 0)

    # test: start server
    testthat::expect_message(ws$startServer())
    testthat::expect_length(ws$listServers(), 1)
    # test: cannot start server twice
    testthat::expect_message(ws$startServer(), "existing server")

    # test: stop server
    testthat::expect_message(ws$stopServer())
    testthat::expect_length(ws$listServers(), 0)
    # test: cannot stop server twice
    testthat::expect_message(ws$stopServer(), "no server")

    # test sketch_mode
    testthat::expect_message(ws$sketch_mode(), "No server")

    ws$stopAllServers()
    testthat::expect_length(ws$listServers(), 0)
})
