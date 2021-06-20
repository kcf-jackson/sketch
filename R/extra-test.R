
#' Test a sketch application
#'
#' @param app_script A character string; the file path to the app.
#' @param test_script A character string; the file path to the tests.
#' @param port An integer to pass to `websocket$new()`.
#' @param ... Additional arguments to pass to \link{source_r}.
#'
#' @return A "websocket" object.
#' @export
#'
#' @examples
#' \dontrun{
#' app_file <- system.file("test_files/test_testthat_app.R", package = "sketch")
#' test_file <- system.file("test_files/test_testthat_test.R", package = "sketch")
#' # This following command will launch the default browser
#' res <- test_sketch(app_file, test_file)
#' }
test_sketch <- function(app_script, test_script, port = 9454, ...) {
    # helpers
    load_library <- function(path) glue::glue("#! load_library(\"{path}\")")
    load_script <- function(path) glue::glue("#! load_script(\"{path}\")")
    command <- function(x) {
        message <- list(type = "command", message = x)
        jsonlite::toJSON(message, auto_unbox = TRUE)
    }

    # main
    app <- tempfile(fileext = ".R")
    new_script <- ifelse(has_websocket(app_script),
                         c(), load_library("websocket"))
    new_script %>%
        c(load_library("testthat")) %>%
        c(load_script(app_script)) %>%
        writeLines(con = app)

    in_handler <- function(msg) {  # nocov start
        msg <- jsonlite::fromJSON(msg)

        if (msg$type == "WebSocket.onopen") {
            message(msg$message)
            message("Testing App: ", appendLF = FALSE)
            # Compile and send test script over to App
            test_script %>%
                compile_r(output = tempfile(fileext = ".js")) %>%
                readLines() %>%
                paste(collapse = "\n") %>%
                command() %>%
                con$ws$send()
            # On completion, request App to send the test report to R
            con$ws$send(command(
                "ws.send(JSON.stringify({
                    \"type\": \"testthat\",
                    \"message\": testthat.report()
                }))"
            ))
        }

        if (msg$type == "testthat") {
            report <- msg$message
            message(glue::glue("[ {yellow('FAIL')} {report$fail} | {green('PASS')} {report$pass} ]"))
            con$env$result <- msg$message
            con$stopServer()
        }
    }  # nocov end

    # Launch WebSocket testing server and then the user app
    con <- websocket$new(in_handler = in_handler, port = port)
    con$startServer()
    source_r(app, ...)
    return(con)
}

# has_websocket :: File char -> logical
has_websocket <- function(app_script) {
    app_script %>%
        assets(trace = TRUE) %>%
        `$`("head") %>%
        purrr::map_lgl(is_websocket) %>%
        any()
}

# is_websocket :: char -> logical
is_websocket <- function(x) {
    expr <- parse_expr(x)
    is_call(expr, "load_library") &&
        rlang::is_syntactic_literal(expr[[2]]) &&
        (expr[[2]] == "websocket")
}
