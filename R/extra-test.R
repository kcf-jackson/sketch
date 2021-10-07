
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
    # load_script <- function(path) glue::glue("#! load_script(\"{path}\")")
    command <- function(x) {
        message <- list(type = "command", message = x)
        jsonlite::toJSON(message, auto_unbox = TRUE)
    }
    add_DOM_command <- function(x) {
        command(glue::glue(
            "var tmp_1IOP2A551 = document.createElement('script');
            tmp_1IOP2A551.type = 'text/javascript';
            tmp_1IOP2A551.src = new DOMParser().parseFromString('{x}', 'text/xml').firstChild.getAttribute('src');
            document.querySelector('head').appendChild(tmp_1IOP2A551);"
        ))
    }

    # main
    dir0 <- tempdir()
    app <- file.path(dir0, "app.R")
    new_script <- ifelse(has_websocket(app_script),
                         c(), load_library("websocket"))
    new_script %>%
        c(load_library("testthat")) %>%
        c(readLines(app_script)) %>%
        writeLines(con = app)

    # copy local dependencies to the temporary folder from which the app is served
    file_dependencies <- get_dependencies(app_script)
    file.copy(file_dependencies, dir0)

    in_handler <- function(msg) {  # nocov start
        msg <- jsonlite::fromJSON(msg)

        if (msg$type == "WebSocket.onopen") {
            message(msg$message)
            message("Testing App: ", appendLF = FALSE)
            # Extract dependencies of the test script and add that to the app
            test_script %>%
                assets() %>%
                `$`("head") %>%
                purrr::map(
                    ~.x %>%
                       as.character() %>%
                       add_DOM_command() %>%
                       con$ws$send()
                )
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
    return(invisible(con))
}


#' Extract the content of the `load_script` headers of a sketch R file
#'
#' @param app_script A character string; the path to the sketch R file.
#' @param local_only TRUE / FALSE; if TRUE, exclude the ones that are web link.
#'
#' @examples
#'
#' sample_file <- system.file("test_files/test_sketch.R", package = "sketch")
#' cat(readLines(sample_file), sep = "\n")  # Preview the file content
#' get_dependencies(sample_file)
#'
#' @export
# get_dependencies :: File char -> [char]
get_dependencies <- function(app_script, local_only = TRUE) {
    get_argument <- function(x) parse_expr(x)[[2]]

    headers <- app_script %>%
        assets(trace = TRUE) %>%
        `$`("head")

    keep <- headers %>%
        purrr::map_lgl(~load_script_pred(.x))

    res <- headers[keep] %>%
        purrr::map_chr(get_argument) %>%
        as.character()
    if (local_only) {
        return(res[purrr::map_lgl(res, is_local)])
    }
    res
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
