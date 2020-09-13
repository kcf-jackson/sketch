library(shiny)
library(htmltools)
library(sketch)
library(magrittr)

file <- tempfile()
R_web <- system.file('assets/browser-R_core.js', package = "sketch")

shiny_msg <- function(tgt, transpiled, rendered, path) {
    list(tgt = tgt, transpiled = transpiled, rendered = rendered, path = path)
}

# example_1 <- example_2 <- ""
# example_1 <- paste(readLines("eg_1.R"), collapse = "\n")
# example_2 <- paste(readLines("eg_2.R"), collapse = "\n")
# example_3 <- paste(readLines("eg_3.R"), collapse = "\n")

shinyServer(function(input, output, session) {
    # Switch engine
    observeEvent(input$engine, {
        session$sendCustomMessage('toggle_engine', "")  # empty message
    }, ignoreInit = TRUE)

    # # Switch example
    # observeEvent(input$example, {
    #     eg <- input$example
    #     # print(eg)
    #     eg_engine_mismatch <- (
    #         (eg == 1) && (input$engine == "V8")) || (
    #         (eg == 2) && (input$engine == "Browser"))
    #
    #     if (eg_engine_mismatch) {
    #         session$sendCustomMessage('toggle_engine', "")  # empty message
    #     }
    #
    #     msg <- switch(eg,'1' = example_1, '2' = example_2)
    #     session$sendCustomMessage('switch_example', msg)
    # })

    # "Transpile" button
    observeEvent(input$transpile, {
        res = try({
            text_input <- input$src
            # cat("Input:\n")
            # print(text_input)
            compile_exprs(text_input)
        }, silent = TRUE)
        # cat("Output:\n")
        # print(res)
        if (inherits(res, 'try-error')) {
            msg <- attr(res, "condition")$message
            message_obj <- shiny_msg(input$engine, msg, msg, "")
        } else {
            if (input$engine == "Browser") {
                msg <- paste(res, collapse = "\n")
                # print(msg)
                path <- html_print(tags$script(msg))
                temp_dir <- tempdir()
                dir.create(file.path(temp_dir, "www"))
                file.copy(path, file.path(temp_dir, "www/index.html"), overwrite = TRUE)
                addResourcePath("www", temp_dir)
                message_obj <- shiny_msg(input$engine, msg, msg, "./index.html")
            } else { # input$engine == "V8"
                ct <- V8::v8()
                ct$source(R_web)
                # print(res)
                msg <- res %>%
                    purrr::map_chr(~paste(">", .x, "\n", ct$eval(.x, serialize = TRUE), "\n")) %>%
                    paste(collapse = "\n")
                message_obj <- shiny_msg(input$engine, paste(res, collapse = "\n"), msg, "")
            }
        }
        session$sendCustomMessage('replace_textarea', message_obj)
    })
})