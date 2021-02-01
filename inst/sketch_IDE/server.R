library(shiny)
library(htmltools)
library(sketch)
library(magrittr)

temp <- tempfile()

shinyServer(function(input, output, session) {
    # "Transpile" button
    observeEvent(input$transpile, {
        res = try({
            text_input <- input$editor_text
            # cat("Input:\n")
            # print(text_input)
            # cat(text_input, file = temp)  # defined at the top
            # print(readLines(temp))
            source_r(temp, launch_browser = NULL)
        }, silent = TRUE)
        # cat("Output:\n")
        # print(res)
        if (inherits(res, 'try-error')) {
            msg <- attr(res, "condition")$message
            message_obj <- list(status = "error", msg = msg)
        } else {
            temp_dir <- tempdir()
            dir.create(file.path(temp_dir, "www"))
            file.copy(res, file.path(temp_dir, "www/index.html"), overwrite = TRUE)
            addResourcePath("www", temp_dir)
            message_obj <- list(status = "success", msg = "./index.html")
        }
        session$sendCustomMessage('replace_textarea', message_obj)
    })
})
