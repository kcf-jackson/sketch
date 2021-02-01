#' Websocket for 'sketch' applications
#'
#' @description This combines the *-Server family of functions in 'httpuv'
#' with the transpilation functionality provided by 'sketch'.
#'
#' @export
websocket <- R6::R6Class("websocket", public = list(
    #' @field app A list of functions that define the application.
    app = NULL,

    #' @field server A server handle to be used by 'stopServer'.
    server = NULL,

    #' @field log A character vector that keep tracks of all the commands
    #' sent to the browser session.
    log = c(),

    #' @field ws A WebSocket channel to handle the communication between
    #' the R session and the browser session.
    ws = NULL,

    #' @field in_handler A function to handle instructions sent by the
    #' browser session.
    in_handler = NULL,

    #' @field out_handler A function to handle instruction sent to the
    #' browser session.
    out_handler = NULL,

    #' @field env An environment to store variables temporarily.
    env = new.env(),

    #' @field port An integer; the TCP port number.
    port = 9454,

    #' @field message TRUE or FALSE; whether to display a prompt when
    #' a server is started and when it is stopped.
    message = TRUE,

    #' @field connected TRUE or FALSE; whether a connection has been established.
    #' One should ways start the WebSocket server before visiting the web page
    #' that connects to the server.
    connected = FALSE,

    #' @field started TRUE or FALSE; whether a server has been started. Use
    #' the \code{startServer} method to start a server.
    started = FALSE,

    #' @description Start a WebSocket server
    startServer = function() {
        if (self$started) {
            message("There is an existing server running.")
            return(invisible(NULL))
        }
        # Start the server if not already started
        self$server <- httpuv::startServer("0.0.0.0", self$port, self$app, 250)
        if (self$message) message("Server started.")
        self$started <- TRUE
    },

    #' @description Stop a WebSocket server
    stopServer = function() {
        if (!self$started) {
            message("There is no server running.")
            return(invisible(NULL))
        }
        # Stop the server if not already stopped
        httpuv::stopServer(self$server)
        if (self$message) message("Server stopped.")
        self$started <- FALSE
        self$connected <- FALSE
    },

    #' @description List all running WebSocket servers
    listServers = function() {
        res <- httpuv::listServers()
        message(glue::glue("{length(res)} server(s) is/are running."))
        return(res)
    },

    #' @description Stop all running WebSocket servers
    stopAllServers = function() {
        httpuv::stopAllServers()
        self$started <- FALSE
        self$connected <- FALSE
        self$listServers()
    },

    #' @description Enter sketch mode, in which all commands go through
    #' the transpiler before reaching the browser session.
    sketch_mode = function() {
        if (!self$started) {
            message("No server is running.")
            return(invisible(NULL))
        }

        if (!self$connected) {   # nocov start
            message("No connection has been established.")
            return(invisible(NULL))
        }

        cat("(Type `q()` to exit sketch mode)")
        while (TRUE) {
            input <- read_multilines("sketch > ")
            self$log <- c(self$log, input)
            if (deparse(parse_expr(input)) == "q()") break
            purrr::walk(self$out_handler(input), ~self$ws$send(.x))
        }    # nocov end
    },

    #' @description Create a blank HTML page with interactive access.
    #' This function is designed for newcomers.
    #'
    #' @param preamble (Optional) A named list; the preamble to include.
    #' Use the name 'lib' for arguments to \code{load_library}, 'script'
    #' for arguments to \code{load_script} and 'data' for arguments to
    #' \code{load_data}. Note that the "dom" and "websocket" modules are
    #' required and loaded by default.
    #' @param ... Extra parameters to pass to \link{source_r}.
    #'
    #' @return The (invisible) temporary file path to the app.
    new_app = function(preamble = list(library = c(), script = c(), data = c()), ...) {  # nocov start
          preamble_to_string <- function(preamble) {
              list(names(preamble), preamble) %>%
                  purrr::pmap(~glue::glue('#! load_{..1}("{..2}")')) %>%
                  unlist()
          }

          temp_file <- tempfile(fileext = ".R")
          preamble$library <- unique(c("dom", "websocket", preamble$library))
          preamble %>%
              preamble_to_string() %>%
              writeLines(temp_file)
          source_r(temp_file, ...)
    },  # nocov end

    #' @description Initialise a WebSocket connection
    #'
    #' @param in_handler A function to handle incoming message, default to
    #' be \link[base:print]{print} which only displays the message without
    #' any processing.
    #' @param out_handler A function to handle outgoing message, default to
    #' be \link{compile_exprs} which transpiles R commands into JavaScript
    #' commands.
    #' @param message TRUE or FALSE; whether to display a prompt when
    #' a server is started and when it is stopped.
    #' @param port An integer; the TCP port number.
    #'
    #' @return A 'websocket' object.
    #'
    #' @examples
    #' \dontrun{
    #' # Launch a WebSocket server
    #' ws <- websocket$new()
    #' ws$startServer()
    #' ws$listServers()    # Check that a server is running
    #'
    #' # Launch a 'sketch' application with WebSocket functionality
    #' file <- system.file("test_files/test_websocket.R", package = "sketch")
    #' source_r(file, debug = TRUE)   # Launch the default browser
    #'
    #' # Enter sketch mode to send commands to the application
    #' ws$sketch_mode()
    #' # Within sketch mode
    #' print("1234")
    #' x <- 10
    #' print(x + 1)
    #' q()
    #'
    #' # Back to normal mode, inspect the log and stop the server
    #' ws$log
    #' ws$stopServer()
    #' ws$listServers()    # Confirm no server is running
    #' }
    initialize = function(in_handler = function(x) cat(x$msg, "\n"),
                          out_handler = sketch::compile_exprs,
                          message = TRUE, port = 9454) {
        self$app <- list(
            call = function(req) {  # nocov start
                if (message) message("Received http request.")
                list(
                    status = 200L,
                    headers = list("Content-Type" = "text/html"),
                    body = "Server is running."
                )
            },
            onWSOpen = function(ws) {
                self$ws <- ws
                self$connected <- TRUE
                ws$send("console.log(\"Connection established.\")")
                ws$onMessage(function(binary, input) {
                    in_handler(input)
                })
            }  # nocov end
        )
        self$in_handler <- in_handler
        self$out_handler <- out_handler
        self$message <- message
        self$port <- port
    }
))


#' Read one or more lines from the Terminal
#'
#' @description \code{read_multilines} reads one or more lines from
#' the terminal (in interactive use).
#'
#' @inheritParams base::readline
#'
#' @details
#' This function repeatedly calls \code{readline} until enough inputs
#' are provided to reach a successful parse.
#'
#' This can only be used in an interactive session.
#'
#' @export
read_multilines <- function(prompt) {  # nocov start
    success_parse <- function(x) {
        is.null(purrr::safely(parse_expr)(x)$error)
    }
    # Main
    res <- readline(prompt = prompt)
    while (!success_parse(res)) {
        res <- paste(res, "\n", readline(prompt = ""))
    }
    return(res)
}  # nocov end
