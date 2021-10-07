#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*") # testing
    # res$setHeader("Access-Control-Allow-Origin", "https://kcf-jackson.github.io/")
    plumber::forward()
}


#* Transpile an R string into a JavaScript string
#* @param x A encoded JSON string. It must have an 'input' attribute containing an R string to be transpiled to JavaScript and optional attributes 'rules' and 'deparsers' for transpiler.
#* @get /json
function(x, res) {
    is_valid <- function(x) is.null(x$error)
    safe_parse <- purrr::safely(rlang::parse_exprs)

    # x is a JSON string
    x_list <- jsonlite::fromJSON(httpuv::decodeURIComponent(x))
    result <- safe_parse(x_list$input)

    # If the input is not valid, return the error message
    if (!is_valid(result)) {
        dom <- shiny::div(as.character(result$error))
        file <- htmltools::html_print(dom, viewer = NULL)
        on.exit(unlink(file))
        return(plumber::include_html(file, res))
    }

    # If the input is valid, write to temporary file for transpilation
    temp <- tempfile(fileext = ".R")
    writeLines(x_list$input, con = temp)
    on.exit(unlink(temp))

    # Configure the transpiler
    if (isTRUE(x_list$rules == "basic")) {
        rules <- sketch::basic_rules()
    } else {
        rules <- sketch::default_rules()
    }

    if (isTRUE(x_list$deparsers == "basic")) {
        deparsers <- sketch::basic_deparsers()
    } else {
        deparsers <- sketch::default_deparsers()
    }

    # Begin the transpilation
    y <- sketch::source_r(
        file = temp, launch_browser = NULL,
        rules = rules, deparsers = deparsers
    )
    on.exit(unlink(y))
    return(plumber::include_html(y, res))
}
