#' Read a text file
#'
#' @param con A character string; either the address to the resource
#' or a dataURI string.
#'
#' @return A character vector or a promise object
#'
#' @examples
#' # Serving a file from a local server
#' x <- readLines("./file_index.txt")  # return a promise
#' x$then(print)
#'
#' # Server a file from a local directory
#' y <- readLines(dataURI("./file_index.txt", "text/plain"))  # return a string
#' print(y)
#'
#' @export
readLines <- function(con) {
    let(is_dataURI = con$substr(0, 22) == "data:text/plain;base64")
    if (is_dataURI) {
        return(atob(con$substr(23)))
    }
    # Fetch from a server (local / remote) and return a Promise
    return(fetch(con)$then(response %=>% response$text()))
}


#' Read a text file
#'
#' @description When the function is called, it will open a file
#' dialog box for user to choose the input file.
#'
#' @param f A callback function that takes the file content as input
#' and performs the desired operations. In particular, it can be a
#' function that performs a side-effect which passes the file content
#' to a variable by reference.
#'
#' @note This function must be called with user activation, e.g.
#' directly using the function the browser console.
#'
#' @export
scan <- function(f = console::log) {
    let (file_loader = document$createElement("input"))
    file_loader$type <- "file"
    file_loader$onchange <- function(event) {
        let (reader = FileReader$new())
        reader$onload = event %=>% f(event$target$result)
        return(reader$readAsText(event$target$files[0], "UTF-8"))
    }
    file_loader$click()
}


#' Write a string to a file
#'
#' @param x A character string; the input string.
#' @param file A character string; the file name.
#' @param type A character string; the dataURL type.
#'
#' @export
write <- function(x, file, type = "text/plain") {
    let (a = document$createElement("a"))
    a$href <- x %>%
        Array() %>%
        Blob$new(list(type = type)) %>%
        URL::createObjectURL()
    a$download <- file
    a$click()
    return(TRUE)
}
