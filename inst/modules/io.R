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
    declare (is_dataURI)
    is_dataURI <- con$substr(0, 22) == "data:text/plain;base64"
    if (is_dataURI) {
        return(atob(con$substr(23)))
    }
    # Fetch from a server (local / remote) and return a Promise
    return(fetch(con)$then(response %=>% response$text()))
}


#' Read a text file
read <- function() {

}


#' Write a string to a file
#'
#' @param x A character string; the input string.
#' @param file A character string; the file name.
#'
#' @export
write <- function(x, file) {
    declare (href, a)
    href <- x %>%
        Array() %>%
        Blob$new(list(type = "text/plain")) %>%
        URL$createObjectURL()
    a <- dom("a", list(href = href, download = file))
    a$click()
    return(TRUE)
}
