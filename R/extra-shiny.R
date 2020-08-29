#' Run Shiny Application
#' @export
runApp <- function() {
    dir <- system.file("sketch_editor", package = "sketch")
    shiny::runApp(dir)
}
