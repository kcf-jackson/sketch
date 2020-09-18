#' Run 'Shiny' Application
#'
#' @examples
#' \dontrun{
#' runShinyApp()
#' }
#'
#' @export
runShinyApp <- function() {
    dir <- system.file("sketch_editor", package = "sketch")
    shiny::runApp(dir)
}
