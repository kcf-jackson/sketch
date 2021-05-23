library(shiny)

myStyle <- 'width: 800px; height: 500px; font-family: monospace; font-size:20px; word-wrap: normal; white-space: pre;'
consoleStyle <- 'background: black; display: none; color: white; height: 1005px; font-family: monospace; font-size:20px; word-wrap: normal; white-space: pre; padding-left:12px;'
`%+%` <- paste0

init_example <- 'window$onload <- function() {
    textbox <- document$createElement("div")
    textbox$innerText <- "Hello World!"
    document$querySelector("body")$appendChild(textbox)
}
'

shinyUI(fluidPage(
    title = 'Sketch Playground',
    helpText(),  # just a placeholder for a little bit top margin
    fluidRow(
        # Button
        column(1, actionButton('transpile', 'Transpile', icon('toggle-right'), class= "btn-primary")),
        # Select engine
        column(1, selectInput(
            "engine", NULL, choices = c("Browser", "V8"), selected = "Browser"))
        # Select examples
        # column(1, selectInput(
        #     "example", NULL,
        #     choices = list("Example 1" = 1, "Example 2" = 2),
        #     selected = "Example 1"))
    ),
    splitLayout(
        mainPanel(
            tags$head(tags$script(src = 'shiny-handler.js')),
            # R source code
            fluidRow(
                tags$textarea(
                    id = 'src', rows = 20, style = myStyle,
                    placeholder = 'Paste your R code here...',
                    init_example
                )
            ),
            # Transpiled JS code
            fluidRow(
                tags$textarea(
                    id = 'tgt', rows = 20, style = myStyle,
                    placeholder = 'JS code will show up here...'
                )
            ),
            width = 6
        ),
        # Result panel
        mainPanel(
            # app page
            tags$iframe(id = "tgt-render", src = "", width="800px", height="1035px",
                        style="border:none; background-color:white"),
            # console
            tags$textarea(
                id = 'console', rows = 40, style = myStyle %+% consoleStyle
            ),
            width = 6
        )
    )
))
