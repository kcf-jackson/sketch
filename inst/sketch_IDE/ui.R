library(shiny)

myStyle <- 'width: 800px; height: 600px; font-family: monospace; font-size:20px; word-wrap: normal; white-space: pre;'
errorConsoleStyle <- 'width: 800px; height: 200px; font-family: monospace; font-size:20px; word-wrap: normal; white-space: pre;'
consoleStyle <- 'background: black; display: none; color: white; height: 1005px; font-family: monospace; font-size:20px; word-wrap: normal; white-space: pre; padding-left:12px;'
`%+%` <- paste0

init_example <- 'textbox <- document$createElement("div")
textbox$innerText <- "Hello World!"
document$querySelector("body")$appendChild(textbox)
'

shinyUI(fluidPage(
    title = 'Sketch Playground',
    helpText(),  # just a placeholder for a little bit top margin
    splitLayout(
        mainPanel(
            tags$head(
                tags$script(src = 'shiny-handler.js'),
                tags$script(src = './ace/ace.js')
            ),
            fluidRow(
                # Button
                column(1, actionButton('transpile', 'Transpile', icon('toggle-right'), class= "btn-primary"))
            ),
            helpText(),
            fluidRow(
                # R source code
                tags$div(id = 'editor', style = "width: 800px; height: 600px;")
            ),
            # Transpiled JS code
            fluidRow(tabsetPanel(
                # tabPanel("Web console",
                #          mainPanel(fluidRow(
                #              tags$textarea(
                #                  id = 'web_console', rows = 20, style = errorConsoleStyle,
                #                  placeholder = 'Web error msg'
                #              )
                #          ))),
                tabPanel("R console",
                         mainPanel(fluidRow(
                             tags$textarea(
                                 id = 'r_console', rows = 20, style = errorConsoleStyle,
                                 placeholder = 'R error message will show up here if there is one'
                             )
                         )))
            )),
            tags$script(
                'var editor = ace.edit("editor");
                    editor.setFontSize(24);
                    editor.setTheme("ace/theme/monokai");
                    editor.session.setMode("ace/mode/r");'
            ),
            width = 6
        ),
        # Result panel
        mainPanel(
            # app page
            tags$iframe(
                id = "tgt-render", src = "", width="800px", height="1035px",
                style="border:none; background-color:white"
            ),
            # console
            tags$textarea(
                id = 'console', rows = 40, style = myStyle %+% consoleStyle
            ),
            width = 6
        )
    )
))
