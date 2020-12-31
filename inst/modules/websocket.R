ws <- WebSocket$new("ws://0.0.0.0:9454")

ws$onopen <- function() {
    ws$send(JSON.stringify("Browser requesting connection..."))
}

ws$onmessage <- function(msg) {
    eval(msg$data)
}
