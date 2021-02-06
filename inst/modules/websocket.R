#! config(rules = basic_rules(), deparsers = default_deparsers())
ws <- WebSocket$new("ws://0.0.0.0:9454")

ws$onopen <- function() {
    ws$send(JSON::stringify(
        list(
            type = "WebSocket.onopen",
            message = "App connected."
        )
    ))
}

# Dispatch by message type
handler <- function(x, y) {
    return(list(type_predicate = x, type_function = y))
}

end_message <- function(x) {
    console::log("The dispatch function reaches the end.")
}

ws$dispatch <- Array(
    handler(type %=>% type == "text", console::log),
    handler(type %=>% type == "command", eval),
    handler(type %=>% TRUE, end_message)
)

ws$onmessage <- function(msg) {
    let (data = JSON::parse(msg$data),
         type = data$type,
         message = data$message)

    for (proc in ws$dispatch) {
        if (proc$type_predicate(type)) {
            return(proc$type_function(message))
        }
    }
}
