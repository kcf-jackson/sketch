ws = new WebSocket("ws://0.0.0.0:9454")
ws.onopen = function() {
    ws.send(JSON.stringify({ "type": "WebSocket.onopen", "message": "App connected." }))
}
handler = function(x, y) {
    return({ "type_predicate": x, "type_function": y })
}
end_message = function(x) {
    console.log("The dispatch function reaches the end.")
}
ws.dispatch = Array(handler(type => type == "text", console.log), handler(type => type == "command", eval), handler(type => true, end_message))
ws.onmessage = function(msg) {
    let data = JSON.parse(msg.data), type = data.type, message = data.message
    for (let proc of ws.dispatch) {
        if (proc.type_predicate(type)) {
            return(proc.type_function(message))
        }
    }
}
