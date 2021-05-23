ws = new WebSocket("ws://0.0.0.0:9454")
ws.onopen = function() {
    ws.send(JSON.stringify({ "type": "WebSocket.onopen", "msg": "App connected." }))
}
ws.onmessage = function(msg) {
    eval(msg.data)
}
