readLines = function(con) {
    let is_dataURI = con.substr(0, 22) == "data:text/plain;base64"
    if (is_dataURI) {
        return(atob(con.substr(23)))
    }
    return(fetch(con).then(response => response.text()))
}
scan = function(f = console.log) {
    let file_loader = document.createElement("input")
    file_loader.type = "file"
    file_loader.onchange = function(event) {
        let reader = new FileReader()
        reader.onload = event => f(event.target.result)
        return(reader.readAsText(event.target.files[0], "UTF-8"))
    }
    file_loader.click()
}
write = function(x, file, type = "text/plain") {
    let a = document.createElement("a")
    a.href = URL.createObjectURL(new Blob(Array(x), { "type": type }))
    a.download = file
    a.click()
    return(true)
}
