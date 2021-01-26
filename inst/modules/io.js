readLines = function(con) {
    let is_dataURI
    is_dataURI = con.substr(0, 22) == "data:text/plain;base64"
    if (is_dataURI) {
        return(atob(con.substr(23)))
    }
    return(fetch(con).then(response => response.text()))
}
write = function(x, file) {
    let href, a
    href = URL.createObjectURL(new Blob(Array(x), { "type": "text/plain" }))
    a = dom("a", { "href": href, "download": file })
    a.click()
    return(true)
}
