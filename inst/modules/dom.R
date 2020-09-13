dom <- function(tag0, attr0) {
    domObj <- document$createElement(tag0)
    for (key in Object$keys(attr0)) {
        domObj[key] <- attr0[key]
    }
    return(domObj)
}

print_dom <- function(el) {
    document$querySelector("body")$appendChild(el)
}
