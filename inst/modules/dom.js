dom = function(tag0, attr0) {
    domObj = document.createElement(tag0)
    for (let key of Object.keys(attr0)) {
        domObj[key] = attr0[key]
    }
    return(domObj)
}
select_dom = function(x) {
    return(document.querySelector(x))
}
select_doms = function(x) {
    return(document.querySelectorAll(x))
}
print_dom = function(el, x = "body") {
    res = document.querySelector(x)
    res.appendChild(el)
    return(res)
}
svg = function(tag0, attr0) {
    svgObj = document.createElementNS("http://www.w3.org/2000/svg", tag0)
    for (let key of Object.keys(attr0)) {
        svgObj.setAttribute(key, attr0[key])
    }
    return(svgObj)
}
