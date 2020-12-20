dom = function(tag0, attr0 = {  }) {
    let domObj
    domObj = document.createElement(tag0)
    for (let key of Object.keys(attr0)) {
        domObj[key] = attr0[key]
    }
    let args = Array(...arguments), args_len = args.length
    if (args_len >= 3) {
        for (let el of args.slice(2)) {
            domObj.appendChild(el)
        }
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
    let res
    res = document.querySelector(x)
    res.appendChild(el)
    return(res)
}
append_doms = function(parent) {
    let args = Array(...arguments), args_len = args.length
    if (args_len == 1) {
        return(parent)
    }
    for (let el of args.slice(1)) {
        parent.appendChild(el)
    }
    return(parent)
}
svg = function(tag0, attr0) {
    let svgObj
    svgObj = document.createElementNS("http://www.w3.org/2000/svg", tag0)
    for (let key of Object.keys(attr0)) {
        svgObj.setAttribute(key, attr0[key])
    }
    return(svgObj)
}
