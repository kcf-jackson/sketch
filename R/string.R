rewrite_new <- function(x) {
    pat <- "[a-zA-Z_]+[$]new"
    from <- unlist(stringr::str_extract_all(x, pattern = pat))
    if (purrr::is_empty(from)) return(x)

    rewrite <- function(x) {
        s <- unlist(strsplit(x, split = "[$]"))
        paste(rev(s), collapse = " ")
    }
    to <- purrr::map(from, rewrite)

    fun_list <- purrr::map2(from, to, function(from, to) {
        f(x, gsub(x = x, pattern = from, replacement = to, fixed = T))
    })

    freduce(x, fun_list)
}


rewrite_dot <- function(x) {
    pat <- "[a-zA-Z0-9_]+([$][a-zA-Z0-9_]+)+"
    from <- unlist(stringr::str_extract_all(x, pattern = pat))

    # Need to order `from' by length of string, because gsub may replace
    # a substring contained in a longer string and this changes the longer
    # string making the subsequent gsub ineffective
    from <- from[order(nchar(from), decreasing = T)]
    if (purrr::is_empty(from)) return(x)

    to <- purrr::map(from, f(x, gsub("[$]", ".", x)))

    fun_list <- purrr::map2(from, to, function(from, to) {
        f(x, gsub(x = x, pattern = from, replacement = to, fixed = T))
    })

    freduce(x, fun_list)
}
