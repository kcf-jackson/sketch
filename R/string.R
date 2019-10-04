split_str <- function(x, split) {
    unlist(strsplit(x, split))
}

rewrite_KW <- function(x, pat, rewrite) {
    from <- unlist(stringr::str_extract_all(x, pattern = pat))
    if (purrr::is_empty(from)) return(x)
    to <- purrr::map(from, rewrite)

    fun_list <- purrr::map2(from, to, function(from, to) {
        f(x, gsub(x = x, pattern = from, replacement = to, fixed = T))
    })
    freduce(x, fun_list)
}


rewrite_new <- function(x) {
    pattern <- "[a-zA-Z0-9_]+[$]new"
    rewrite <- function(x) {
        x %>%
            split_str(split = "[$]") %>%
            rev() %>% paste(collapse = " ")
    }
    rewrite_KW(x, pattern, rewrite)
}

rewrite_dot <- function(x) {
    pattern <- "[a-zA-Z0-9_]+([$][a-zA-Z0-9_]+)+"
    rewrite <- f(x, gsub("[$]", ".", x))
    rewrite_KW(x, pattern, rewrite)
}

rewrite_for_loop <- function(x) {
    pattern <- "[(][a-zA-Z0-9_]+ in [^{]+"
    rewrite <- f(x, gsub("in", "of", x))
    rewrite_KW(x, pattern, rewrite)
}
