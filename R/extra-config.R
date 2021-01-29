# Make a function configurable
# with_config :: char -> function -> function
# Precedence of arguments: default << config << call
with_config <- function(file_arg, f) {
    res_f <- function() {
        default_args <- as.list(formals(f))  # list of "unevaluated" symbols

        call_args <- match.call(expand.dots = FALSE)  # See Note 1 (below).
        call_args[[1]] <- as.symbol("list")
        call_args <- eval.parent(call_args)  # See Note 1.

        file <- call_args[[file_arg]]
        res_args <- default_args
        if (has_config(file)) {
            config_args <- extract_config(file)  # eager evaluation returns a list
            res_args <- merge_alist(res_args, config_args)
        }

        res_args <- merge_alist(res_args, call_args)
        do.call(f, expand_dots(res_args))
    }
    formals(res_f) <- formals(f)
    res_f
}

# Note 1: When `match.call` happens, it captures the call expression that triggers
# the current function. The actual variables associated with the symbols (in the
# expression) are outside of the current function and stored in the scope that calls
# the current function. However, `match.call` does not keep track of that environment,
# and users need to manually go up one environment level using `eval.parent`. Note
# that lexical scoping does not help. To see that, consider an example where you
# - define functions `f` and `g` in the global,
# - you call `g` within `f` with variables that exist only in `f`, and
# - within `g` you do a `match.call` and `eval` on the arguments,
# then, you will see by lexical scoping, when `g` fails to find that variable, it
# will go straight to the global scope to look for the variable (and fail). What we
# really want here is to find that variable in the caller environment `f`.
#
# Reference: https://stat.ethz.ch/pipermail/r-help/2005-July/074770.html


# has_config :: file -> bool
has_config <- function(file) {
    config_len <- length(get_config(file))
    if (config_len >= 2) {
        stop("Two configurations are detected. Please only use one.")
    }
    config_len == 1
}

# extract_config :: file -> named list
extract_config <- function(file) {
    file %>%
        get_config() %>%
        magrittr::extract2(1) %>%
        eval()
}

# get_config :: file -> [language]
get_config <- function(file) {
    file %>%
        extract_headers() %>%
        purrr::map(parse_expr) %>%
        purrr::keep(~is_call(.x, "config"))
}

# merge_alist :: list -> list -> list
# The second list overrides the first list
merge_alist <- function(fst, snd) {
    for (var in names(snd)) {
        if (var %in% names(fst)) {
            fst[[var]] <- snd[[var]]
        } else if ("..." %in% names(fst)) {
            if (deparse1(fst[["..."]]) == "") {
                fst[["..."]] <- snd[var]
            } else {
                fst[["..."]] <- append(fst[["..."]], snd[var])
            }
        } # else { ignore the extra argument }
    }
    fst
}

# expand_dots :: list -> list
expand_dots <- function(x) {
    x <- as.list(x)
    if ("..." %in% names(x)) {
        dots_list <- x[["..."]]
        x[["..."]] <- NULL
        return(c(x, dots_list))
    }
    x
}

config <- list
