#| config(rules = basic_rules(), deparsers = default_deparsers())

map <- function(x, f) {
    return(x$map(f))
}

reduce <- function(x, f) {
    return(x$reduce(f))
}

reduce_right <- function(x, f) {
    return(x$reduceRight(f))
}

filter <- function(x, f) {
    return(x$filter(f))
}

walk <- function(x, f) {
    return(x$forEach(walk))
}

all <- function(x, f) {
    return(x$every(f))
}

any <- function(x, f) {
    return(x$some(f))
}
