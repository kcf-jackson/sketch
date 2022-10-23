#' Parse and annotate expressions
#'
#' @description Parse and annotate expressions with lines and columns tracking.
#' @param x A character string; the input to parse.
#
# @examples
# x <- annotate_exprs("a <- f(g(x), g(x)); b <- 2;")
# x[[1]]   # first statement
# x[[1]][[3]][[2]]   # first g(x)
# x[[1]][[3]][[3]]   # second g(x)
annotate_exprs <- function(x) {
    deparse2 <- \(x) deparse1(x, collapse = "\n")
    normalise_expr <- \(x) deparse2(parse(text = x, keep.source = FALSE)[[1]])

    # Extend the getParseData by two columns 'used' and 'text2'.
    # The column 'used' is for resolving the position of an expression provided
    # in the text form, and the column 'text2' is the normalised form of the 'text'
    # column.
    getParseTable <- function(px) {
        parse_table <- getParseData(px, includeText = TRUE)
        parse_table <- parse_table[!parse_table$terminal, ]
        parse_table$used <- FALSE
        parse_table$text2 <- parse_table$text %>%  # Normalised text
            purrr::map_chr(normalise_expr)
        parse_table
    }

    # Add line and column location as attributes to an expression
    add_annotation <- function(expr) {
        # Since attributes cannot be set on a symbol, only calls have
        # location information attached to them.
        if (rlang::is_call(expr)) {
            expr <- as.call(purrr::map(expr, add_annotation))
            attributes(expr) <- get_location(expr)
            attr(expr, "used") <- NULL
        }
        return(expr)
    }

    # Get the location of a sub-expression by looking up the 'text2' column;
    # this uses a variable 'used' to resolve the position when the sub-expression
    # has multiple matches in the parse table.
    get_location <- function(expr) {
        df0 <- envir$parse_table
        ind <- min(which(df0$text2 == deparse2(expr) & !df0$used))
        envir$parse_table$used[ind] <- TRUE
        return(df0[ind, ])
    }

    # Main
    px <- parse(text = x, keep.source = TRUE)
    envir <- new.env()
    envir$parse_table <- getParseTable(px)
    purrr::map(px, add_annotation)
}
