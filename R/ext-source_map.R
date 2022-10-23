# source_map :: annotated expression -> source map
# source map := [generated: {line: Int, column: Int},
#                original: {line: Int, column: Int},
#                text: {original: Char, target: Char}]

#' Create a source map (.map) file
#'
#' @param source_file A character string; the input R file.
#' @param target_file A character string; the corresponding JavaScript file.
#' @param ... Additional arguments to pass to `rewrite_annotated_exprs`.
#'
#' @note This feature is experimental.
#
# @examples
source_map_from_files <- function(source_file, target_file, ...) {
    input <- paste(readLines(source_file), collapse = "\n")
    src_maps <- annotate_exprs(input) %>%
        purrr::map(rewrite_annotated_exprs, ...) %>%
        purrr::map(source_map) %>%
        # Sanitise the source map
        purrr::map(function(src_map) {
            for (i in seq_along(src_map)) {
                src_map[[i]]$text <- NULL
                src_map[[i]]$source <- source_file
            }
            src_map
        })

    # Convert the source map into a JSON string
    src_map_json <- src_maps %>%
        unlist(recursive = FALSE) %>%
        jsonlite::toJSON(auto_unbox = TRUE)

    # Convert the source map JSON string into the '.map' string
    ctx <- V8::v8()
    ctx$source(system.file("assets/source_map.js", package = "sketch"))
    res <- ctx$eval(glue::glue(
        "sourceMap.fromJSON('{target_file}', JSON.parse('{src_map_json}'))"
    ))
    temp_file <- file.path(tempdir(),
                           gsub(basename(source_file),
                                pattern = "[.]R$",
                                replacement = ".map",
                                ignore.case = TRUE))
    cat(res, file = temp_file)
    invisible(temp_file)
}


#' Convert a compiled AST into a source map
#'
#' @param ast The compiled AST. The JavaScript AST compiled from the R AST.
#'
#' @return A (list of) source map.
#'
#' @note This feature is experimental.
#
# @export
#
# @examples
source_map <- function(ast) {
    envir <- new.env()
    envir$res <- list()
    envir$current_pos <- 1

    record <- function(val) envir$res[[length(envir$res) + 1]] <- val
    loc <- function(lloc, cloc) list(line = lloc, column = cloc)

    create_source_map <- function(ast) {
        if (!is.list(ast)) {
            envir$current_pos <- envir$current_pos + nchar(ast)
            return(NULL)
        }
        record(
            list(generated = loc(1, envir$current_pos),
                 original = loc(attr(ast, "line1"),
                                attr(ast, "col1")),
                 text = list(original = attr(ast, "text2"),
                             target = deparse_js_ast(ast)))
        )
        purrr::walk(ast, create_source_map)
    }
    create_source_map(ast)  # side effect
    envir$res
}

#' Deparse a compiled AST
#'
#' @param ast The compiled AST. The JavaScript AST compiled from the R AST.
#'
#' @return A character string. The compiled string.
#'
#' @note This feature is experimental.
#
# @export
#
# @examples
deparse_js_ast <- function(ast) {
    if (!is.list(ast)) return(ast)
    ast %>%
        purrr::map_chr(deparse_js_ast) %>%
        paste(collapse = "")
}

#' Display the source map in a table
#'
#' @param x A source map. The output from `source_map`.
#'
#' @return  A data frame.
#'
#' @note This feature is experimental.
#
# @export
source_map_table <- function(x) {
    res <- x %>%
        purrr::map(
            ~list(
                original_line = .x$original$line,
                original_column = .x$original$column,
                original_text = .x$text$original,
                original_length = nchar(.x$text$original),
                generated_line = .x$generated$line,
                generated_column = .x$generated$column,
                generated_text = .x$text$target,
                generated_length = nchar(.x$text$target)
            )
        )
    as.data.frame(do.call(rbind, res))
}

#' Verify a source map
#'
#' @param ast The compiled AST. The JavaScript AST compiled from the R AST.
#' @param src_map The source map. The output from `source_map`.
#'
#' @return A data frame; a source map table expanded by the 'pass_test' column.
#'
#' @note This feature is experimental.
verify_source_map <- function(ast, src_map) {
    output_str <- deparse_js_ast(ast)
    src_map_table <- source_map_table(src_map)
    test <- c()
    for (i in 1:nrow(src_map_table)) {
        cloc <- src_map_table$generated_column[[i]]
        clen <- src_map_table$generated_length[[i]]
        ctext <- src_map_table$generated_text[[i]]
        pass <- substring(output_str, cloc, cloc + clen - 1) == ctext
        test <- c(test, pass)
    }
    src_map_table$pass_test <- test
    src_map_table
}
