testthat::context("Test the rewriting and deparsing modules")

# Helper functions
green <- function(x) paste0("\033[32m", x, "\033[39m")
red <- function(x) paste0("\033[31m", x, "\033[39m")
color <- function(pred) if (pred) green(pred) else red(pred)
test_equal <- function(f, input, expected, silent = TRUE) {
    if (!silent) {
        cat("==========Test==========", "\n")
        cat("Input    : ", input, "\n")
        cat("Output   : ", f(input), "\n")
        cat("Expected : ", expected, "\n")
        cat("Passed   : ", color(f(input) == expected), "\n")
    }
    testthat::expect_equal(f(input), expected)
}


# Test expressions transpilation (`compile_exprs`)
testthat::test_that("Test transpilation with basic rules and deparsers (exprs)", {
    basic <- purrr::partial(compile_exprs, rules = basic_rules(), deparsers = basic_deparsers())
    unit_test <- purrr::partial(test_equal, f = basic)

    # Test variable assignment
    unit_test("a <- x <<- 10", "a = x = 10")
    unit_test("a <<- b <- 3 + 4", "a = b = 3 + 4")

    # Test arithmetic
    unit_test("3^2 + 14", "3 ** 2 + 14")
    unit_test("3 * pi - 3", "3 * pi - 3")
    unit_test("-3", "-3")
    unit_test("2 - 3", "2 - 3")
    unit_test("-3 + 4", "-3 + 4")
    unit_test("-(2 + 3) - 4", "-(2 + 3) - 4")
    unit_test("123 %% 5 == 4", "123 % 5 == 4")

    # Test subsetting / extraction
    unit_test("self$abc(123)", "self.abc(123)")
    unit_test("obj_1$method_1(x)", "obj_1.method_1(x)")
    unit_test("obj_1$attr_1$x", "obj_1.attr_1.x")
    unit_test("obj_1$attr_1$f(1 + 2)", "obj_1.attr_1.f(1 + 2)")
    unit_test("(a + b)$x", "(a + b).x")
    unit_test("abc$abc[0]", "abc.abc[0]")
    unit_test("abc$abc[[0]]", "abc.abc[[0]]")

    # Test variable declaration
    unit_test("let (x)", "let x")
    unit_test("let (x = 3)", "let x = 3")
    unit_test("const (y = 4)", "const y = 4")

    # Test control flow
    unit_test("if (TRUE) f(x)", "if (true) f(x)")
    unit_test("if (TRUE) f(x) else g(x)", "if (true) f(x) else g(x)")
    unit_test("for (i in iterables) { x }", "for (let i of iterables) {\n    x\n}")
    unit_test("while (TRUE) { do(x) }", "while (true) {\n    do(x)\n}")
    unit_test("function(b, c) {}", "function(b, c) {\n    \n}")

    # Test that function arguments are rewritten
    unit_test("function(b = TRUE, c = FALSE) {}", "function(b = true, c = false) {\n    \n}")
    unit_test("function(n = 2 ^ 4) {}", "function(n = 2 ** 4) {\n    \n}")
    unit_test("function(n = f(3 + g(2 ^ 4))) {}", "function(n = f(3 + g(2 ** 4))) {\n    \n}")
    unit_test("function(n = 3 ^ 2 ^ 2) {}", "function(n = 3 ** 2 ** 2) {\n    \n}")

    # Test error handling
    unit_test("try(1 + 1)", "try {\n    1 + 1\n} catch(error) {\n    console.log(error)\n}")
    unit_test("try({1 + 2; 3 + 4})", "try {\n    1 + 2\n    3 + 4\n} catch(error) {\n    console.log(error)\n}")
    unit_test("stop(\"ERROR_MSG\")", "throw new Error(\"ERROR_MSG\")")
})

testthat::test_that("Test transpilation with default rules and deparsers (exprs)", {
    default <- purrr::partial(compile_exprs, rules = default_rules(), deparsers = default_deparsers())
    unit_test <- purrr::partial(test_equal, f = default)

    # Test assignment
    unit_test("a <- x <<- 10", "a = x = 10")
    unit_test("a <<- b <- 3 + 4", "a = b = R.add(3, 4)")

    # Test arithmetic
    unit_test("3^2 + 14", "R.add(R.pow(3, 2), 14)")
    unit_test("1:10 + 120", "R.add(R.seq(1, 10), 120)")
    unit_test("\":\"", "\":\"")
    unit_test("3 * pi - 3", "R.subtract(R.multiply(3, R.pi), 3)")
    unit_test("+103", "R.unaryPlus(103)")
    unit_test("-3", "-3")
    unit_test("2 - 3", "R.subtract(2, 3)")
    unit_test("-3 + 4", "R.add(-3, 4)")
    unit_test("-2 + 3 - 4", "R.subtract(R.add(-2, 3), 4)")
    unit_test("-(2 + 3) - 4", "R.subtract(R.unaryMinus((R.add(2, 3))), 4)")
    unit_test("123 %% 5 == 4", "R.EQ(R.mod(123, 5), 4)")

    # Test extraction /  subsetting
    unit_test("self$abc(123)", "self.abc(123)")
    unit_test("obj_1$method_1(x)", "obj_1.method_1(x)")
    unit_test("obj_1$attr_1$x", "obj_1.attr_1.x")
    unit_test("obj_1$attr_1$f(1 + 2)", "obj_1.attr_1.f(R.add(1, 2))")
    unit_test("(a + b)$x", "(R.add(a, b)).x")
    unit_test("obj_1$new(x, y)", "new obj_1(x, y)")
    unit_test("lib_1$obj_1$new(x, y)", "new lib_1.obj_1(x, y)")
    unit_test("abc$abc[0]", "R.extract(abc.abc, 0)")
    unit_test("abc$abc[[0]]", "R.extract2(abc.abc, 0)")
    unit_test("abc$abc[[]]", "null")
    unit_test("abc$abc[[c(0, 1)]]", "R.extract2(abc.abc, R.c(0, 1))")
    unit_test("abc$abc[0] <- 2", "abc.abc = R.extractAssign(abc.abc, 2, 0)")
    unit_test("abc$abc[0, 3] <- 2", "abc.abc = R.extractAssign(abc.abc, 2, 0, 3)")
    unit_test("abc$abc[[0]] <- 99", "abc.abc = R.extract2Assign(abc.abc, 99, 0)")
    unit_test("abc$abc[[c(0, 1)]] <- c(2,3)", "abc.abc = R.extract2Assign(abc.abc, R.c(2, 3), R.c(0, 1))")
    testthat::expect_error(default("abc$abc[[0, 1]]"))
    testthat::expect_error(default("abc$abc[[]] <- 99"))
    testthat::expect_error(default("abc$abc[[0, 1]] <- 99"))
    unit_test("abc$abc[0,1]", "R.extract(abc.abc, 0, 1)")
    unit_test("abc$abc[1:3]", "R.extract(abc.abc, R.seq(1, 3))")
    unit_test("abc$abc[]", "R.extract(abc.abc, R.emptyIndex(abc.abc, 0))")
    unit_test("abc$abc[, 0]", "R.extract(abc.abc, R.emptyIndex(abc.abc, 0), 0)")
    unit_test("abc$abc[0, ]", "R.extract(abc.abc, 0, R.emptyIndex(abc.abc, 1))")
    unit_test("abc[, , 0]", "R.extract(abc, R.emptyIndex(abc, 0), R.emptyIndex(abc, 1), 0)")
    unit_test("abc[1:3, , 0]", "R.extract(abc, R.seq(1, 3), R.emptyIndex(abc, 1), 0)")

    # Test control flow
    unit_test("if (TRUE) f(x)", "if (true) f(x)")
    unit_test("if (TRUE) f(x) else g(x)", "if (true) f(x) else g(x)")
    unit_test("for (i in iterables) { x }", "for (let i of iterables) {\n    x\n}")
    unit_test("while (TRUE) { do(x) }", "while (true) {\n    do(x)\n}")

    # Test variable declaration
    unit_test("let (x)", "let x")
    unit_test("let (x = 3)", "let x = 3")
    unit_test("declare (x)", "let x")
    unit_test("declare (y = 4)", "let y = 4")
    unit_test("const (y = 4)", "const y = 4")

    # Test R list
    unit_test("list(x = 1, y = 2)", "{ \"x\": 1, \"y\": 2 }")
    unit_test("list('x' = 1)", "{ \"x\": 1 }")
    unit_test("list(`x` = 1)", "{ \"x\": 1 }")
    unit_test("list('abc-abc' = 1)", "{ \"abc-abc\": 1 }")
    unit_test("list(`abc-abc` = 1)", "{ \"abc-abc\": 1 }")
    testthat::expect_warning(default("list(1, 99)"))
    testthat::expect_warning(default("list(x = 1, 99)"))

    # Test dataframe operations
    unit_test("data.frame(x = 2, y = 2)", "R.data_frame({ \"x\": 2, \"y\": 2 })")
    unit_test("summarise(df0, n = f)", "R.summarise(df0, 'n', f)")
    unit_test("summarise(df0, n = f, m = g)", "R.summarise(df0, ['n', 'm'], [f, g])")
    testthat::expect_error(default("summarise(df0, f)"))
    unit_test("mutate(df0, n = f)", "R.mutate(df0, 'n', f)")
    unit_test("mutate(df0, n = f, m = g)", "R.mutate(df0, ['n', 'm'], [f, g])")
    testthat::expect_error(default("mutate(df0, f)"))

    # Test ifelse operator
    unit_test("ifelse(test, yes, no)", "test ? yes : no")

    # Test anonymous function
    unit_test("lambda(sin(30))", "function() { return R.sin(30); }")
    unit_test("lambda(x, sin(x))", "function(x) { return R.sin(x); }")
    unit_test("lambda(x = 99, sin(x))", "function(x = 99) { return R.sin(x); }")

    # Test pipe operator
    unit_test("a %>% b", "b(a)")
    unit_test("a %>% b()", "b(a)")
    unit_test("a %>% b(arg2 = 2)", "b(a, 2)")
    unit_test("a %>% b(arg2 = 2, arg3 = 3)", "b(a, 2, 3)")
    unit_test("f(x) %>% b(arg2 = 2)", "b(f(x), 2)")
    unit_test("f(x=4) %>% b", "b(f(4))")
    unit_test("f(x=4) %>% b()", "b(f(4))")

    # Test function definition
    unit_test("function(b, c) {}", "function(b, c) {\n    \n}")
    unit_test("function(x, y) x + y", "function(x, y) { R.add(x, y) }")
    unit_test("function(x, y) {x + y}", "function(x, y) {\n    R.add(x, y)\n}")
    unit_test("function(x = 3, y) {x + y}", "function(x = 3, y) {\n    R.add(x, y)\n}")

    # Test custom binary operator
    unit_test("abc %op% abc", "%op%(abc, abc)")

    # Test raw string
    unit_test("raw_str(r'(`x = $(1 + 1)`)')", "`x = $(1 + 1)`")
    unit_test("raw_str('`x = $(1 + 1)`')", "`x = $(1 + 1)`")
    unit_test("raw_str('/[0-9]+/')", "/[0-9]+/")
    unit_test("raw_str('/[0-9]+/')", "/[0-9]+/")
    testthat::expect_error(default("raw_str(123)"))

    # Test that function arguments are rewritten
    unit_test("function(b = TRUE, c = FALSE) {}", "function(b = true, c = false) {\n    \n}")
    unit_test("function(b = 1:3, c = 3:5) {}", "function(b = R.seq(1, 3), c = R.seq(3, 5)) {\n    \n}")
    unit_test("function(n = 2 ^ 4) {}", "function(n = R.pow(2, 4)) {\n    \n}")
    unit_test("function(n = 3 ^ 2 ^ 2) {}", "function(n = R.pow(3, R.pow(2, 2))) {\n    \n}")
    unit_test("function(x = 3 + a, y) {x + y}", "function(x = R.add(3, a), y) {\n    R.add(x, y)\n}")

    # Test conditional rewriting
    unit_test("a$length", "a.length")
    unit_test("obj_1$length <- length(abcde)", "obj_1.length = R.length(abcde)")
    unit_test("length(obj_1$length$max)", "R.length(obj_1.length.max)")

    # Test error handling
    unit_test("stop(\"ERROR_MSG\")", "throw new Error(\"ERROR_MSG\")")
    unit_test("try({print(123)})",
              "try {\n    R.print(123)\n} catch(error) {\n    console.log(error)\n}")
    unit_test("try({print(123)\nprint('abc')})",
              "try {\n    R.print(123)\n    R.print(\"abc\")\n} catch(error) {\n    console.log(error)\n}")
    unit_test(
        "tryCatch(1+1, function(e) {\n      print(e)\n})",
        "try {\n    R.add(1, 1)\n} catch(error) {\n    (function(e) {\n        R.print(e)\n    })(error)\n}"
    )
    unit_test(
        "tryCatch(1+1, function(e) {\n      print(e)\n}, print(\"finally!\"))",
        "try {\n    R.add(1, 1)\n} catch(error) {\n    (function(e) {\n        R.print(e)\n    })(error)\n} finally {\n    R.print(\"finally!\")\n}"
    )

    # Test R6Class
    unit_test(
        "R6Class(\"Class_1\")",
        "function() {\n    // public variables and methods\n    let self = this\n    \n    // private variables and methods\n    let that = this, private = {}\n    \n    if (self.initialize) {\n        self.initialize()\n    }\n}"
    )
    testthat::expect_error(default("R6Class(\"myClass\", x = \"NO LIST\")"))
    testthat::expect_error(default("R6Class(\"myClass\", list(\"NO NAME\"))"))
    testthat::expect_error(default("R6Class(\"myClass\", list(), x = \"NO LIST\")"))
    testthat::expect_error(default("R6Class(\"myClass\", list(), list(\"NO NAME\"))"))
    testthat::expect_error(default("R6Class(\"myClass\", list(initialize = \"ERROR: NOT A FUNCTION\"))"))
})

testthat::test_that("Test transpilation with default 2 deparsers", {
    # Basic 2 setup
    basic_2 <- purrr::partial(compile_exprs, rules = basic_2_rules(), deparsers = default_2_deparsers())
    unit_test <- purrr::partial(test_equal, f = basic_2)

    unit_test("function(x) {x}", "function(x) {\n    return x\n}")
    testthat::expect_warning(
        unit_test("function(x) {if(x) {x} else {x + 1}}",
                  "function(x) {\n    if (x) {\n        x\n    } else {\n        x + 1\n    }\n}")
    )
    unit_test("x <- 3", "var x = 3")
    unit_test("x <<- 3", "x = 3")
    unit_test("x$a <- 1", "x.a = 1")
    unit_test("x[1] <- 1", "x[1] = 1")
    testthat::expect_warning(basic_2("function(x) { x <- 10 }"))
    testthat::expect_warning(basic_2("function(x) {if(x) {x} else {x + 1}}"))
    testthat::expect_warning(basic_2("function(x) { for (i in 1:10) { print(i) } }"))

    # Default 2 setup
    default_2 <- purrr::partial(compile_exprs, rules = default_2_rules(), deparsers = default_2_deparsers())
    unit_test <- purrr::partial(test_equal, f = default_2)

    unit_test("function(x) {}", "function(x) {\n    \n}")
    unit_test("function(x) {x}", "function(x) {\n    return x\n}")
    unit_test("function(x) x", "function(x) { return x }")
    testthat::expect_warning(
        unit_test("function(x) {if(x) {x} else {x + 1}}",
                  "function(x) {\n    if (x) {\n        x\n    } else {\n        R.add(x, 1)\n    }\n}")
    )
    unit_test("x <- 3", "var x = 3")
    unit_test("x <<- 3", "x = 3")
    unit_test("x$a <- 1", "x.a = 1")
    unit_test("x[1] <- 1", "R.extract(x, 1) = 1")

    testthat::expect_warning(default_2("function(x) { x <- 10 }"))
    testthat::expect_warning(default_2("function(x) {if(x) {x} else {x + 1}}"))
    testthat::expect_warning(default_2("function(x) { for (i in 1:10) { print(i) } }"))
})


# Test files transpilation (`compile_r`)
test_compile_r <- function(input) {
    in_file <- tempfile()
    writeLines(input, in_file)

    out_file <- tempfile()
    compile_r(in_file, out_file, basic_rules(), basic_deparsers())

    paste(readLines(out_file), collapse = "\n")
}

testthat::test_that("Test transpilation with basic rules and deparsers (files)", {
    unit_test <- purrr::partial(test_equal, f = test_compile_r)

    # Test assignment
    unit_test("a <- x <<- 10", "a = x = 10")
    unit_test("a <<- b <- 3 + 4", "a = b = 3 + 4")

    # Test arithmetic
    unit_test("3^2 + 14", "3 ** 2 + 14")
    unit_test("3 * pi - 3", "3 * pi - 3")
    unit_test("-3", "-3")
    unit_test("2 - 3", "2 - 3")
    unit_test("-3 + 4", "-3 + 4")
    unit_test("-(2 + 3) - 4", "-(2 + 3) - 4")
    unit_test("123 %% 5 == 4", "123 % 5 == 4")

    # Test subsetting / extraction
    unit_test("self$abc(123)", "self.abc(123)")
    unit_test("obj_1$method_1(x)", "obj_1.method_1(x)")
    unit_test("obj_1$attr_1$x", "obj_1.attr_1.x")
    unit_test("obj_1$attr_1$f(1 + 2)", "obj_1.attr_1.f(1 + 2)")
    unit_test("(a + b)$x", "(a + b).x")
    unit_test("abc$abc[0]", "abc.abc[0]")
    unit_test("abc$abc[[0]]", "abc.abc[[0]]")

    # Test control flow
    unit_test("if (TRUE) f(x)", "if (true) f(x)")
    unit_test("if (TRUE) f(x) else g(x)", "if (true) f(x) else g(x)")
    unit_test("while (TRUE) { do(x) }", "while (true) {\n    do(x)\n}")
    unit_test("for (i in iterables) { x }", "for (let i of iterables) {\n    x\n}")

    # Test variable declaration
    unit_test("let (x)", "let x")
    unit_test("let (x = 3)", "let x = 3")

    # Test function declaration
    unit_test("function(b, c) {}", "function(b, c) {\n    \n}")

    # Test R list
    unit_test("list(x = 1, y = 2)", "{ \"x\": 1, \"y\": 2 }")
    unit_test("list('x' = 1)", "{ \"x\": 1 }")
    unit_test("list(`x` = 1)", "{ \"x\": 1 }")
    unit_test("list('abc-abc' = 1)", "{ \"abc-abc\": 1 }")
    unit_test("list(`abc-abc` = 1)", "{ \"abc-abc\": 1 }")

    # Test raw string
    unit_test("raw_str(r'(`x = $(1 + 1)`)')", "`x = $(1 + 1)`")
    unit_test("raw_str('`x = $(1 + 1)`')", "`x = $(1 + 1)`")
    unit_test("raw_str('/[0-9]+/')", "/[0-9]+/")

    # Test error handling
    unit_test("try({print(123)})",
              "try {\n    print(123)\n} catch(error) {\n    console.log(error)\n}")
    unit_test("try({print(123)\nprint('abc')})",
              "try {\n    print(123)\n    print(\"abc\")\n} catch(error) {\n    console.log(error)\n}")

    # Test that function arguments are rewritten
    unit_test("function(b = TRUE, c = FALSE) {}", "function(b = true, c = false) {\n    \n}")
    unit_test("function(n = 2 ^ 4) {}", "function(n = 2 ** 4) {\n    \n}")
    unit_test("function(n = f(3 + g(2 ^ 4))) {}", "function(n = f(3 + g(2 ** 4))) {\n    \n}")
    unit_test("function(n = 3 ^ 2 ^ 2) {}", "function(n = 3 ** 2 ** 2) {\n    \n}")
})


# Additional tests
testthat::test_that("Test raw string", {
    fpath <- system.file("test_files/test_raw_string.R", package = "sketch")
    exprs <- rlang::parse_exprs(file(fpath))
    x <- deparse_js(exprs[[1]], deparsers = basic_deparsers())
    testthat::expect_equal(x, "/[123]+/")

    x <- deparse_js(exprs[[2]], deparsers = basic_deparsers())
    testthat::expect_equal(x, "`template: ${x}.`")
})

testthat::test_that("Test R6Class", {
    read_file <- function(x) trimws(readLines(x))

    file <- system.file("test_files/test_R6.R", package = "sketch")
    file_ref <- system.file("test_files/test_R6.js", package = "sketch")
    temp <- compile_r(file, tempfile())
    testthat::expect_equal(read_file(temp), read_file(file_ref))

    file <- system.file("test_files/test_R6_2.R", package = "sketch")
    file_ref <- system.file("test_files/test_R6_2.js", package = "sketch")
    temp <- compile_r(file, tempfile())
    testthat::expect_equal(read_file(temp), read_file(file_ref))
})


# # Test (deparsers) list concatenation
# testthat::test_that("Test (deparsers) list concatenation", {
#     testthat::expect_equal(
#         clist(list(x = 1), list(x = 3)),
#         list(x = 3)
#     )
#     testthat::expect_equal(
#         clist(list(x = 1, y = 2), list(x = 3)),
#         list(x = 3, y = 2)
#     )
#     testthat::expect_equal(
#         clist(list(x = 1), list(x = 3, y = 4)),
#         list(x = 3, y = 4)
#     )
#     testthat::expect_equal(
#         clist(list(x = 1), list(x = 3, y = 4, z = 5)),
#         list(x = 3, y = 4, z = 5)
#     )
# })
