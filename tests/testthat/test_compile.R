# Testing high-level functions - rewrite and deparse0
unit_test <- function(x, y) {
  f <- function(x) rewrite(parse0(x))
  # g <- purrr::compose(deparse, f)
  h <- purrr::compose(deparse0, f)

  cat("==========Test==========", "\n")
  cat("Input             : ", x, "\n")
  # cat("Output (deparse)  : ", g(x), "\n")
  cat("Output (deparse0) : ", h(x), "\n")

  testthat::expect_equal(h(x), y)
}

unit_test("a <- x <<- 10", "a = x = 10")
unit_test("a <<- b <- 3 + 4", "a = b = 3 + 4")
unit_test("3^2 + 14", "3 ** 2 + 14")
unit_test("1:10 + 120", "R.seq_by(1, 10) + 120")
unit_test("3 * pi - 3", "3 * Math.PI - 3")
unit_test("self$abc(123)", "this.abc(123)")
unit_test("123 %% 5 == 4", "123 % 5 == 4")
unit_test("obj_1$method_1(x)", "obj_1.method_1(x)")
unit_test("obj_1$new(x, y)", "new obj_1(x, y)")

# Test function arguments are rewritten
unit_test("function(b = TRUE, c = FALSE) {}", "function(b = true, c = false) {\n    \n}")
unit_test("function(b = 1:3, c = 3:5) {}", "function(b = R.seq_by(1, 3), c = R.seq_by(3, 5)) {\n    \n}")

# Test conditional rewriting
unit_test("a$length", "a.length")
unit_test("obj_1$length <- length(abcde)", "obj_1.length = R.length(abcde)")
unit_test("length(obj_1$length$max)", "R.length(obj_1.length.max)")
