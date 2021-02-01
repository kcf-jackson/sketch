test <- R6Class("testthat", list(
    total = 0,
    pass = 0,
    error_msg = Array(),

    reset = function() {
        self$total <- 0
        self$pass <- 0
    },
    conduct_test = function() {
        self$total <- self$total + 1
    },
    pass_test = function() {
        self$pass <- self$pass + 1
    },
    report = function() {
        return(list(
            total = self$total,
            pass = self$pass,
            fail = self$total - self$pass,
            error_msg = self$error_msg
        ))
    },
    # Testing functions
    expect_true = function(object) {
        declare (msg)
        self$conduct_test()
        if (object != TRUE) {
            msg <- raw_str("`Error: ${object} isn't true.`")
            self$error_msg$push(msg)
            try(stop(msg))
        } else {
            self$pass_test()
        }
    },
    expect_false = function(object) {
        declare (msg)
        self$conduct_test()
        if (object != FALSE) {
            msg <- raw_str("`Error: ${object} isn't false.`")
            self$error_msg$push(msg)
            try(stop(msg))
        } else {
            self$pass_test()
        }
    },
    expect_equal = function(object, expected) {
        declare (msg)
        self$conduct_test()
        if (object != expected) {
            msg <- raw_str("`Error: ${object} not equal to ${expected}.`")
            self$error_msg$push(msg)
            try(stop(msg))
        } else {
            self$pass_test()
        }
    }
    # self$expect_error <- function(object) {
    #     declare (msg)
    #     self$conduct_test()
    #     tryCatch(object(),  })
    #     if (object) {
    #         msg <- raw_str("`Error: ${object} did not throw an error.`")
    #         self$error_msg$push(msg)
    #         try(stop(msg))
    #     } else {
    #         self$pass_test()
    #     }
    # }
))

testthat <- test$new()
# testthat::expect_true(123)
# testthat::expect_true(FALSE)
# testthat::expect_true(TRUE)
# print(testthat::report())
# print(123)
