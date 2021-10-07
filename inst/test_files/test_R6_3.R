#! config(debug = TRUE)

a <- R6Class(
    'test',
    public = list(
        x = 0,
        initialize = function(x = 999) {
            self$x <- x
            self
        }
    )
)

a_instance <- a$new()
print(a_instance$x)

a_instance_2 <- a$new(123)
print(a_instance_2$x)
