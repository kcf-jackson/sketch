Person2 <- R6Class("Person", public = list(
    x = 10,
   initialize = function(name, age) {
       private$name <- name
       private$age <- age
   },
   get_age = function() {
       # checks if public method can access private variable
       print(private$age)
       # checks if private method can access public variable
       print(private$get_age_plus_x())
   }
), private = list(
   age = JS_NULL,
   name = JS_NULL,
   get_age_plus_x = function() {
       return(private$age + self$x)
   }
))
elena <- Person2$new("Elena", 4)
elena$get_age()
