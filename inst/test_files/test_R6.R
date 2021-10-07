Person <- R6Class("Person", list(
  name = JS_NULL,
  age = JS_NULL,
  initialize = function(name, age) {
    self$name <- name
    self$age <- age
  }
))
eva <- Person$new("Eva", 6)
print(eva$name)
print(eva$age)
