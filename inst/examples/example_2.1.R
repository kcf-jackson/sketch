# Mover class
Mover <- function() {
  self$position <- createVector(30, 30)
  self$velocity <- createVector(0, 0)
  self$acceleration <- createVector(0, 0)
  self$mass <- 1

  self$applyForce <- function(force) {
    f <- p5$Vector$div(force, this.mass)
    self$acceleration$add(f)
  }

  self$update <- function() {
    self$velocity$add(self$acceleration)
    self$position$add(self$velocity)
    self$acceleration$mult(0)
  }

  self$display <- function() {
    stroke(0)
    fill(175)
    ellipse(self$position$x, self$position$y, 48, 48)
  }

  self$check_edges <- function() {
    if ((self$position$x > width) || (self$position$x < 0)) {
      self$velocity$x <- self$velocity$x * -1
    }
    if ((self$position$y > height) || (self$position$y < 0)) {
      self$velocity$y <- self$velocity$y * -1
    }
  }
}

# p5 Canvas
m1 <- 0

setup <- function() {
    createCanvas(500, 300)
    background(225)
    m1 <<- Mover$new()
}

draw <- function() {
    background(225)
    wind <- createVector(0.01, 0)
    gravity <- createVector(0, 0.1)

    m1$applyForce(wind)
    m1$applyForce(gravity)

    m1$update()
    m1$display()
    m1$check_edges()
}
