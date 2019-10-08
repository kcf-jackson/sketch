# Mover class
Mover <- function(m, x, y) {
    self$position <- createVector(x, y)
    self$velocity <- createVector(0, 0)
    self$acceleration <- createVector(0, 0)
    self$mass <- m

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
        ellipse(self$position$x, self$position$y, self$mass * 16, self$mass * 16)
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
m1 <- 0:9

setup <- function() {
    createCanvas(500, 300)
    background(225)
    for (i in 0:9) {
        m1[i] <<- Mover$new(random(0.1, 5), 0, 0);
    }
}

draw <- function() {
    background(225)
    wind <- createVector(0.01, 0)
    gravity <- createVector(0, 0.1)

    for (i in 0:9) {
        m1[i]$applyForce(wind)
        m1[i]$applyForce(gravity)

        m1[i]$update()
        m1[i]$display()
        m1[i]$check_edges()
    }
}
