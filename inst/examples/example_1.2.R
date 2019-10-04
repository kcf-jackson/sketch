# Bouncing ball
# source: Example 1.2 from https://natureofcode.com/book/chapter-1-vectors/

# Class implementation
Vector <- function(x, y) {
    self$x <- x
    self$y <- y
    self$add <- function(pv) {
        self$x <- self$x + pv$x
        self$y <- self$y + pv$y
    }
}

# Main
position <- Vector$new(100, 100)
velocity <- Vector$new(2.5, 5)

setup <- function() {
    createCanvas(500, 300)
    background(225)
}

draw <- function() {
    background(225)

    position$add(velocity)

    if ((position$x > width) || (position$x < 0)) {
        velocity$x <<- velocity$x * -1
    }
    if ((position$y > height) || (position$y < 0)) {
        velocity$y <<- velocity$y * -1
    }

    stroke(0)
    fill(175)
    ellipse(position$x, position$y, 48, 48)
}
