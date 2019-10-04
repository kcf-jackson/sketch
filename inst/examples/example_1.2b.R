# Bouncing ball
# source: Example 1.2 from https://natureofcode.com/book/chapter-1-vectors/

position <- 0
velocity <- 0

setup <- function() {
    createCanvas(500, 300)
    position <<- createVector(100, 100)
    velocity <<- createVector(2.5, 5)
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
