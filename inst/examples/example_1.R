# Bouncing ball
# source: Example 1.1 from https://natureofcode.com/book/chapter-1-vectors/

x <- 100
y <- 100
xspeed <- 2
yspeed <- 7

setup <- function() {
    createCanvas(500, 300)
    background(225)
}

draw <- function() {
    background(225)

    x <<- x + xspeed
    y <<- y + yspeed

    if ((x > width) || (x < 0)) {
        xspeed <<- xspeed * -1
    }
    if ((y > height) || (y < 0)) {
        yspeed <<- yspeed * -1
    }

    stroke(0);
    fill(175);
    ellipse(x, y, 48, 48);
}
