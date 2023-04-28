#| load_library("p5")
#| load_script("test_sketch_2.R")

setup <- function() {
    createCanvas(300, 300)
}

draw <- function() {
    circle(150, 150, 30)
    noLoop()
}
