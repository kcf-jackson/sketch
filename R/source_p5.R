source_p5_addin <- function() {
  source_p5_r(source_active())
}


#--------------------------------------------------------------------
# Source a p5.R file
source_p5_r <- function(index_r, keep = F) {
  temp_file <- tempfile()
  index_js <- compile_p5r(index_r, temp_file)
  if (keep) file.copy(temp_file, getwd())
  source_p5_js(index_js)
}


#--------------------------------------------------------------------
# Source active file
source_active <- function(file = "index.js") {
  x <- rstudioapi::getSourceEditorContext()$contents
  temp_file <- file.path(tempdir(), file)
  write(x, file = temp_file)
  temp_file
}


# Source a p5.js file
source_p5_js <- function(index_js) {
  index_html <- index_html()

  temp_dir <- tempdir()
  temp_html <- file.path(temp_dir, "index.html")
  temp_js <- file.path(temp_dir, "index.js")
  file.copy(
    from = c(index_html, index_js),
    to = c(temp_html, temp_js),
    overwrite = T
  )

  getOption("viewer")(temp_html)
}


index_html <- function() {
  system.file("template/index.html", package = "sketch")
}
