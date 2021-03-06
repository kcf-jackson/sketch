% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extra-rmarkdown.R
\name{insert_sketch}
\alias{insert_sketch}
\title{Insert a 'sketch' app into an R Markdown document}
\usage{
insert_sketch(file, id, output_dir = NULL, render = TRUE, ...)
}
\arguments{
\item{file}{A character string; the path to the 'sketch' file.}

\item{id}{A character string; an unique identifier for the 'sketch' file.
Needed only when \code{output_dir} is not \code{NULL}.}

\item{output_dir}{A character string; a separate directory to save the
'sketch' app. Default to be NULL, which embeds the app in the Rmd file.}

\item{render}{TRUE or FALSE; if TRUE, call \link[htmltools]{doRenderTags};
if FALSE, return the 'shiny.tag' object.}

\item{...}{(Optional) Other attributes to pass to iframes. Also supports
the `rules`, `deparsers` and `debug` options to pass to `source_r`.}
}
\value{
An HTML string if \code{render} is TRUE, or a 'shiny.tag' object
if \code{render} is FALSE.
}
\description{
Insert a 'sketch' app into an R Markdown document
}
\examples{
# In an R code chunk of an R Markdown document
file <- system.file("test_files/test_RMD.R", package = "sketch")
insert_sketch(file, style = "width:500px; height:500px;", render = FALSE)

}
