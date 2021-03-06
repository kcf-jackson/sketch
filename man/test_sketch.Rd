% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extra-test.R
\name{test_sketch}
\alias{test_sketch}
\title{Test a sketch application}
\usage{
test_sketch(app_script, test_script, port = 9454, ...)
}
\arguments{
\item{app_script}{A character string; the file path to the app.}

\item{test_script}{A character string; the file path to the tests.}

\item{port}{An integer to pass to `websocket$new()`.}

\item{...}{Additional arguments to pass to \link{source_r}.}
}
\value{
A "websocket" object.
}
\description{
Test a sketch application
}
\examples{
\dontrun{
app_file <- system.file("test_files/test_testthat_app.R", package = "sketch")
test_file <- system.file("test_files/test_testthat_test.R", package = "sketch")
# This following command will launch the default browser
res <- test_sketch(app_file, test_file)
}
}
