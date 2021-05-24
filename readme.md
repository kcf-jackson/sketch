
## R package `sketch`

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/kcf-jackson/sketch.svg?branch=master)](https://travis-ci.org/kcf-jackson/sketch)
[![Codecov test
coverage](https://codecov.io/gh/kcf-jackson/sketch/branch/master/graph/badge.svg)](https://codecov.io/gh/kcf-jackson/sketch?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/sketch)](https://CRAN.R-project.org/package=sketch)
<!-- badges: end -->

Creates interactive illustrations embeddable in R Markdown documents.
The package compiles R code into JavaScript code by applying rewriting
rules to the R AST. This allows users to develop JS-style visualisations
using only the R syntax.

![](./man/figures/ast_transform.png)

### Installation

``` r
install.packages("sketch")

# For the development version
# install.packages("remotes")
remotes::install_github("kcf-jackson/sketch")
```

Visit the [website](https://kcf-jackson.github.io/sketch-website/) for
[getting-started
guide](https://kcf-jackson.github.io/sketch-website/docs/),
[tutorials](https://kcf-jackson.github.io/sketch-website/tutorial/) and
other documentation\!
