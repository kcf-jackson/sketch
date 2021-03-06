% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core-rewrite.R
\name{split_rules}
\alias{split_rules}
\title{Split rules for customisation}
\usage{
split_rules(rs)
}
\arguments{
\item{rs}{A list of (grouped) rewriting rules. Note that a list of n rules
without grouping is a list of n groups of single rule.}
}
\description{
This function is the left-inverse of `combine_rules`, i.e.
\code{split_rules(combine_rules(rs, group)) = rs} for any variable `group`.
It is created to facilitate the addition or removal of rewriting rules.
}
