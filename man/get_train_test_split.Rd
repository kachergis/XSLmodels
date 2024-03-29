% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitting.R
\name{get_train_test_split}
\alias{get_train_test_split}
\title{Split Data into Training and Testing Sets Based on Indices}
\usage{
get_train_test_split(test_inds, conds)
}
\arguments{
\item{test_inds}{A numeric vector of indices indicating which elements in \code{conds} should be used for the test set.}

\item{conds}{A list of conditions, where each condition is represented as a separate element.}
}
\value{
A list with two elements: \code{train} and \code{test}. Each element is a list of conditions, where \code{train} includes the conditions not indexed by \code{test_inds}, and \code{test} includes the conditions indexed by \code{test_inds}.
}
\description{
This function divides a given set of conditions into training and testing sets. The division is based on a provided vector of indices, which specify the conditions to be used for testing.
}
