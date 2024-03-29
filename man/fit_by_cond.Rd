% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitting.R
\name{fit_by_cond}
\alias{fit_by_cond}
\title{Fit Model to Each Condition Individually}
\usage{
fit_by_cond(mname, conds, lower, upper)
}
\arguments{
\item{mname}{A string specifying the name of the model to be fitted to each condition.}

\item{conds}{A list where each element represents a different condition with its respective data.}

\item{lower}{Numeric vector of lower bounds for the model's parameters.}

\item{upper}{Numeric vector of upper bounds for the model's parameters.}
}
\value{
A list of fitting results, where each element corresponds to a different condition. The fitting results are objects returned from the \code{fit_model} function.
}
\description{
This function applies the \code{fit_model} function to each specified condition individually. It iteratively fits a given model to each condition and compiles the fitting results.
}
