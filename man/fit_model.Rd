% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitting.R
\name{fit_model}
\alias{fit_model}
\title{Fit Model to Conditions Using Differential Evolution}
\usage{
fit_model(model_name, conds, lower, upper)
}
\arguments{
\item{model_name}{A string specifying the name of the model to be fitted.}

\item{conds}{A list representing the conditions under which the model is fit.}

\item{lower}{Numeric vector of lower bounds for the model's parameters.}

\item{upper}{Numeric vector of upper bounds for the model's parameters.}
}
\value{
An object of class \code{DEoptim} representing the fitting result, which includes the best set of parameters found and the corresponding SSE value.
}
\description{
This function fits a model to provided conditions using the Differential Evolution optimization algorithm. It optimizes the model parameters to minimize the sum of squared errors (SSE) between the model's predictions and human accuracy data.
}
