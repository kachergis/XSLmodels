% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitting.R
\name{run_stochastic_model}
\alias{run_stochastic_model}
\title{Run Stochastic Model on Provided Conditions}
\usage{
run_stochastic_model(
  conds,
  model_name,
  parameters,
  SSE_only = F,
  print_perf = F,
  get_resp_matrix = F,
  Nsim = 500
)
}
\arguments{
\item{conds}{A list representing the conditions under which the model is run. This could include training and testing data, along with human item accuracy (\code{HumanItemAcc}).}

\item{model_name}{A string specifying the name of the stochastic model to run.}

\item{parameters}{A list of parameters to be used by the stochastic model.}

\item{SSE_only}{Logical; if TRUE, only the SSE will be returned.}

\item{print_perf}{Logical; if TRUE, model performance and SSE will be printed.}

\item{get_resp_matrix}{Logical; if TRUE, the function returns the response matrix instead of the standard performance metrics.}

\item{Nsim}{Integer; the number of simulations to run for the stochastic model.}
}
\value{
Depending on the parameters, the function can return different types of outputs:
\itemize{
\item If \code{get_resp_matrix} is TRUE, it returns a response matrix aggregated over all simulations.
\item If \code{SSE_only} is TRUE, it returns only the SSE.
\item Otherwise, it returns a list containing model performance across conditions and the SSE.
}
}
\description{
This function runs a stochastic version of the specified word learning model on a set of provided conditions. It simulates the model multiple times and aggregates the results. The function can return either a response matrix, model performance, or sum of squared errors (SSE).
}
