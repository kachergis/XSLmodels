# A nested special case of uncfam_general(): sampled attention, K objects per
# word per trial.
uncfam_sampling_model <- function(params, data, control) {
  uncfam_general_model(general_params(params, K = params[["K"]]), data, control)
}

#' Kachergis 2012
#'
#' Kachergis et al. 2012 uncertainty- and familiarity-biased associative model
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param K Number of associations to update per word (constraint: B <= X)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_sampling(X = .1, C = 1, B = .98, K = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_sampling <- function(X, B, C, K = 1) {
  xslMod(
    name = "uncfam_sampling",
    description = "Kachergis et al. 2012 uncertainty- and familiarity-biased associative model (sampling version)",
    model = uncfam_sampling_model,
    params = list(X = X, B = B, C = C, K = K),
    stochastic = TRUE,
    supports_start_matrix = TRUE
  )
}

# TODO: X = chi, B = lambda, C = alpha, K = k
