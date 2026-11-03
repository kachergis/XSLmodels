# A nested special case of uncfam_general(): "uncertainty-only" drops the
# familiarity term (gamma = 0), "novelty" swaps entropy for novelty.
uncfam_model <- function(params, data, control) {
  variant <- params[["variant"]]
  uncfam_general_model(
    general_params(params, gamma = if (variant == "uncertainty-only") 0 else 1,
                   uncertainty = if (variant == "novelty") "novelty" else "entropy"),
    data, control)
}

#' Kachergis 2012
#'
#' Kachergis et al. 2012 uncertainty- and familiarity-biased associative model
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param variant Which variant of the model to fit (one of "entropy",
#'   "novelty", "uncertainty-only").
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam(X = .1, C = 1, B = .98)
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = .98, variant = "novelty")
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = 0) # familiarity-only
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = .98, variant = "uncertainty-only")
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam <- function(X, B, C, variant = c("entropy", "novelty",
                                        "uncertainty-only")) {
  variant <- match.arg(variant)
  xslMod(
    name = "uncfam_model",
    description = "Kachergis et al. 2012 uncertainty- and familiarity-biased associative model",
    model = uncfam_model,
    params = list(X = X, B = B, C = C, variant = variant),
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}

# TODO: X = chi, B = lambda, C = alpha
