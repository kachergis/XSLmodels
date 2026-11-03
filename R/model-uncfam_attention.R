# A nested special case of uncfam_general(): trial-level attention, kappa = 1.
uncfam_attention_model <- function(params, data, control) {
  uncfam_general_model(general_params(params, kappa = 1), data, control)
}

#' Biased associative model with attention scaled to trial uncertainty
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) in which the associative weight
#' distributed on a trial is additionally scaled by the ratio of that
#' trial's mean object entropy to the mean entropy of all objects seen so
#' far. This implements the "system-level" theory proposed by Fitneva &
#' Christiansen (2015): that learners are more alert (i.e. allocate a higher
#' learning rate) on trials with more uncertain items, such as in a Low
#' Initial Accuracy condition where many items have been mis-paired.
#'
#' @inheritParams uncfam
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_attention(X = .1, C = 1, B = .98)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_attention <- function(X, B, C) {
  xslMod(
    name = "uncfam_attention_model",
    description = paste(
      "Biased associative model (Kachergis et al. 2012) with learning rate",
      "scaled by relative trial uncertainty (Fitneva & Christiansen 2015",
      "system-level attention account)"
    ),
    model = uncfam_attention_model,
    params = list(X = X, B = B, C = C),
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}
