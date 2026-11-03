# A nested special case of uncfam_general(): free elimination weight.
uncfam_elimination_model <- function(params, data, control) {
  uncfam_general_model(general_params(params, eps = params[["eps"]]), data, control)
}

#' Biased associative model with learning by elimination
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) that adds inference by elimination
#' (mutual exclusivity) to how a trial's associative weight is allocated. If
#' the other words and objects on a trial are already confidently paired with
#' each other, the remaining word and object probably go together, so their
#' pairing should draw attention even though it is still weak. In
#' [uncfam()], by contrast, an already-strong pair on the trial draws weight
#' *away* from such a pairing.
#'
#' On each trial, each on-trial pair \eqn{(w', o')} has a mutual confidence
#' \eqn{q(w', o') = P(o'|w') P(w'|o')}, from the row- and column-normalized
#' associations. A pair \eqn{(w, o)} gets elimination support
#' \eqn{s(w, o) = \sum_{w' \ne w, o' \ne o} q(w', o')}: how much of the rest
#' of the trial is accounted for by other pairs. The share of the trial's
#' weight \eqn{X} given to \eqn{(w, o)} is then
#' \deqn{\frac{b(w, o) + \epsilon s(w, o)}{1 + \epsilon \sum s}}
#' where \eqn{b(w, o)} is [uncfam()]'s normalized familiarity-and-uncertainty
#' share, so `eps = 0` reproduces [uncfam()] exactly.
#'
#' Motivated by the initial-accuracy experiment
#' ([kachergis_initial_accuracy]), where learners were more accurate on an
#' initially mis-paired word the more of its study trials it shared with a
#' word they already knew.
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param eps Weight of elimination inference (0 = identical to [uncfam()])
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_elimination(X = .1, B = .98, C = 1, eps = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_elimination <- function(X, B, C, eps) {
  xslMod(
    name = "uncfam_elimination_model",
    description = "uncfam() with learning by elimination (mutual exclusivity)",
    model = uncfam_elimination_model,
    params = list(X = X, B = B, C = C, eps = eps),
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}
