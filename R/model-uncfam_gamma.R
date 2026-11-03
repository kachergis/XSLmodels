# A nested special case of uncfam_general(): free familiarity exponent.
uncfam_gamma_model <- function(params, data, control) {
  uncfam_general_model(general_params(params, gamma = params[["gamma"]]), data, control)
}

#' Kachergis 2012 with a free familiarity exponent
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) that decouples familiarity's
#' contribution to a trial's attentional allocation from uncertainty's.
#' [uncfam()]'s allocation weight for pair (w, o) is `exp(B * entropy) *
#' assocs` -- entropy gets a free, tunable exponent (`B`), but familiarity
#' (`assocs`, the pair's current association strength) enters linearly, with
#' an implicit weight of 1 and no free parameter of its own. This adds one
#' parameter, `gamma`, raising familiarity to a free power instead:
#' `assocs^gamma * exp(B * entropy)`. `gamma = 1` is identical to
#' [uncfam()]; `gamma < 1` dampens the "rich get richer" effect (diminishing
#' returns on an already-strong pairing); `gamma > 1` amplifies it
#' (winner-take-all).
#'
#' Cross-validated against all of [xsl_datasets] (SSE to human 18AFC
#' accuracy, the package's standard model-comparison objective),
#' `uncfam_gamma()` beats plain [uncfam()] in every fold of a 5-fold split
#' (mean test SSE 0.48 vs. 0.56), with a best-fit `gamma` around 0.3 -- i.e.
#' familiarity's pull *dampens* with diminishing returns when explaining how
#' well people learn from a passively-received training sequence. A
#' separate analysis fitting this same free-`gamma` substrate to *active*
#' cross-situational word learning (Kachergis, Yu, & Shiffrin's paradigm
#' where learners choose which items to see named next, rather than
#' receiving a fixed passive sequence) found a best-fit `gamma` around 2
#' instead -- amplified, not dampened. Both contexts agree that familiarity
#' should get its own free exponent rather than [uncfam()]'s hard-coded
#' linear one, but disagree on which direction it bends: the process
#' governing moment-to-moment choice of what to study next doesn't appear
#' to be simply the same one governing how much is learned from a given,
#' already-fixed sequence.
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param gamma Familiarity exponent (1 = identical to [uncfam()])
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' # gamma = 1 reproduces uncfam() exactly
#' sse_gamma1 <- xsl_run(uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1),
#'                       get_example_ambiguous_condition())$sse
#' sse_uncfam <- xsl_run(uncfam(X = .1, C = 1, B = .98),
#'                       get_example_ambiguous_condition())$sse
#' isTRUE(all.equal(sse_gamma1, sse_uncfam))
#'
#' mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 0.3) # dampened familiarity
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_gamma <- function(X, B, C, gamma) {
  xslMod(
    name = "uncfam_gamma_model",
    description = "uncfam() with a free familiarity exponent (gamma)",
    model = uncfam_gamma_model,
    params = list(X = X, B = B, C = C, gamma = gamma),
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}
