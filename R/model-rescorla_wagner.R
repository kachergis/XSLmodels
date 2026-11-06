# Rescorla-Wagner (1972) associative learning model
# adapted for cross-situational word learning by
# George Kachergis  george.kachergis@gmail.com

rescorla_wagner_model <- function(params, data, control) {
  C <- params[["C"]]
  alpha <- params[["alpha"]]
  lambda <- params[["lambda"]]
  beta <- params[["beta"]] * lambda
  all_outcomes <- identical(params[["outcomes"]] %||% "present", "all")
  word_cues <- identical(params[["cues"]] %||% "words", "words")
  phi <- params[["phi"]]

  reps <- control[["reps"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  m <- if (is.null(control[["start_matrix"]])) matrix(0, voc_sz, ref_sz) else control[["start_matrix"]] # association matrix
  colnames(m) <- ref
  rownames(m) <- voc
  perf <- matrix(0, reps, voc_sz) # a row for each block
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {

      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]
      wi <- match(tr_w, voc)
      oi <- match(tr_o, ref)

      if (word_cues) {
        # words are cues and objects outcomes: each outcome's prediction is
        # the summed association of the present words, and its prediction
        # error goes to every present word's association with it. Outcomes
        # are the present objects (target lambda), plus, with outcomes =
        # "all", every absent object (target 0)
        outs <- if (all_outcomes) seq_len(ref_sz) else oi
        pred <- colSums(m[wi, outs, drop = FALSE])
        delta <- alpha * beta * (lambda * (outs %in% oi) - pred) # one error per outcome
        # spread by column (outcome): adding the vector directly would
        # recycle it down the columns, giving word i outcome i's error
        m[wi, outs] <- m[wi, outs, drop = FALSE] +
          matrix(delta, length(wi), length(outs), byrow = TRUE)
      } else {
        # objects are cues and words outcomes (Ramscar et al., 2010): the
        # same, transposed; m[w, o] is object o's association with word w
        outs <- if (all_outcomes) seq_len(voc_sz) else wi
        pred <- rowSums(m[outs, oi, drop = FALSE])
        delta <- alpha * beta * (lambda * (outs %in% wi) - pred)
        m[outs, oi] <- m[outs, oi, drop = FALSE] + matrix(delta, length(outs), length(oi))
      }

      m <- m * C

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }

    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(if (is.null(phi)) m_test else softmax_weights(m_test, phi))
  }

  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Rescorla-Wagner (1972) error-driven associative model
#'
#' On each trial, the words present are cues and the objects outcomes. Each
#' outcome's prediction is the summed association of the present cues, and
#' its prediction error (`lambda - prediction` if it is present, `0 -
#' prediction` if absent) changes every present cue's association with it by
#' `alpha * beta * lambda` times the error. Cues therefore compete to predict
#' an outcome (e.g. blocking: a word paired with an object that another word
#' already predicts learns little about it). All associations then decay by
#' `C`.
#'
#' The defaults (`outcomes = "present"`, `cues = "words"`, Luce choice) are
#' the model as originally applied to cross-situational learning, which learns
#' only about the objects present on a trial. In designs where every pair
#' recurs, it then stays close to counting co-occurrences.
#'
#' @section Options:
#' - `outcomes = "all"` follows Rescorla & Wagner (1972) in treating every
#'   possible outcome as present (target `lambda`) or absent (target 0) on
#'   every trial, so a word's associations with objects that are absent when
#'   it is heard weaken. This is discriminative learning (Ramscar et al.,
#'   2010; Baayen et al., 2011). Associations can then become negative, which
#'   Luce choice treats as 0; `phi` gives a choice rule defined for them.
#' - `cues = "objects"` reverses the direction of prediction: objects are cues
#'   and words outcomes, as in Ramscar et al.'s (2010) feature-label ordering
#'   account. The matrix keeps words as rows (`m[w, o]` is object `o`'s
#'   association with word `w`), and test choices are still made among a
#'   word's row. Cues now compete to predict words rather than objects, so
#'   the two directions learn different associations.
#' - `phi` replaces Luce choice with a softmax choice rule with inverse
#'   temperature `phi` (see [softmax_weights()]), applied by [xsl_run()] and
#'   to the model's own `perf`.
#'
#' Choice is invariant to the scale of the associations under Luce choice,
#' and under softmax the scale trades off against `phi`, so `lambda` is not
#' identifiable separately from `beta` (and `phi`): fix it (e.g. at 1) when
#' fitting. Likewise `alpha` only enters as `alpha * beta`.
#'
#' @param C Decay
#' @param alpha Salience (fix at 1 unless manipulated)
#' @param beta Learning rate -- a proportion of lambda
#' @param lambda Maximum associative value that a CS can achieve - should be
#'   larger than learning rate
#' @param outcomes `"present"` (the default) updates only the objects present
#'   on a trial; `"all"` also updates absent objects toward 0.
#' @param cues `"words"` (the default): words predict objects. `"objects"`:
#'   objects predict words.
#' @param phi Optional softmax inverse temperature for the choice rule.
#'   `NULL` (the default) uses Luce choice.
#'
#' @references
#' Baayen, R. H., Milin, P., Durdevic, D. F., Hendrix, P., & Marelli, M.
#' (2011). An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. *Psychological
#' Review, 118*(3), 438-481.
#'
#' Ramscar, M., Yarlett, D., Dye, M., Denny, K., & Thorpe, K. (2010). The
#' effects of feature-label-order and their implications for symbolic
#' learning. *Cognitive Science, 34*(6), 909-957.
#'
#' Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian
#' conditioning: Variations in the effectiveness of reinforcement and
#' nonreinforcement. In A. H. Black & W. F. Prokasy (Eds.), *Classical
#' conditioning II* (pp. 64-99). Appleton-Century-Crofts.
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- rescorla_wagner(C = 1, alpha = 1, beta = 0.1, lambda = 3)
#' xsl_run(mod, get_example_ambiguous_condition())
#' # discriminative learning, with a softmax choice rule
#' mod <- rescorla_wagner(C = .98, alpha = 1, beta = .3, lambda = 1, outcomes = "all", phi = 5)
#' xsl_run(mod, get_example_ambiguous_condition())
rescorla_wagner <- function(C, alpha, beta, lambda, outcomes = c("present", "all"),
                            cues = c("words", "objects"), phi = NULL) {
  outcomes <- match.arg(outcomes)
  cues <- match.arg(cues)
  params <- list(C = C, alpha = alpha, beta = beta, lambda = lambda, outcomes = outcomes, cues = cues)
  if (!is.null(phi)) params$phi <- phi
  xslMod(
    name = "rescorla_wagner",
    description = "Rescorla-Wagner (1972) error-driven associative model",
    model = rescorla_wagner_model,
    params = params,
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}
