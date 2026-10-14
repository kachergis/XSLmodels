uncfam_general_model <- function(params, data, control) {
  X <- params[["X"]] # associative weight to distribute
  B <- params[["B"]] # weighting of uncertainty vs. familiarity (lambda)
  C <- params[["C"]] # decay
  gamma <- params[["gamma"]] # familiarity exponent
  eps <- params[["eps"]] # weight of elimination (mutual-exclusivity) inference
  kappa <- params[["kappa"]] # exponent on trial-level attention scaling of X
  K <- params[["K"]] # samples per word per trial (Inf = deterministic allocation)
  uncertainty <- params[["uncertainty"]] # "entropy" or "novelty"
  rho <- params[["rho"]] # weight of the short-term trace in familiarity (0 = none)
  Cs <- params[["Cs"]] # decay of the short-term trace

  reps <- control[["reps"]]
  start_matrix <- control[["start_matrix"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  freq_w <- rep(0, voc_sz) # times each word has appeared (novelty)
  freq_o <- rep(0, ref_sz)
  names(freq_w) <- voc
  names(freq_o) <- ref
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  if (!is.null(start_matrix)) {
    m <- start_matrix
  } else {
    m <- matrix(0, voc_sz, ref_sz) # association matrix
  }
  colnames(m) <- ref
  rownames(m) <- voc
  # short-term trace: receives every update like m, but decays at Cs; it
  # only adds to the familiarity that drives attention (not to uncertainty,
  # elimination, or test performance)
  st <- m * 0
  perf <- matrix(0, reps, voc_sz) # a row for each block
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {

      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      m <- update_known(m, tr_w, tr_o) # what's been seen so far?

      # uncertainty of each word and object on the trial
      if (uncertainty == "entropy") {
        term_w <- if (length(tr_w) > 1) apply(m[tr_w, ], 1, shannon_entropy) else shannon_entropy(m[tr_w, ])
        term_o <- apply(as.matrix(m[, tr_o]), 2, shannon_entropy)
      } else { # novelty
        freq_w[tr_w] <- freq_w[tr_w] + 1
        freq_o[tr_o] <- freq_o[tr_o] + 1
        term_w <- 1 / (1 + freq_w[tr_w])
        term_o <- 1 / (1 + freq_o[tr_o])
      }

      # trial-level attention: scale X by how uncertain this trial's objects
      # are relative to all objects seen so far (as in uncfam_attention())
      X_t <- X
      if (kappa != 0) {
        ent_o_all <- apply(m, 2, shannon_entropy)
        mean_ent_o <- mean(ent_o_all, na.rm = TRUE)
        if (is.finite(mean_ent_o) && mean_ent_o != 0) {
          X_t <- X * (mean(ent_o_all[tr_o], na.rm = TRUE) / mean_ent_o) ^ kappa
        }
      }

      # familiarity: long-term association, plus the short-term trace
      fam <- if (rho != 0) m + rho * st else m

      if (is.infinite(K)) {
        # deterministic allocation: familiarity^gamma x uncertainty,
        # normalized over the trial's pairs
        assocs <- m[tr_w, tr_o]
        terms <- exp(B * term_w) %*% t(exp(B * term_o))
        terms <- ((if (rho != 0) fam[tr_w, tr_o] else assocs) ^ gamma) * terms
        idx_w <- tr_w
        idx_o <- tr_o
      } else {
        # sampled attention (as in uncfam_sampling()): each word attends to K
        # objects sampled in proportion to familiarity^gamma x uncertainty,
        # and only the sampled pairs share the trial's weight
        u_tr_w <- unique(tr_w)
        u_tr_o <- unique(tr_o)
        tr_o_idx <- match(u_tr_o, ref)
        ent_w_tr <- exp(B * term_w[match(u_tr_w, tr_w)])
        names(ent_w_tr) <- u_tr_w
        ent_o_tr <- exp(B * term_o[match(u_tr_o, tr_o)])
        names(ent_o_tr) <- u_tr_o
        chosen <- matrix(0, length(u_tr_w), length(u_tr_o), dimnames = list(u_tr_w, u_tr_o))
        for (w in tr_w) {
          row_probs <- numeric(ref_sz)
          row_probs[tr_o_idx] <- fam[w, u_tr_o] ^ gamma * ent_w_tr[[as.character(w)]] * ent_o_tr
          if (sum(row_probs) == 0) next
          x_lab <- ref[sample(1:ref_sz, K, replace = TRUE, prob = row_probs)]
          chosen[as.character(w), as.character(x_lab)] <- fam[w, x_lab] ^ gamma
        }
        nent <- outer(ent_w_tr, ent_o_tr)
        terms <- chosen * nent
        assocs <- m[u_tr_w, u_tr_o]
        idx_w <- u_tr_w
        idx_o <- u_tr_o
      }

      # learning by elimination (as in uncfam_elimination()): a pair gains
      # weight to the extent that the trial's other words and objects are
      # already accounted for by each other
      if (eps != 0 && sum(terms) > 0) {
        a <- matrix(assocs, length(idx_w), length(idx_o))
        p_o_given_w <- a / rowSums(m[idx_w, , drop = FALSE])
        p_w_given_o <- t(t(a) / colSums(m[, idx_o, drop = FALSE]))
        q <- p_o_given_w * p_w_given_o
        support <- pmax(sum(q) - outer(rowSums(q), colSums(q), "+") + q, 0)
        share <- (terms / sum(terms) + eps * support) / (1 + eps * sum(support))
        delta <- X_t * share
      } else if (is.infinite(K)) {
        delta <- (X_t * terms) / sum(terms)
      } else {
        # same operation order as uncfam_sampling(): sampling sorts tied
        # probabilities, so even 1-ulp differences in m change later draws
        delta <- (X_t * chosen * nent) / sum(terms)
      }

      m <- m * C # decay everything
      # update associations on this trial (a trial on which nothing was
      # sampled is a no-op, as in uncfam_sampling(); decay still applies)
      if (all(is.finite(delta))) m[idx_w, idx_o] <- m[idx_w, idx_o] + delta
      if (rho != 0) {
        st <- st * Cs
        if (all(is.finite(delta))) st[idx_w, idx_o] <- st[idx_w, idx_o] + delta
      }

      index <- (rep - 1) * length(data$words) + t  # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' General biased associative model (nests the uncfam family)
#'
#' A single biased associative model (Kachergis et al. 2012) whose
#' extensions are free parameters, so that the package's [uncfam()] variants
#' are nested special cases and can be compared by fitting one model with
#' parameters fixed or freed. On each trial, a word-object pair \eqn{(w, o)}
#' on the trial receives a share of the trial's associative weight
#' \deqn{X_t \cdot \frac{b(w, o) + \epsilon s(w, o)}{1 + \epsilon \sum s},
#'   \quad b(w, o) \propto M_{w,o}^{\gamma} e^{\lambda (U(w) + U(o))}}
#' where \eqn{U} is each stimulus's uncertainty (entropy of its associations,
#' or novelty), \eqn{s} is elimination support (see [uncfam_elimination()]),
#' and \eqn{X_t = X \cdot (\bar{H}_{trial} / \bar{H}_{all})^\kappa} scales the
#' trial's weight by its objects' relative uncertainty (see
#' [uncfam_attention()]). With finite `K`, attention is sampled rather than
#' allocated: each word attends to `K` objects drawn in proportion to
#' \eqn{b}, and only sampled pairs share the weight (see [uncfam_sampling()]);
#' as `K` grows every pair is sampled and this approaches `K = Inf`.
#'
#' A short-term trace (`rho > 0`) adds recency to the familiarity bias: every
#' update also goes into a second matrix \eqn{S} that decays by `Cs` per trial
#' (vs. `C` for \eqn{M}), and familiarity in \eqn{b} becomes
#' \eqn{(M + \rho S)^\gamma}, so a pairing strengthened on a recent trial
#' draws extra attention. The trace affects only allocation during learning:
#' uncertainty, elimination, and test performance use \eqn{M}.
#'
#' Special cases (each reproduces the named function exactly):
#' \tabular{ll}{
#'   defaults \tab `uncfam()` \cr
#'   `B = 0` \tab familiarity only \cr
#'   `gamma = 0` \tab `uncfam(variant = "uncertainty-only")` \cr
#'   `uncertainty = "novelty"` \tab `uncfam(variant = "novelty")` \cr
#'   `gamma` free \tab `uncfam_gamma()` \cr
#'   `eps` free \tab `uncfam_elimination()` \cr
#'   `kappa = 1` \tab `uncfam_attention()` \cr
#'   `K` finite \tab `uncfam_sampling()` \cr
#' }
#' [uncfam_predictive()] is not nested: its prediction-error update is
#' unnormalized, a different class of rule.
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity (\eqn{\lambda})
#' @param C Decay
#' @param gamma Familiarity exponent (1 = [uncfam()])
#' @param eps Weight of learning by elimination (0 = none)
#' @param kappa Exponent on trial-level attention scaling (0 = none, 1 =
#'   [uncfam_attention()])
#' @param K Objects sampled per word per trial (`Inf` = deterministic
#'   allocation; finite = sampled attention, a stochastic model)
#' @param uncertainty Uncertainty measure: `"entropy"` (default) or
#'   `"novelty"` (`1 / (1 + times seen)`)
#' @param rho Weight of the short-term trace in familiarity (0 = none)
#' @param Cs Per-trial decay of the short-term trace (used only if `rho > 0`)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' dat <- get_example_ambiguous_condition()
#' xsl_run(uncfam_general(X = .1, B = .98, C = 1), dat)
#'
#' # nested comparison: does freeing gamma improve on uncfam()?
#' xsl_run(uncfam_general(X = .1, B = .98, C = 1, gamma = .5), dat)$sse
#'
#' # sampled attention (stochastic)
#' xsl_run(uncfam_general(X = .1, B = .98, C = 1, K = 1), dat, control = xslControl(n_sim = 50))
uncfam_general <- function(X, B, C, gamma = 1, eps = 0, kappa = 0, K = Inf,
                           uncertainty = c("entropy", "novelty"), rho = 0, Cs = .5) {
  uncertainty <- match.arg(uncertainty)
  if (!is.infinite(K) && (K < 1 || K != round(K))) stop("`K` must be a positive integer or Inf")
  xslMod(
    name = "uncfam_general_model",
    description = "General biased associative model nesting the uncfam() family",
    model = uncfam_general_model,
    params = list(X = X, B = B, C = C, gamma = gamma, eps = eps, kappa = kappa,
                  K = K, uncertainty = uncertainty, rho = rho, Cs = Cs),
    stochastic = !is.infinite(K)
  )
}
