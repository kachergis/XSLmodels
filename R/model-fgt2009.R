# Frank, Goodman & Tenenbaum (2009) intentional Bayesian word-learning model,
# plus the pragmatic (RSA) extension.
#
# Pure-R port of the Python package `wordlearn` (github.com/mcfrank/wurwur):
#   - src/wordlearn/model.py     -> the scoring kernel below (fgt_* helpers)
#   - src/wordlearn/rsa.py       -> fgt_rsa_situation_loglik()
#   - src/wordlearn_v2/mcmc.py   -> fgt_run_gibbs() (blocked per-object Gibbs +
#                                   MI-biased single-edge Metropolis moves)
#
# Unlike every other model in this package, this is a BATCH model: it does joint
# posterior inference over the whole corpus at once rather than an incremental
# trial-by-trial tally, so it has no learning trajectory and `control$reps` is
# ignored. `xslFit$matrix` is the posterior edge-marginal matrix
# P(word names object | corpus), which feeds get_perf()/mafc_test() directly.
#
# George Kachergis + Claude, September 2026

# --------------------------------------------------------------------------
# Scoring kernel (port of model.py)
# --------------------------------------------------------------------------

# All 2^n binary patterns over n positions, MSB-first (matches MATLAB
# `dec2bin(0:(2^n-1))=='1'`): n = 2 -> rows (0,0) (0,1) (1,0) (1,1).
fgt_enumerate_intents <- function(n) {
  if (n <= 0) return(matrix(0L, nrow = 1L, ncol = 0L))
  idx <- 0:(2^n - 1)
  cols <- lapply((n - 1):0, function(s) bitwAnd(bitwShiftR(idx, s), 1L))
  matrix(unlist(cols), nrow = length(idx), ncol = n)
}

# Per-trial intent cache. `gamma_intents` has shape (n_intents, n_obj + 1):
# column 1 is the (1 - gamma) "used non-referentially" weight, columns 2..n+1
# are the gamma * (1 / |intent|) weights for each candidate referent. The
# empty-intent row puts all weight on the non-referential channel.
fgt_build_intent_cache <- function(corpus, gamma) {
  lapply(corpus, function(sit) {
    n_obj <- length(sit$objects)
    intents <- fgt_enumerate_intents(n_obj)
    n_intents <- nrow(intents)
    isize <- rowSums(intents)
    if (length(isize)) isize[1] <- 1            # empty-intent row: avoid 0/0
    gi <- matrix(0, n_intents, n_obj + 1L)
    gi[, 1] <- 1 - gamma
    if (n_obj > 0) gi[, -1] <- gamma * (intents / isize)  # recycles isize by row
    gi[1, ] <- 0
    gi[1, 1] <- 1
    c(sit, list(gamma_intents = gi, n_intents = n_intents))
  })
}

# Per-lexicon word-cost cache. `word_costs` has shape (ref_sz + 1, voc_sz):
# row 1 is the distribution over words used non-referentially (in-lexicon words
# down-weighted by kappa); rows 2.. are P(word | this object is the referent),
# uniform over the words mapped to that object, falling back to the
# non-referential distribution for objects with no mapped word.
fgt_word_costs <- function(map, voc_sz, ref_sz, kappa) {
  n_map <- if (length(map)) ncol(map) else 0L
  nrp <- rep(1, voc_sz)
  if (n_map > 0) nrp[map[1, ]] <- nrp[map[1, ]] * kappa
  nrp <- nrp / sum(nrp)

  wc <- matrix(0, ref_sz + 1L, voc_sz)
  if (n_map > 0) for (k in seq_len(n_map)) wc[map[2, k] + 1L, map[1, k]] <- 1
  rs <- rowSums(wc)
  no_map <- rs == 0
  wc <- wc / ifelse(no_map, 1, rs)              # recycles divisor by row
  if (any(no_map)) wc[no_map, ] <- matrix(nrp, sum(no_map), voc_sz, byrow = TRUE)
  list(nrp = nrp, word_costs = wc)
}

# (unnormalized) log posterior of a lexicon `map` (2 x |L|, row 1 = word ids,
# row 2 = object ids, both 1-indexed positions) given the intent-cached corpus.
# `sit_indices` restricts the likelihood to a subset of trials (used by the
# Gibbs proposal); the prior is always the full geometric -alpha * |L|.
fgt_score_map <- function(map, cache, voc_sz, ref_sz, alpha, kappa,
                          sit_indices = NULL) {
  wc <- fgt_word_costs(map, voc_sz, ref_sz, kappa)$word_costs
  n_map <- if (length(map)) ncol(map) else 0L
  idxs <- if (is.null(sit_indices)) seq_along(cache) else sit_indices

  ll <- 0
  for (i in idxs) {
    sit <- cache[[i]]
    rows <- c(1L, sit$objects + 1L)
    word_cost <- wc[rows, sit$words, drop = FALSE]
    word_scores <- sit$gamma_intents %*% word_cost      # n_intents x n_words
    scores <- apply(word_scores, 1, prod)               # product over words
    ll <- ll + (log(sum(scores)) - log(length(scores))) # uniform 1/n_intents
  }
  -alpha * n_map + ll
}

# RSA variant: the referential word choice (uniform over an object's names) is
# replaced by the RSA pragmatic speaker S(word | object). The non-referential
# channel, the kappa penalty, and the marginalization over intents are
# unchanged, so the literal model is the depth-0 special case.
fgt_score_map_rsa <- function(map, cache, voc_sz, ref_sz, alpha, kappa,
                              rsa_alpha, rsa_depth, sit_indices = NULL) {
  nrp <- fgt_word_costs(map, voc_sz, ref_sz, kappa)$nrp
  n_map <- if (length(map)) ncol(map) else 0L
  mbin <- matrix(0, voc_sz, ref_sz)
  if (n_map > 0) for (k in seq_len(n_map)) mbin[map[1, k], map[2, k]] <- 1
  idxs <- if (is.null(sit_indices)) seq_along(cache) else sit_indices

  ll <- 0
  for (i in idxs) {
    ll <- ll + fgt_rsa_situation_loglik(cache[[i]], mbin, nrp, rsa_alpha, rsa_depth)
  }
  -alpha * n_map + ll
}

# RSA referential rows for one trial, then the same intent mixture as the
# literal kernel. Port of rsa.py::_rsa_situation_loglik (non-social path).
fgt_rsa_situation_loglik <- function(sit, mbin, nrp, alpha, depth) {
  present <- sit$objects
  words <- sit$words
  P <- length(present)
  U <- length(words)

  obj_rows <- matrix(0, P, U)
  msub <- mbin[, present, drop = FALSE]              # voc_sz x P
  named <- colSums(msub) > 0                         # present objects with a name
  cand <- which(rowSums(msub) > 0)                   # words naming any present object

  if (length(cand)) {
    tm <- msub[cand, , drop = FALSE]                 # C x P
    norm_rows <- function(x) x / rowSums(x)
    norm_cols <- function(x) sweep(x, 2, colSums(x), "/")
    suppressWarnings({
      L <- norm_rows(tm)                             # L0(object | word), uniform prior
      S <- norm_cols(exp(alpha * log(L)))            # S1(word | object)
      for (d in seq_len(depth - 1)) {
        L <- norm_rows(S)
        S <- norm_cols(exp(alpha * log(L)))
      }
    })
    S[!is.finite(S)] <- 0                            # unnamed-object columns -> 0
    row_of_word <- match(seq_len(nrow(mbin)), cand)  # word id -> row in S, else NA
    for (u in seq_len(U)) {
      k <- row_of_word[words[u]]
      if (!is.na(k)) obj_rows[, u] <- S[k, ]
    }
  }
  if (any(!named)) {
    obj_rows[!named, ] <- matrix(nrp[words], sum(!named), U, byrow = TRUE)
  }

  word_cost <- rbind(nrp[words], obj_rows)           # (P + 1) x U
  word_scores <- sit$gamma_intents %*% word_cost
  scores <- apply(word_scores, 1, prod)
  log(sum(scores)) - log(length(scores))
}

# --------------------------------------------------------------------------
# Inference: blocked per-object Gibbs + MI-biased single-edge Metropolis
# (port of the numpy-only path in wordlearn_v2/mcmc.py)
# --------------------------------------------------------------------------

# Normalized pointwise-MI matrix from co-occurrence, used to bias the add /
# swap proposals toward plausible edges. Port of fm_data.py::_compute_mis.
fgt_compute_mis <- function(corpus, voc_sz, ref_sz) {
  cooc <- matrix(0, voc_sz, ref_sz)
  w_count <- rep(0, voc_sz)
  o_count <- rep(0, ref_sz)
  for (sit in corpus) {
    for (w in sit$words) {
      w_count[w] <- w_count[w] + 1
      cooc[w, sit$objects] <- cooc[w, sit$objects] + 1
    }
    o_count[sit$objects] <- o_count[sit$objects] + 1
  }
  total <- sum(cooc)
  if (total == 0) return(matrix(0.5, voc_sz, ref_sz))
  pw_po <- outer(w_count / sum(w_count), o_count / sum(o_count))
  mi <- ifelse(pw_po > 0, (cooc / total) / pw_po, 0)
  mx <- max(mi)
  if (mx > 0) mi / mx else mi
}

# Per-object candidate word menus for the blocked Gibbs move: the words whose
# single-edge inclusion most improves the conditional likelihood over that
# object's trials, restricted to words that actually co-occur with it. This is
# wurwur's GibbsContext.build with the co-occurrence thresholds relaxed for the
# small, dense corpora of XSL experiments (min_cooc 1, no specificity floor).
fgt_build_candidates <- function(corpus, score1, voc_sz, ref_sz,
                                 top_k = 8L, min_cooc = 1L, prefilter = 40L) {
  cooc <- matrix(0, voc_sz, ref_sz)
  w_count <- rep(0, voc_sz)
  sit_idx <- vector("list", ref_sz)
  for (i in seq_along(corpus)) {
    uw <- unique(corpus[[i]]$words)
    uo <- unique(corpus[[i]]$objects)
    w_count[uw] <- w_count[uw] + 1
    for (w in uw) cooc[w, uo] <- cooc[w, uo] + 1
    for (o in uo) sit_idx[[o]] <- c(sit_idx[[o]], i)
  }

  lapply(seq_len(ref_sz), function(o) {
    sidx <- sit_idx[[o]]
    if (is.null(sidx)) return(integer(0))
    cond <- ifelse(w_count > 0, cooc[, o] / pmax(w_count, 1), 0)
    pool <- which(cooc[, o] >= min_cooc)
    if (!length(pool)) return(integer(0))
    if (length(pool) > prefilter) pool <- pool[order(-cond[pool])[seq_len(prefilter)]]
    base <- score1(matrix(integer(0), 2L, 0L), sidx)
    gains <- vapply(pool, function(w) {
      score1(matrix(c(w, o), 2L, 1L), sidx) - base
    }, numeric(1))
    pool[order(-gains)[seq_len(min(top_k, length(pool)))]]
  })
}

fgt_edges_to_map <- function(edges) {
  if (!length(edges$w)) return(matrix(integer(0), 2L, 0L))
  rbind(as.integer(edges$w), as.integer(edges$o))
}

# Resample one object's entire set of names, one candidate edge at a time, from
# its exact full conditional P(edge | rest) proportional to exp(log_p / T).
# Ordinary Gibbs -- always valid, and cannot deadlock the way an all-or-nothing
# joint block move can. Port of block_gibbs_object_step().
fgt_gibbs_object <- function(edges, logp, o, cand_o, score_full, temperature = 1) {
  cur_names <- edges$w[edges$o == o]
  C <- unique(c(cur_names, cand_o))
  if (!length(C)) return(list(edges = edges, logp = logp))

  for (w in C[sample.int(length(C))]) {
    is_on <- any(edges$w == w & edges$o == o)
    if (is_on) {
      keep <- !(edges$w == w & edges$o == o)
      toggled <- list(w = edges$w[keep], o = edges$o[keep])
    } else {
      toggled <- list(w = c(edges$w, w), o = c(edges$o, o))
    }
    toggled_logp <- score_full(fgt_edges_to_map(toggled))
    if (is_on) {
      lp_on <- logp; lp_off <- toggled_logp
    } else {
      lp_on <- toggled_logp; lp_off <- logp
    }
    p_on <- 1 / (1 + exp(-(lp_on - lp_off) / temperature))
    want_on <- runif(1) < p_on
    if (want_on != is_on) {
      edges <- toggled
      logp <- toggled_logp
    }
  }
  list(edges = edges, logp = logp)
}

# One MI-biased single-edge Metropolis-Hastings step: add (propose (w, o) with
# probability proportional to mis[w, o]), delete (uniform over current edges),
# or swap-meaning (replace one word for an object with another, proportional to
# mis). Proposal asymmetry is corrected exactly. Port of mh_step().
fgt_mh_step <- function(edges, logp, mis_total, mis, score_full,
                        p_add = 0.4, p_del = 0.4, temperature = 1) {
  n <- length(edges$w)
  has_edge <- function(w, o) any(edges$w == w & edges$o == o)
  mis_in_lex <- if (n) sum(mis[cbind(edges$w, edges$o)]) else 0
  W <- nrow(mis); O <- ncol(mis)

  if (n == 0) {
    move <- "add"
  } else {
    u <- runif(1)
    move <- if (u < p_add) "add" else if (u < p_add + p_del) "del" else "swap"
  }

  if (move == "add") {
    wo <- NULL
    for (attempt in 1:100) {
      flat <- sample.int(W * O, 1, prob = as.vector(t(mis)))
      w <- (flat - 1L) %/% O + 1L
      o <- (flat - 1L) %% O + 1L
      if (!has_edge(w, o)) { wo <- c(w, o); break }
    }
    if (is.null(wo)) return(list(edges = edges, logp = logp))
    w <- wo[1]; o <- wo[2]
    toggled <- list(w = c(edges$w, w), o = c(edges$o, o))
    new_logp <- score_full(fgt_edges_to_map(toggled))

    pa <- if (n == 0) 1 else p_add
    mass_avail <- mis_total - mis_in_lex
    log_q_fwd <- log(pa) + log(mis[w, o]) - log(mass_avail)
    log_q_rev <- log(p_del) - log(n + 1)
    log_mh <- (new_logp - logp) / temperature + (log_q_rev - log_q_fwd)
    if (log(runif(1)) < log_mh) return(list(edges = toggled, logp = new_logp))
    return(list(edges = edges, logp = logp))
  }

  if (move == "del") {
    idx <- sample.int(n, 1)
    w <- edges$w[idx]; o <- edges$o[idx]
    toggled <- list(w = edges$w[-idx], o = edges$o[-idx])
    new_logp <- score_full(fgt_edges_to_map(toggled))

    mis_in_after <- mis_in_lex - mis[w, o]
    mass_avail_after <- mis_total - mis_in_after
    pa_after <- if ((n - 1) == 0) 1 else p_add
    log_q_fwd <- log(p_del) - log(n)
    log_q_rev <- log(pa_after) + log(mis[w, o]) - log(mass_avail_after)
    log_mh <- (new_logp - logp) / temperature + (log_q_rev - log_q_fwd)
    if (log(runif(1)) < log_mh) return(list(edges = toggled, logp = new_logp))
    return(list(edges = edges, logp = logp))
  }

  # swap-meaning: keep the object, swap the word
  objs_in_lex <- unique(edges$o)
  o <- objs_in_lex[sample.int(length(objs_in_lex), 1)]
  slots <- which(edges$o == o)
  slot <- slots[sample.int(length(slots), 1)]
  w_old <- edges$w[slot]

  w_new <- NULL
  col_prob <- mis[, o]
  for (attempt in 1:100) {
    cand <- sample.int(W, 1, prob = col_prob)
    if (cand != w_old && !has_edge(cand, o)) { w_new <- cand; break }
  }
  if (is.null(w_new)) return(list(edges = edges, logp = logp))

  toggled <- edges
  toggled$w[slot] <- w_new
  new_logp <- score_full(fgt_edges_to_map(toggled))
  log_mh <- (new_logp - logp) / temperature +
    log(mis[w_old, o]) - log(mis[w_new, o])
  if (log(runif(1)) < log_mh) return(list(edges = toggled, logp = new_logp))
  list(edges = edges, logp = logp)
}

# Multi-chain sampler. Each recorded draw is preceded by `edge_per` single-edge
# MH steps and `gibbs_per` blocked per-object Gibbs moves; both move families
# leave the posterior invariant. Returns the posterior edge-marginal matrix
# P(edge present | corpus), averaged over chains and draws.
fgt_run_gibbs <- function(corpus, voc_sz, ref_sz, alpha, gamma, kappa,
                          score_full, score_partial,
                          n_chains = 4L, n_warmup = 120L, n_samples = 300L,
                          gibbs_per = 1L, edge_per = 1L, top_k = 6L,
                          seed = 1L, verbose = FALSE) {
  cache_ready <- corpus  # already intent-cached by caller
  mis <- fgt_compute_mis(cache_ready, voc_sz, ref_sz)
  mis_total <- sum(mis)
  cands <- fgt_build_candidates(cache_ready, score_partial, voc_sz, ref_sz,
                                top_k = top_k)

  marg_sum <- matrix(0, voc_sz, ref_sz)
  n_draws <- 0L
  for (ci in seq_len(n_chains)) {
    set.seed(seed + (ci - 1L) * 9973L)
    edges <- list(w = integer(0), o = integer(0))
    logp <- score_full(fgt_edges_to_map(edges))

    sweep_once <- function(edges, logp) {
      for (. in seq_len(edge_per)) {
        st <- fgt_mh_step(edges, logp, mis_total, mis, score_full)
        edges <- st$edges; logp <- st$logp
      }
      for (. in seq_len(gibbs_per)) {
        o <- sample.int(ref_sz, 1)
        st <- fgt_gibbs_object(edges, logp, o, cands[[o]], score_full)
        edges <- st$edges; logp <- st$logp
      }
      list(edges = edges, logp = logp)
    }

    for (. in seq_len(n_warmup)) {
      st <- sweep_once(edges, logp); edges <- st$edges; logp <- st$logp
    }
    for (. in seq_len(n_samples)) {
      st <- sweep_once(edges, logp); edges <- st$edges; logp <- st$logp
      if (length(edges$w)) {
        marg_sum[cbind(edges$w, edges$o)] <- marg_sum[cbind(edges$w, edges$o)] + 1
      }
      n_draws <- n_draws + 1L
    }
    if (verbose) {
      message(sprintf("fgt2009 chain %d: final |L| = %d, log_p = %.2f",
                      ci, length(edges$w), logp))
    }
  }
  # Add-half (Jeffreys) smoothing: a Monte Carlo marginal of exactly 0 is a
  # finite-sample artifact, not a true zero. This also keeps the returned
  # matrix strictly positive, so an empty-lexicon posterior maps to chance
  # performance under get_perf()/mafc_test() rather than a 0/0 NaN.
  (marg_sum + 0.5) / (n_draws + 1)
}

# --------------------------------------------------------------------------
# xslMod model function + constructors
# --------------------------------------------------------------------------

fgt2009_model <- function(params, data, control) {
  alpha <- params[["alpha"]]
  gamma <- params[["gamma"]]
  kappa <- params[["kappa"]]
  use_rsa <- isTRUE(params[["rsa"]])
  rsa_alpha <- params[["rsa_alpha"]] %||% 3
  rsa_depth <- params[["rsa_depth"]] %||% 1L

  n_chains <- params[["n_chains"]] %||% 4L
  n_warmup <- params[["n_warmup"]] %||% 120L
  n_samples <- params[["n_samples"]] %||% 300L
  gibbs_per <- params[["gibbs_per"]] %||% 1L
  edge_per <- params[["edge_per"]] %||% 1L
  top_k <- params[["top_k"]] %||% 6L
  seed <- params[["seed"]] %||% 1L

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc)
  ref_sz <- length(ref)

  corpus <- lapply(seq_along(data$words), function(t) {
    w <- unlist(data$words[[t]]); w <- w[!is.na(w) & w != ""]
    o <- unlist(data$objects[[t]]); o <- o[!is.na(o) & o != ""]
    list(words = match(w, voc), objects = match(o, ref))
  })
  corpus <- Filter(function(s) length(s$words) > 0, corpus)
  corpus <- fgt_build_intent_cache(corpus, gamma)

  if (use_rsa) {
    score_full <- function(map) {
      fgt_score_map_rsa(map, corpus, voc_sz, ref_sz, alpha, kappa,
                        rsa_alpha, rsa_depth)
    }
    score_partial <- function(map, sidx) {
      fgt_score_map_rsa(map, corpus, voc_sz, ref_sz, alpha, kappa,
                        rsa_alpha, rsa_depth, sit_indices = sidx)
    }
  } else {
    score_full <- function(map) {
      fgt_score_map(map, corpus, voc_sz, ref_sz, alpha, kappa)
    }
    score_partial <- function(map, sidx) {
      fgt_score_map(map, corpus, voc_sz, ref_sz, alpha, kappa, sit_indices = sidx)
    }
  }

  marg <- fgt_run_gibbs(corpus, voc_sz, ref_sz, alpha, gamma, kappa,
                        score_full, score_partial,
                        n_chains = n_chains, n_warmup = n_warmup,
                        n_samples = n_samples, gibbs_per = gibbs_per,
                        edge_per = edge_per, top_k = top_k, seed = seed,
                        verbose = isTRUE(control[["verbose"]]))
  rownames(marg) <- voc
  colnames(marg) <- ref

  xslFit(perf = get_perf(marg), matrix = marg, traj = list(marg))
}

#' Frank, Goodman & Tenenbaum (2009) intentional Bayesian word-learning model
#'
#' A joint Bayesian model that infers a *lexicon* -- a set of word-object
#' edges -- from the whole corpus at once, marginalizing over the speaker's
#' latent referential intention on each trial. Words are generated either
#' referentially (probability `gamma`, naming an object in the intention) or
#' non-referentially (`1 - gamma`, with in-lexicon words down-weighted by
#' `kappa`); a geometric prior `P(L) proportional to exp(-alpha |L|)` favors
#' small lexicons. Inference is MCMC (blocked per-object Gibbs plus MI-biased
#' single-edge Metropolis moves), and `xslFit$matrix` holds the resulting
#' posterior edge marginals `P(word names object | corpus)`.
#'
#' This is a **batch** model, unlike the incremental associative models in this
#' package: it has no trial-by-trial learning trajectory, and `control$reps` is
#' ignored. Ported from the Python package `wordlearn`
#' (github.com/mcfrank/wurwur); see also [fgt2009_rsa()] for the pragmatic
#' extension.
#'
#' `alpha` scales with corpus size (larger corpora want a larger `alpha`) and
#' the fit-vs-`alpha` curve is single-peaked, so `alpha` is best chosen with a
#' sweep (see [fgt2009_sweep_alpha()]) rather than a general-purpose optimizer.
#' `gamma` and `kappa` default to the values used for the analogous
#' cross-situational simulation in the source package
#' (`gamma = 1`: every word in a controlled XSL experiment is a referential
#' label, so there is no non-referential channel; `kappa = 0.5`). The
#' naturalistic-speech values are `gamma = 0.1`, `kappa = 0.05`.
#'
#' @param alpha Geometric lexicon-size prior: `P(L)` proportional to
#'   `exp(-alpha |L|)`. Must be tuned to corpus size.
#' @param gamma Probability that a word is used referentially. `1` (the
#'   default) is appropriate when every word heard is a label, as in
#'   controlled XSL experiments.
#' @param kappa Down-weight applied to an in-lexicon word used
#'   non-referentially (only relevant when `gamma < 1`).
#' @param n_chains,n_warmup,n_samples,gibbs_per,edge_per,top_k,seed Sampler
#'   controls. Defaults are tuned for the small, dense corpora of XSL
#'   experiments; raise `n_chains`/`n_samples` to reduce Monte Carlo noise.
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- fgt2009(alpha = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
fgt2009 <- function(alpha, gamma = 1, kappa = 0.5,
                    n_chains = 4L, n_warmup = 120L, n_samples = 300L,
                    gibbs_per = 1L, edge_per = 1L, top_k = 6L, seed = 1L) {
  xslMod(
    name = "fgt2009",
    description = paste(
      "Frank, Goodman & Tenenbaum (2009) intentional Bayesian word-learning",
      "model: joint posterior inference over a word-object lexicon,",
      "marginalizing the speaker's referential intention, with a geometric",
      "prior favoring small lexicons (ported from the wordlearn package)"
    ),
    model = fgt2009_model,
    params = list(alpha = alpha, gamma = gamma, kappa = kappa,
                  rsa = FALSE,
                  n_chains = n_chains, n_warmup = n_warmup, n_samples = n_samples,
                  gibbs_per = gibbs_per, edge_per = edge_per, top_k = top_k,
                  seed = seed),
    stochastic = FALSE
  )
}

#' Frank, Goodman & Tenenbaum word-learning model with pragmatic (RSA) reasoning
#'
#' [fgt2009()] with the literal speaker replaced by a Rational Speech Act
#' pragmatic speaker: a speaker who chooses words to be informative to a
#' listener who reasons about that speaker (Frank & Goodman, 2012; Smith,
#' Goodman & Frank, 2013). The non-referential channel, the `kappa` penalty,
#' and the marginalization over intentions are unchanged, so the literal
#' [fgt2009()] model is the `rsa_depth = 0` special case. The recursion is what
#' produces *strong* mutual exclusivity -- the speaker could have used a
#' different word -- which the literal model underproduces.
#'
#' @inheritParams fgt2009
#' @param rsa_alpha Speaker rationality (softmax temperature) in the RSA
#'   recursion.
#' @param rsa_depth Number of pragmatic recursions; 1 gives the usual
#'   one-step pragmatic listener/speaker.
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- fgt2009_rsa(alpha = 1, rsa_alpha = 3)
#' xsl_run(mod, get_example_ambiguous_condition())
fgt2009_rsa <- function(alpha, gamma = 1, kappa = 0.5,
                        rsa_alpha = 3, rsa_depth = 1L,
                        n_chains = 4L, n_warmup = 120L, n_samples = 300L,
                        gibbs_per = 1L, edge_per = 1L, top_k = 6L, seed = 1L) {
  xslMod(
    name = "fgt2009_rsa",
    description = paste(
      "Frank, Goodman & Tenenbaum word-learning model with a Rational Speech",
      "Act pragmatic speaker layer (Smith, Goodman & Frank, 2013): closes the",
      "strong mutual-exclusivity gap the literal model underproduces (ported",
      "from the wordlearn package)"
    ),
    model = fgt2009_model,
    params = list(alpha = alpha, gamma = gamma, kappa = kappa,
                  rsa = TRUE, rsa_alpha = rsa_alpha, rsa_depth = rsa_depth,
                  n_chains = n_chains, n_warmup = n_warmup, n_samples = n_samples,
                  gibbs_per = gibbs_per, edge_per = edge_per, top_k = top_k,
                  seed = seed),
    stochastic = FALSE
  )
}

#' Sweep the lexicon-size prior alpha for an FGT model
#'
#' `fgt2009()`'s `alpha` trades lexicon size against fit and must scale with
#' corpus size, and the fit-vs-`alpha` curve is single-peaked -- so a coarse
#' sweep is the right way to choose it (and to evaluate the model fairly),
#' rather than a general-purpose optimizer. This runs `xsl_run()` for each
#' `alpha` and returns the per-value SSE against the dataset's human accuracy.
#'
#' For a naturalistic corpus (e.g. [rollins_corpus], [fm_corpus]) there is no
#' human accuracy vector; pass `gold` (a `list(words, objects)` lexicon) to also
#' get the best-threshold F-score of the learned matrix against it (via
#' [get_roc_max()]) at each `alpha`. Remember such corpora also want the
#' naturalistic-speech `gamma = 0.1`, `kappa = 0.05` rather than the defaults
#' (see [fgt2009()]).
#'
#' @param data An `xslData` object (or list of them).
#' @param alphas Numeric vector of `alpha` values to try.
#' @param rsa Logical; if `TRUE`, sweep [fgt2009_rsa()] instead of [fgt2009()].
#' @param gold Optional gold-standard lexicon as `list(words, objects)`. When
#'   given, the result gains an `f_max` column (best-threshold F-score against
#'   `gold`). `data` must be a single `xslData` in this case.
#' @param ... Further arguments to the model constructor (e.g. `gamma`,
#'   `kappa`, sampler controls).
#' @param control Control arguments passed to `xsl_run()`.
#'
#' @return A data frame with columns `alpha` and `sse` (plus `unweighted_sse`
#'   when `data` is a list, or `f_max` when `gold` is given).
#' @export
#'
#' @examples
#' fgt2009_sweep_alpha(get_example_ambiguous_condition(),
#'                     alphas = c(1, 2, 4, 8))
fgt2009_sweep_alpha <- function(data, alphas, rsa = FALSE, gold = NULL, ...,
                                control = xslControl()) {
  ctor <- if (rsa) fgt2009_rsa else fgt2009
  if (!is.null(gold) && !inherits(data, "xslData")) {
    stop("`gold` scoring requires `data` to be a single xslData object")
  }
  rows <- lapply(alphas, function(a) {
    res <- xsl_run(ctor(alpha = a, ...), data, control = control)
    row <- data.frame(alpha = a, sse = res$sse,
                      unweighted_sse = res$unweighted_sse)
    if (!is.null(gold)) {
      row$f_max <- get_roc_max(res$fits[[1]]$matrix, gold_lexicon = gold)
    }
    row
  })
  out <- do.call(rbind, rows)
  if (inherits(data, "xslData")) out$unweighted_sse <- NULL
  out
}
