# An independent, literal transcription of Tilles & Fontanari (2013), eqs 1-13
# and the procedure summarized on p. 4, used to check tilles() in
# test-models.R. It is written from the paper rather than from
# R/model-tilles.R: sets are built from scratch on each trial, eq 6 uses the
# natural log of the unnormalized confidences, and every word's alpha is
# recomputed before each trial. The paper's contexts have C words and C
# objects; for contexts with more objects than words, the C of eq 1 and the
# C~_t of eq 2 count objects, and a new word that appears with no new objects
# gives eq 2's beta share to all of the trial's objects. When a word has no
# confidence on the objects that eq 5 or eq 11 is proportional to, the flux is
# distributed evenly over them, as the paper first describes eq 5 (p. 3); if
# no object is absent, eq 13's beta outflow has nowhere to go and is skipped.
# `train` must use positional labels (1..N words, 1..N objects).
literal_tilles <- function(chi, beta, alpha0, train) {
  W <- lapply(train$words, function(x) { x <- unlist(x); x[!is.na(x)] })
  O <- lapply(train$objects, function(x) { x <- unlist(x); x[!is.na(x)] })
  P <- matrix(0, max(unlist(W)), max(unlist(O)))
  P[W[[1]], O[[1]]] <- 1 / length(O[[1]]) # eq 1
  seen_w <- W[[1]]
  seen_o <- O[[1]]
  for (t in 2:length(W)) {
    ctx_w <- W[[t]]
    ctx_o <- O[[t]]
    new_w <- setdiff(ctx_w, seen_w)
    new_o <- setdiff(ctx_o, seen_o)
    bar_w <- setdiff(seen_w, ctx_w) # seen before, absent from this context
    bar_o <- setdiff(seen_o, ctx_o)
    all_o <- union(seen_o, ctx_o)
    N_t <- length(all_o)
    # eqs 2-4: P_{t-1} for the new words
    me_o <- if (length(new_o) > 0) new_o else ctx_o
    for (w in new_w) {
      P[w, all_o] <- (1 - beta) / N_t
      P[w, me_o] <- beta / length(me_o) + (1 - beta) / N_t
    }
    # eqs 6-7: alpha_{t-1} for every word seen so far, including the new ones
    alpha <- numeric(nrow(P))
    for (w in union(seen_w, new_w)) {
      p <- P[w, all_o]
      p <- p[p > 0]
      H <- -sum(p * log(p))
      alpha[w] <- alpha0 + (1 - alpha0) * (1 - if (N_t > 1) H / log(N_t) else 0)
    }
    Q <- P
    for (w in ctx_w) { # eqs 5, 8, 9
      a <- alpha[w]
      in_ctx <- sum(P[w, ctx_o])
      r <- if (in_ctx == 0) chi * sum(P[w, bar_o]) / length(ctx_o) else
        chi * P[w, ctx_o] * sum(P[w, bar_o]) / in_ctx
      Q[w, ctx_o] <- P[w, ctx_o] + a * r + (1 - a) * (1 / N_t - P[w, ctx_o])
      Q[w, bar_o] <- P[w, bar_o] - a * chi * P[w, bar_o] + (1 - a) * (1 / N_t - P[w, bar_o])
    }
    for (w in bar_w) { # eqs 11-13
      a <- alpha[w]
      out_ctx <- sum(P[w, bar_o])
      r_bar <- if (out_ctx == 0) beta * sum(P[w, ctx_o]) / length(bar_o) else
        beta * P[w, bar_o] * sum(P[w, ctx_o]) / out_ctx
      beta_out <- if (length(bar_o) > 0) beta else 0
      Q[w, bar_o] <- P[w, bar_o] + a * r_bar + (1 - a) * (1 / N_t - P[w, bar_o])
      Q[w, ctx_o] <- P[w, ctx_o] - a * beta_out * P[w, ctx_o] + (1 - a) * (1 / N_t - P[w, ctx_o])
    }
    P <- Q
    seen_w <- union(seen_w, ctx_w)
    seen_o <- all_o
  }
  P
}
