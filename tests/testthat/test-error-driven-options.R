# rescorla_wagner()'s and kalman_filter()'s options (absent outcomes, cue
# direction, full covariance) and the softmax choice rule (phi).

# Rescorla-Wagner written out cell by cell, with every option.
rw_ref <- function(train, C, beta, outcomes = "present", cues = "words") {
  voc <- sort(unique(unlist(train$words))); ref <- sort(unique(unlist(train$objects)))
  m <- matrix(0, length(voc), length(ref), dimnames = list(voc, ref))
  for (t in seq_along(train$words)) {
    w <- as.character(train$words[[t]]); o <- as.character(train$objects[[t]])
    if (cues == "words") {
      outs <- if (outcomes == "all") colnames(m) else o
      err <- vapply(outs, \(oj) (oj %in% o) - sum(m[w, oj]), numeric(1))
      for (oj in outs) for (wi in w) m[wi, oj] <- m[wi, oj] + beta * err[[oj]]
    } else {
      outs <- if (outcomes == "all") rownames(m) else w
      err <- vapply(outs, \(wj) (wj %in% w) - sum(m[wj, o]), numeric(1))
      for (wj in outs) for (oi in o) m[wj, oi] <- m[wj, oi] + beta * err[[wj]]
    }
    m <- m * C
  }
  m
}

# The full-covariance Kalman filter written out per object.
kf_ref <- function(train, tau2, s_obs, s0, outcomes = "present") {
  voc <- sort(unique(unlist(train$words))); ref <- sort(unique(unlist(train$objects)))
  W <- matrix(0, length(voc), length(ref), dimnames = list(voc, ref))
  S <- lapply(ref, \(r) diag(s0, length(voc)))
  for (t in seq_along(train$words)) {
    x <- as.numeric(voc %in% train$words[[t]])
    for (j in seq_along(ref)) {
      S[[j]] <- S[[j]] + diag(tau2, length(voc))
      present <- ref[j] %in% train$objects[[t]]
      if (outcomes == "present" && !present) next
      Sx <- drop(S[[j]] %*% x)
      k <- Sx / (sum(x * Sx) + s_obs)
      W[, j] <- W[, j] + k * (present - sum(x * W[, j]))
      S[[j]] <- S[[j]] - tcrossprod(k, Sx)
    }
  }
  W
}

run_mod <- function(mod, train) mod$model(mod$params, train, xslControl())$matrix
conds <- lapply(xsl_datasets[c(1, 20, 30)], \(d) positional_train(d$train)$train)

test_that("rescorla_wagner()'s outcome and cue options match a cell-by-cell implementation", {
  for (tr in conds) for (outcomes in c("present", "all")) for (cues in c("words", "objects")) {
    expect_equal(unname(run_mod(rescorla_wagner(C = .97, alpha = 1, beta = .2, lambda = 1, outcomes = outcomes, cues = cues), tr)),
                 unname(rw_ref(tr, C = .97, beta = .2, outcomes = outcomes, cues = cues)), tolerance = 1e-12,
                 info = paste(outcomes, cues))
  }
})

test_that("with outcomes = 'all', a word's association with an absent object weakens", {
  train <- list(words = list("A", "A"), objects = list("X", "Y"))
  present <- run_mod(rescorla_wagner(C = 1, alpha = 1, beta = .5, lambda = 1), train)
  all <- run_mod(rescorla_wagner(C = 1, alpha = 1, beta = .5, lambda = 1, outcomes = "all"), train)
  expect_equal(present["A", "X"], .5)
  expect_equal(all["A", "X"], .25) # X absent on trial 2: .5 + .5 * (0 - .5)
  expect_equal(all["A", "Y"], .5)
})

test_that("the full-covariance Kalman filter matches a per-object implementation", {
  for (tr in conds) for (outcomes in c("present", "all")) {
    expect_equal(unname(run_mod(kalman_filter(tau2 = .01, sigma2_obs = 1, sigma2_0 = .5, covariance = "full", outcomes = outcomes), tr)),
                 unname(kf_ref(tr, tau2 = .01, s_obs = 1, s0 = .5, outcomes = outcomes)), tolerance = 1e-10,
                 info = outcomes)
  }
})

test_that("the full-covariance filter shows backward blocking; the independent one can't", {
  # A and B appear together with X; then A appears with X alone. With full
  # covariance, A and B's weights are negatively correlated, so confirming A
  # lowers B's association with X. Independent filters never touch B again.
  train <- list(words = c(rep(list(c("A", "B")), 4), rep(list("A"), 6)),
                objects = c(rep(list("X"), 4), rep(list("X"), 6)))
  b_after <- function(covariance) {
    mod <- kalman_filter(tau2 = 0, sigma2_obs = 1, sigma2_0 = 1, covariance = covariance)
    full_run <- run_mod(mod, train)["B", "X"]
    first <- run_mod(mod, lapply(train, \(x) x[1:4]))["B", "X"]
    full_run - first
  }
  expect_lt(b_after("full"), -.05)
  expect_equal(b_after("independent"), 0)
})

test_that("kalman_filter()'s independent filters can also learn about absent objects", {
  train <- list(words = list("A", "A"), objects = list("X", "Y"))
  m <- run_mod(kalman_filter(tau2 = 0, sigma2_obs = 1, sigma2_0 = 1, outcomes = "all"), train)
  # trial 1: A-X gains .5 (variance 1 -> .5); A-Y, absent, is observed as 0,
  # so its variance also falls to .5. Trial 2: both gains are 1/3, X toward 0
  # and Y toward 1
  expect_equal(m["A", "X"], .5 - .5 / 3)
  expect_equal(m["A", "Y"], 1 / 3)
})

test_that("softmax_weights() turns Luce choice into softmax choice", {
  m <- matrix(c(2, -1, 0, 1, 3, -2), 2, dimnames = list(c("a", "b"), c("x", "y", "z")))
  w <- softmax_weights(m, phi = 1.5)
  expect_equal(dimnames(w), dimnames(m))
  expect_equal(w / rowSums(w), exp(1.5 * m) / rowSums(exp(1.5 * m)))
  expect_equal(unname(get_perf(softmax_weights(m, 0))), c(1 / 3, 1 / 3))
  # no overflow at large phi: each word picks its strongest object (a: z,
  # so its diagonal x gets 0; b: y, its diagonal)
  expect_equal(unname(get_perf(softmax_weights(m * 1000, 5))), c(0, 1))
  expect_error(softmax_weights(m, -1))
})

test_that("xsl_run() scores a model with phi by softmax on every path", {
  mod <- rescorla_wagner(C = .98, alpha = 1, beta = .3, lambda = 1, outcomes = "all", phi = 4)
  # all objects as alternatives
  d <- xsl_datasets[[1]]
  f <- xsl_run(mod, d)$fits[[1]]
  expect_equal(f$perf, get_perf(softmax_weights(f$matrix, 4)))
  expect_false(isTRUE(all.equal(f$perf, get_perf(f$matrix))))
  # listed test alternatives, and choice_probabilities() agrees
  d4 <- Filter(\(x) length(x$test) > 0, xsl_datasets)[[1]]
  f4 <- xsl_run(mod, d4)$fits[[1]]
  ch <- choice_probabilities(f4$matrix, d4, phi = 4)
  p_correct <- vapply(seq_len(nrow(ch)), \(i) ch$p[[i]][match(ch$correct[i], ch$alternatives[[i]])], numeric(1))
  expect_equal(p_correct, f4$perf)
  # the likelihood uses the same choice rule
  ll <- xsl_run(mod, d4, xslControl(objective = "multinomial", lapse = .05))$loglik
  ch_l <- choice_probabilities(f4$matrix, d4, phi = 4, lapse = .05)
  expect_equal(ll, sum(mapply(\(p, k) stats::dmultinom(k, prob = p, log = TRUE), ch_l$p, ch_l$counts)))
  # the model's own perf uses it too
  expect_equal(mod$model(mod$params, positional_train(d$train)$train, xslControl())$perf[1, ],
               unname(f$perf))
})

test_that("phi is fit like any other parameter", {
  mod <- kalman_filter(tau2 = .001, sigma2_obs = 1, sigma2_0 = .5, covariance = "full", outcomes = "all", phi = 5)
  fit <- xsl_fit(mod, xsl_datasets[1:2], free = c("sigma2_0", "phi"), lower = c(.01, .5), upper = c(5, 20),
                 deoptim_control = DEoptim::DEoptim.control(NP = 20, itermax = 2, trace = FALSE), seed = 1)[[1]]
  expect_named(fit$optim$bestmem, c("sigma2_0", "phi"))
  expect_true(is.finite(fit$optim$bestval))
})
