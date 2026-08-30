test_that("guess_and_test disconfirms a hypothesis once contradicted, rather than keeping it forever", {
  # regression test: an inverted guard clause (`if (length(which(m[w,]==1)))
  # next`) made the disconfirmation step permanently unreachable, since
  # have_hypoths is already filtered to words with a stored hypothesis (so
  # the guard's condition was always true). Word 1 forms some hypothesis
  # from {1,2} on trial 1, then every remaining trial only offers {3,4} --
  # neither 1 nor 2 is ever a candidate again, so the trial-1 hypothesis
  # MUST be disconfirmed (and a new one formed from {3,4}) for the model to
  # ever end up with weight on {3,4}. With the bug, the trial-1 hypothesis
  # is retained forever and {3,4} never receives any weight at all.
  dat <- xslData(
    train = list(words = list(1, 1, 1, 1, 1, 1),
                 objects = list(c(1, 2), c(3, 4), c(3, 4), c(3, 4), c(3, 4), c(3, 4))),
    accuracy = c(0.5),
    label = "disconfirmation test"
  )
  mod <- guess_and_test(f = 0, sa = 1)
  result <- xsl_run(mod, dat, control = xslControl(n_sim = 500))
  m <- result$fits[[1]]$matrix

  expect_equal(sum(m[1, 1:2]), 0, tolerance = 1e-6)
  expect_equal(sum(m[1, 3:4]), 500, tolerance = 1e-6)
})

test_that("uncfam_attention() falls back to uncfam()'s unscaled rate on a single-trial dataset", {
  # uncfam_attention() scales X by this trial's mean object entropy relative
  # to the mean entropy of all objects seen so far. With only one trial ever
  # presented, every object touched on it has just received its first
  # association -- Shannon entropy of a single nonzero cell is 0 -- so both
  # the trial's and the running mean's entropy are 0, hitting the
  # divide-by-zero fallback (scaled_X <- X). The two models should therefore
  # produce identical results whenever there's only one trial.
  dat <- xslData(
    train = list(words = list(c(1, 2)), objects = list(c(1, 2))),
    accuracy = c(0.5, 0.5),
    label = "single trial"
  )
  plain <- xsl_run(uncfam(X = .3, B = .5, C = .9), dat)
  attention <- xsl_run(uncfam_attention(X = .3, B = .5, C = .9), dat)
  expect_equal(attention$fits[[1]]$matrix, plain$fits[[1]]$matrix)
})

test_that("uncfam_predictive()'s unnormalized update never drives an association negative", {
  # regression test: without the pmax(..., 0) floor documented in
  # model-uncfam_predictive.R, a large learning rate can overshoot an
  # association below 0, which inflates the next trial's prediction error
  # and diverges further -- the file's own comment reports this reaching
  # +-1e300 within a 45-trial run for some DEoptim candidate draws. An
  # aggressive (well outside normal fitting bounds) X/B still shouldn't ever
  # produce a negative or non-finite association.
  mod <- uncfam_predictive(X = 50, B = 15, C = 1)
  result <- xsl_run(mod, xsl_datasets[[1]])
  m <- result$fits[[1]]$matrix
  expect_true(all(is.finite(m)))
  expect_true(all(m >= 0))
})

test_that("tilles() carries a word's alpha forward across trials it doesn't appear on", {
  # regression test: alpha was declared fresh (rep(NA, voc_sz)) inside the
  # trial loop and only ever filled in for words on the *current* trial
  # (tr_w). But the "other old" words update (a word seen before, not on
  # this trial) needs alpha[other_old_w] -- words never in tr_w on this
  # trial -- which was therefore always NA, crashing downstream in
  # shannon_entropy's `if`. alpha now persists across trials as per-word
  # state, updated only for the current trial's words, so absent words keep
  # their own last-computed value.
  mod <- tilles(x = .5, b = .8, alpha_0 = .85)
  result <- tryCatch(xsl_run(mod, xsl_datasets[[1]]), error = function(e) e)
  expect_false(inherits(result, "error"))
  expect_true(all(is.finite(result$fits[[1]]$matrix)))
})

test_that("tilles() works on an asymmetric dataset (word count != object count)", {
  # regression test: `r <- matrix(0, voc_sz, voc_sz)` should have been
  # matrix(0, voc_sz, ref_sz) (matching m's own dimensions, and the second
  # r declared later in the same function) -- a copy-paste bug that only
  # breaks assignments like r[w, tr_o] <- ... when an object index exceeds
  # voc_sz, i.e. whenever voc_sz != ref_sz.
  koehne <- Filter(function(d) d$label == "Koehne2013-aaappp", xsl_datasets)[[1]]
  expect_false(length(unique(unlist(koehne$train$words))) ==
                 length(unique(unlist(koehne$train$objects))))
  result <- tryCatch(xsl_run(tilles(x = .5, b = .8, alpha_0 = .85), koehne),
                     error = function(e) e)
  expect_false(inherits(result, "error"))
})

test_that("tilles() doesn't divide by zero when an old word has no prior mass on this trial's objects", {
  # regression test: eq 5.1's flux <- sum(m[w,other_old_o]) / sum(m[w,tr_o])
  # assumes a currently-present, previously-seen word already has some
  # association with at least one of this trial's objects. That's not
  # guaranteed -- a word can be "old" yet paired with an entirely new-to-it
  # set of objects -- giving sum(m[w,tr_o]) == 0 and thus Inf, which then
  # multiplies by m[w,tr_o] == 0 to produce NaN. Reproduces dataset "206"'s
  # trial 3 exactly (word 18/3 reappear paired with objects they've never
  # been associated with), which failed on every one of 40 random parameter
  # draws before the fix.
  cond206 <- Filter(function(d) d$label == "206", xsl_datasets)[[1]]
  result <- tryCatch(xsl_run(tilles(x = .5, b = .5, alpha_0 = .5), cond206),
                     error = function(e) e)
  expect_false(inherits(result, "error"))
  expect_true(all(is.finite(result$fits[[1]]$matrix)))
})

test_that("kalman_filter()'s Kalman gain matches its closed-form definition and shrinks with confidence", {
  # exercises the model's core claim: an adaptive learning rate that starts
  # large under uncertainty and shrinks as confidence accumulates. Hand-
  # derived from the model's own update equations (gain = sigma2/(sigma2 +
  # sigma2_obs); mu <- mu + gain*(1-mu); sigma2 <- (1-gain)*sigma2, with
  # sigma2 += tau2 diffusion each trial before the update).
  dat <- xslData(
    train = list(words = list(c(1, 2), c(1, 2)), objects = list(c(1, 2), c(1, 2))),
    accuracy = c(0.5, 0.5),
    label = "two trial test"
  )
  tau2 <- .05; sigma2_obs <- 1; sigma2_0 <- 1
  mod <- kalman_filter(tau2 = tau2, sigma2_obs = sigma2_obs, sigma2_0 = sigma2_0)
  result <- xsl_run(mod, dat)

  sigma2_1 <- sigma2_0 + tau2
  gain_1 <- sigma2_1 / (sigma2_1 + sigma2_obs)
  mu_1 <- gain_1 # mu starts at 0, target is 1
  sigma2_after_1 <- (1 - gain_1) * sigma2_1

  sigma2_2 <- sigma2_after_1 + tau2
  gain_2 <- sigma2_2 / (sigma2_2 + sigma2_obs)
  mu_2 <- mu_1 + gain_2 * (1 - mu_1)

  expect_equal(unname(result$fits[[1]]$matrix[1, 1]), mu_2)
  expect_true(gain_2 < gain_1) # learning rate shrinks as confidence accumulates
})

test_that("softmax_rl() learns to prefer a consistently-confirmed referent over a rarely-confirmed one", {
  # word 1 always co-occurs with object 1 (always confirmed if guessed) but
  # only co-occurs with object 2 on 1 of 10 trials (rarely confirmed) -- a
  # reasonable learning rate/temperature should end up strongly favoring
  # object 1's Q-value over object 2's, averaged across many simulations
  dat <- xslData(
    train = list(
      words = as.list(rep(1, 10)),
      objects = c(as.list(rep(1, 9)), list(c(1, 2)))
    ),
    accuracy = c(0.9),
    label = "confirmation preference test"
  )
  mod <- softmax_rl(alpha = .3, beta = 3)
  result <- xsl_run(mod, dat, control = xslControl(n_sim = 500))
  q <- result$fits[[1]]$matrix
  expect_true(q[1, 1] > q[1, 2])
})

test_that("softmax_rl() only updates the sampled action, not every co-occurring pair", {
  # regression/design check: unlike every other model in this package,
  # which updates the full word x object cross-product on a trial, this
  # model should only ever move the ONE q-cell it actually sampled and
  # scored -- so across many single-trial simulations, the total number of
  # nonzero q-cells changed should equal (not exceed) the number of words
  # x sims, never words x objects x sims
  dat <- xslData(
    train = list(words = list(c(1, 2)), objects = list(c(1, 2, 3))),
    accuracy = c(0.5, 0.5),
    label = "sparse update test"
  )
  mod <- softmax_rl(alpha = .5, beta = 1)
  result <- xsl_run(mod, dat, control = xslControl(n_sim = 1))
  q <- result$fits[[1]]$matrix
  # exactly one nonzero cell per word (the one it sampled), never both
  # candidates updated for the same word in a single trial
  expect_equal(sum(q[1, ] != 0), 1)
  expect_equal(sum(q[2, ] != 0), 1)
})
