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
