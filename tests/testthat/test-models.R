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
