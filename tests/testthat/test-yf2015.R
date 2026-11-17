# The trial-level Yurovsky & Frank (2015) model, yf2015(), and the
# remember-or-guess choice rule it uses.

fit_params <- list(sigma = .5677, lambda = .1668, gamma = 1.6175) # fixed-index MLE

test_that("remember_guess() is the paper's Eq. 5, and Luce choice above a total strength of 1", {
  m <- matrix(c(.6, .1, 0, 0, 0, .5), 2, byrow = TRUE,
              dimnames = list(c("A", "B"), c("a1", "a2", "b1")))
  expect_equal(remember_guess(m, "A", c("a1", "x", "y")), c(.6 + .4 / 3, .4 / 3, .4 / 3))
  expect_equal(remember_guess(m, "A", c("a1", "a2")), c(.6 + .3 / 2, .1 + .3 / 2))
  expect_equal(remember_guess(m, 1, 1:2), remember_guess(m, "A", c("a1", "a2")))
  # sums to 1; continuous at a total strength of 1; Luce above it
  expect_equal(sum(remember_guess(m, "B", c("a2", "b1", "x"))), 1)
  m2 <- m; m2["A", ] <- c(.75, .25, 0)
  expect_equal(remember_guess(m2, "A", c("a1", "a2")), c(.75, .25))
  m2["A", ] <- c(1.5, .5, 0)
  expect_equal(remember_guess(m2, "A", c("a1", "a2")), c(.75, .25))
  # a novel word guesses: uniformly, or avoiding named objects with mu > 0
  expect_equal(remember_guess(m, "C", c("b1", "x")), c(.5, .5))
  p <- remember_guess(m, "C", c("b1", "x"), mu = 2)
  expect_equal(p, c(exp(-1), 1) / (1 + exp(-1)))
  # a zero prior weight excludes an object
  expect_equal(remember_guess(m, "A", c("a1", "a2", "x"), prior = c(1, 0, 1)), c(.6 + .4 / 2, 0, .4 / 2))
  expect_error(remember_guess(m, "A", character(0)), "non-empty")
  expect_error(remember_guess(m, "A", "a1", mu = -1))
})

test_that("the remember-or-guess rule is threaded through get_perf(), mafc_test() and choice_probabilities()", {
  m <- matrix(c(.5, .1, .2, .3), 2, dimnames = list(1:2, 1:2))
  expect_equal(get_perf(m, rule = "remember_guess"), c("1" = .5 + .3 / 2, "2" = .3 + .6 / 2))
  expect_equal(get_perf(m), get_perf(m, rule = "luce"))
  test <- list(words = list("1", "2"), objects = list(c("1", "2"), c("2", "9")))
  expect_equal(mafc_test(m, test, rule = "remember_guess"), c(.65, .3 + .7 / 2))
  expect_error(predict_referent(m, 1, 1:2, pragmatic = TRUE, rule = "remember_guess"), "pragmatic")
  d <- xsl_datasets[[1]]
  mod <- yf2015(sigma = .5, lambda = .2, gamma = 1.5, sampling = FALSE)
  fit <- xsl_run(mod, d)$fits[[1]]
  ch <- choice_probabilities(fit$matrix, d, rule = "remember_guess")
  expect_equal(vapply(seq_along(ch$p), \(i) ch$p[[i]][match(ch$correct[i], ch$alternatives[[i]])], 0),
               unname(fit$perf))
  # a model without choice_rule is scored by Luce choice, as before
  expect_equal(choice_args(uncfam(X = .1, B = 3, C = .97)$params), list(d = NULL, rule = "luce", mu = 0))
  # mu is read only under the remember-or-guess rule
  expect_equal(choice_args(list(mu = 3))$mu, 0)
})

test_that("yf2015_paradigm() gives the closed-form Integrated model's Same and Switch predictions", {
  grid <- expand.grid(n_referents = c(2, 3, 4, 8), interval = c(1, 2, 4, 8),
                      trial_type = c("Same", "Switch"), stringsAsFactors = FALSE)
  for (p in list(fit_params, list(sigma = .9, lambda = .5, gamma = .8))) {
    cf <- yf2015_predict(p, grid)
    stopifnot(all(cf$strength <= 1))
    tl <- yf2015_paradigm(do.call(yf2015, p), grid)
    expect_equal(tl$p_target, cf$p_repeated)
    expect_true(all(is.na(tl$p_competitor)))
  }
  # special cases: sigma = 1 is single-referent tracking, sigma = 1/N accumulation
  g4 <- grid[grid$n_referents == 4, ]
  expect_equal(yf2015_paradigm(yf2015(1, .2, .9), g4)$p_target,
               yf2015_predict(list(lambda = .2, gamma = .9), g4, "single_referent")$p_repeated)
  expect_equal(yf2015_paradigm(yf2015(1 / 4, .2, .9), g4)$p_target,
               yf2015_predict(list(lambda = .2, gamma = .9), g4, "accumulation")$p_repeated)
})

test_that("mutual exclusivity (mu) drives New Label and Experiment 3 competitor effects", {
  nl <- data.frame(n_referents = c(2, 4), interval = 1, trial_type = "New Label")
  expect_equal(yf2015_paradigm(do.call(yf2015, fit_params), nl)$p_target, c(1 / 2, 1 / 4))
  me <- do.call(yf2015, c(fit_params, mu = 2))
  expect_true(all(yf2015_paradigm(me, nl)$p_target < c(1 / 2, 1 / 4)))

  e3 <- expand.grid(n_referents = c(2, 4), trial_type = yf3_types, stringsAsFactors = FALSE)
  no_me <- yf2015_paradigm(do.call(yf2015, fit_params), e3)
  # without mutual exclusivity, the competitor's identity doesn't matter
  by_target <- split(no_me$p_target, paste(no_me$n_referents, substr(no_me$trial_type, 1, 4)))
  expect_true(all(vapply(by_target, \(x) diff(range(x)) < 1e-12, TRUE)))
  out <- yf2015_paradigm(me, e3)
  expect_equal(out$p_target + out$p_competitor <= 1 + 1e-12, rep(TRUE, nrow(e3)))
  # a competitor the learner hypothesized for B is avoided more than one it
  # didn't hypothesize (as in the data: Same-competitor trials are easier)
  same_comp <- out[grepl("vs. Same", out$trial_type), ]
  switch_comp <- out[grepl("vs. Switch", out$trial_type), ]
  expect_true(all(same_comp$p_competitor < switch_comp$p_competitor))
  expect_true(all(same_comp$p_target > switch_comp$p_target))

  expect_error(yf2015_paradigm(uncfam(X = .1, B = 3, C = .97), nl), "yf2015")
  expect_error(yf2015_paradigm(me, data.frame(n_referents = 2, trial_type = "Same")), "interval")
  expect_error(yf2015_paradigm(me, data.frame(n_referents = 2, interval = 1, trial_type = "Nope")), "unknown")
})

test_that("yf2015() encodes sigma to the hypothesis and decays traces as a power law", {
  # one exposure of word 1 with 3 objects, then two empty trials: tested at interval 3
  data <- list(words = list(1, NA, NA), objects = list(1:3, NA, NA))
  set.seed(1)
  s <- yf2015(.6, .2, 1.5)$model(yf2015(.6, .2, 1.5)$params, data, xslControl())
  expect_equal(sort(unname(s$matrix[1, ])), sort(c(1.5 * .6, 1.5 * .2, 1.5 * .2) * 3^-.2))
  mf <- yf2015(.6, .2, 1.5, sampling = FALSE)
  expect_equal(unname(mf$model(mf$params, data, xslControl())$matrix[1, ]), rep(1.5 / 3 * 3^-.2, 3))
  # a lone object gets all of gamma; traces sum
  data <- list(words = list(1, 1), objects = list(2, 2))
  s <- mf$model(mf$params, data, xslControl())
  expect_equal(unname(s$matrix[1, 1]), 1.5 * 2^-.2 + 1.5)
})

test_that("yf2015()'s mean field is the sampled model's expectation on one-word trials", {
  # one word per trial, mu = 0 and strengths summing to < 1: the choice rule
  # is linear, so expected traces are exact
  data <- list(words = list(1, 2, 1, 2, 1), objects = list(1:3, c(2, 4), c(1, 4), 3:4, 1:2))
  mf <- yf2015(.8, .3, .4, sampling = FALSE)
  exact <- mf$model(mf$params, data, xslControl())$matrix
  st <- yf2015(.8, .3, .4)
  set.seed(3)
  sims <- replicate(4000, st$model(st$params, data, xslControl())$matrix)
  expect_equal(apply(sims, 1:2, mean), exact, tolerance = .01)
})

test_that("yf2015() hypothesizes distinct objects for the words on a trial", {
  # sigma = 1: each word's traces are all on its hypothesis
  mod <- yf2015(1, 0, .5)
  set.seed(1)
  for (i in 1:20) {
    m <- mod$model(mod$params, list(words = list(1:2), objects = list(1:2)), xslControl())$matrix
    expect_equal(sum(m[1, ] * m[2, ]), 0)
  }
})

test_that("yf2015() runs on xsl_datasets with valid predictions, in both modes", {
  for (sampling in c(FALSE, TRUE)) {
    set.seed(1)
    res <- xsl_run(yf2015(.6, .2, 1.5, mu = 1, sampling = sampling), xsl_datasets[1:5],
                   control = xslControl(n_sim = 3, objective = "multinomial"))
    for (f in res$fits) {
      expect_true(all(f$perf >= 0 & f$perf <= 1))
      expect_true(is.finite(f$loglik))
    }
  }
  expect_false(yf2015(.5, .2, 1, sampling = FALSE)$stochastic)
  expect_error(yf2015(1.5, .2, 1))
  expect_error(yf2015(.5, -1, 1))
})
