# Yurovsky & Frank (2015): the yurovsky_frank2015 data and the closed-form
# models in yf2015_predict() / yf2015_fit(), checked against the paper and the
# original code (github.com/dyurovsky/XSIT-MIN).

d <- yurovsky_frank2015$exp12
grid <- expand.grid(n_referents = c(2, 3, 4, 8), interval = c(1, 2, 4, 8),
                    trial_type = c("Same", "Switch", "New Label"), stringsAsFactors = FALSE)

test_that("yurovsky_frank2015 has the paper's participants", {
  expect_named(yurovsky_frank2015, c("exp12", "exp3"))
  subj <- unique(d[c("experiment", "subject", "n_referents", "interval")])
  expect_equal(as.vector(table(subj$experiment)), c(1196, 803))
  expect_false(anyDuplicated(subj$subject) > 0)
  e1 <- table(subj$n_referents[subj$experiment == 1], subj$interval[subj$experiment == 1])
  e2 <- table(subj$n_referents[subj$experiment == 2], subj$interval[subj$experiment == 2])
  expect_equal(range(e1), c(71, 81))
  expect_equal(range(e2), c(41, 55))
  expect_setequal(unique(d$interval), c(1, 2, 4, 8))
  expect_true(all(table(d$subject, d$trial_type)[, "Same"] == 4))
  expect_equal(sort(unique(as.character(d$trial_type[d$experiment == 1]))), c("Same", "Switch"))
  expect_equal(sort(unique(as.character(d$trial_type[d$experiment == 2]))), c("New Label", "Same"))

  e3 <- yurovsky_frank2015$exp3
  expect_equal(length(unique(e3$subject)), 465)
  expect_equal(length(unique(e3$subject[e3$in_both_conditions])), 8)
  expect_false(any(e3$correct & e3$familiar))
})

test_that("yf2015_predict() matches the original code's formulas", {
  # models/model.helpers.R: integrated.pred(), accumulation.pred(),
  # singleref.pred() and prop.from.pred() (without its Monte Carlo noise)
  sigma <- .6; lambda <- .2; gamma <- 1.5
  for (i in seq_len(nrow(grid))) {
    n <- grid$n_referents[i]; t <- grid$interval[i]; tt <- grid$trial_type[i]
    int <- c(gamma * sigma * t^-lambda, gamma * ((1 - sigma) / (n - 1)) * t^-lambda)
    int <- c(int, int[2] / 4)
    acc <- rep((gamma / n) * t^-lambda, 2); acc <- c(acc, acc[2] / 4)
    sr <- c(gamma * t^-lambda, 0, 0)
    prop <- function(s) c(s[1] + (1 - s[1]) / n, s[2] + (1 - s[2]) / n, (1 - s[3]) / n - s[3])
    j <- match(tt, c("Same", "Switch", "New Label"))
    row <- grid[i, ]
    expect_equal(yf2015_predict(list(sigma = sigma, lambda = lambda, gamma = gamma), row)$p_repeated, prop(int)[j])
    expect_equal(yf2015_predict(c(lambda = lambda, gamma = gamma), row, "accumulation")$p_repeated, prop(acc)[j])
    expect_equal(yf2015_predict(c(lambda = lambda, gamma = gamma), row, "single_referent")$p_repeated, prop(sr)[j])
  }
})

test_that("the special cases are the integrated model at sigma = 1/N and 1", {
  p <- list(lambda = .3, gamma = .8)
  sr <- yf2015_predict(p, grid, "single_referent")
  expect_equal(sr$p_repeated, yf2015_predict(c(p, sigma = 1), grid)$p_repeated)
  acc <- yf2015_predict(p, grid, "accumulation")
  for (n in c(2, 3, 4, 8)) {
    rows <- grid$n_referents == n
    expect_equal(acc$p_repeated[rows], yf2015_predict(c(p, sigma = 1 / n), grid[rows, ])$p_repeated)
  }
  # single-referent tracking is at chance on Switch trials
  sw <- sr$trial_type == "Switch"
  expect_equal(sr$p_repeated[sw], 1 / sr$n_referents[sw])
})

test_that("yf2015_predict() validates its inputs", {
  expect_error(yf2015_predict(list(lambda = .1, gamma = 1), grid), "sigma")
  expect_error(yf2015_predict(list(sigma = .5, lambda = .1, gamma = 1),
                              data.frame(n_referents = 2, interval = 1, trial_type = "Other")),
               "unknown trial_type")
})

test_that("yf2015_fit() reproduces the Stan fits of Experiment 1", {
  # Posterior means from the original Stan models (rstan, 1000 iterations),
  # given correctly indexed data (see PROVENANCE.md), and the r^2s of the
  # paper's Table 3 for those parameters.
  stan <- list(accumulation = c(lambda = .1014, gamma = 1.7687),
               single_referent = c(lambda = .1719, gamma = .9198),
               integrated = c(sigma = .5677, lambda = .1668, gamma = 1.6175))
  r2 <- list(accumulation = c(.319, .672), single_referent = c(.834, .771),
             integrated = c(.951, .975))
  fits <- lapply(names(stan), function(m) yf2015_fit(d, m, seed = 1))
  names(fits) <- names(stan)
  for (m in names(stan)) {
    expect_equal(fits[[m]]$params, stan[[m]], tolerance = .02, info = m)
    expect_equal(unname(fits[[m]]$r2), r2[[m]], tolerance = .005, info = m)
    expect_equal(fits[[m]]$n_obs, 2392)
  }
  # the integrated model wins by a wide margin
  bic <- sapply(fits, `[[`, "bic")
  expect_equal(names(which.min(bic)), "integrated")
  expect_gt(min(bic[c("accumulation", "single_referent")]) - bic[["integrated"]], 900)

  cond <- fits$integrated$conditions
  expect_equal(nrow(cond), 48)
  expect_equal(sum(cond$fitted), 32)
  expect_true(all(cond$predicted > 0 & cond$predicted < 1))
})

test_that("fitting transposed conditions reproduces the paper's published fit", {
  # The original code scored each participant's data as the condition with
  # its number of referents and interval swapped (by level index). Fitting
  # data relabelled that way recovers the paper's estimates and Table 3 r^2s.
  lv <- c(2, 3, 4, 8); iv <- c(1, 2, 4, 8)
  tp <- d
  tp$n_referents <- lv[match(d$interval, iv)]
  tp$interval <- iv[match(d$n_referents, lv)]
  fit <- yf2015_fit(tp, "integrated", seed = 1)
  # the original Stan posterior means
  expect_equal(fit$params, c(sigma = .604, lambda = .184, gamma = 1.495), tolerance = .02)
  truth <- yf2015_fit(d, "integrated", seed = 1)$conditions
  pred <- yf2015_predict(fit$params, truth)$p_repeated
  expect_equal(round(cor(truth$observed[truth$fitted], pred[truth$fitted])^2, 2), .95)
  expect_equal(round(cor(truth$observed, pred)^2, 2), .97)
})

test_that("yf2015_fit() can fit both experiments and is reproducible", {
  f1 <- yf2015_fit(d, "integrated", experiments = 1:2, seed = 3, n_starts = 3)
  f2 <- yf2015_fit(d, "integrated", experiments = 1:2, seed = 3, n_starts = 3)
  expect_identical(f1, f2)
  expect_equal(f1$n_obs, 2392 + 1606)
  expect_true(all(f1$conditions$fitted))
  expect_equal(f1$r2[["fitted"]], f1$r2[["all"]])
  expect_error(yf2015_fit(d, experiments = 3), "no data")
})
