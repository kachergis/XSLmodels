# Re-run of XSIT-MIN/analysis/fit_models.Rmd (Yurovsky & Frank, 2015, Table 3):
# fit the three Stan models to Experiment 1 and score them on Experiments 1-2.
#
# The *.stan files here are the original models/*.e1.stan.R with only syntax
# updated for current Stan (array declarations, `=` for `<-`); the models are
# unchanged. Bootstrap CIs and the figure are omitted, and prop.from.pred()'s
# Monte Carlo (100000 draws) is replaced by its expectation.
#
# TRANSPOSE=1 (default) reproduces the original code, which builds the Stan
# index matrices B_*/E_* as [interval, referents] while the models index them
# as [referents, interval]; TRANSPOSE=0 fixes this.
#
# Run from the package root (needs rstan):
#   TRANSPOSE=1 Rscript data-raw/YurovskyFrank2015/repro/repro_stan.R
#   TRANSPOSE=0 Rscript data-raw/YurovskyFrank2015/repro/repro_stan.R

suppressMessages({library(dplyr); library(readr); library(rstan)})
here <- "data-raw/YurovskyFrank2015"
seed <- as.integer(Sys.getenv("SEED", "1"))
transpose <- Sys.getenv("TRANSPOSE", "1") == "1"
rd <- function(f) read_csv(file.path(here, f), show_col_types = FALSE, name_repair = "unique_quiet")

all.data <- bind_rows(rd("exp1_long.csv"), rd("exp2_long.csv")) |>
  transmute(exp, subid, correct, numPicN, intervalN = intervalN + 1,
            trialType = factor(trialType, levels = c("Same", "Switch", "New Label")))
# participant-level counts, ordered (trialType, intervalN, numPicN, subid) as in the original
mss <- all.data |> group_by(trialType, intervalN, numPicN, subid, exp) |>
  summarise(sum = sum(correct), mean = mean(correct), .groups = "drop") |>
  arrange(trialType, intervalN, numPicN, subid)
e1 <- filter(mss, exp == "Exp 1")
ms <- mss |> group_by(trialType, intervalN, numPicN) |>
  summarise(n = n(), mean = mean(mean), .groups = "drop")
e1.ms <- e1 |> group_by(trialType, intervalN, numPicN) |>
  summarise(n = n(), mean = mean(mean), .groups = "drop")

REFS <- c(2, 3, 4, 8); INTS <- c(1, 2, 4, 8)
index_mat <- function(n, begin) {
  v <- if (begin) head(c(0, cumsum(n)) + 1, -1) else cumsum(n)
  m <- matrix(v, nrow = 4, ncol = 4)   # [referents, interval]: the models' layout
  if (transpose) t(m) else m           # the original code transposed it
}
n_s <- filter(e1.ms, trialType == "Same")$n
n_o <- filter(e1.ms, trialType == "Switch")$n
dat <- list(Trials = 4, PicConds = 4, IntConds = 4, NumPic = REFS, Int = INTS,
            P_s = n_s, P_o = n_o,
            B_s = index_mat(n_s, TRUE), E_s = index_mat(n_s, FALSE),
            B_o = index_mat(n_o, TRUE), E_o = index_mat(n_o, FALSE),
            S = filter(e1, trialType == "Same")$sum, O = filter(e1, trialType == "Switch")$sum)

fit <- function(f) stan(file.path(here, "repro", f), data = dat, iter = 1000, chains = 1,
                        refresh = 0, seed = seed)
fits <- list(accumulation = fit("accumulation.e1.stan"), single_referent = fit("singleref.e1.stan"),
             integrated = fit("integrated.e1.stan"))
post_mean <- function(f, p) mean(rstan::extract(f, p)[[1]])

# model.helpers.R's strength functions; New Label strength = Switch / 4
pred <- function(model, f, n, t, tt) {
  g <- post_mean(f, "gamma"); l <- post_mean(f, "lambda")
  s <- switch(model, integrated = post_mean(f, "sigma"), accumulation = 1 / n, single_referent = 1)
  same <- g * s * t^-l
  other <- if (model == "single_referent") 0 else g * (1 - s) / (n - 1) * t^-l
  m <- switch(as.character(tt), Same = same, Switch = other, "New Label" = other / 4)
  if (tt == "New Label") (1 - m) / n - m else m + (1 - m) / n
}
out <- lapply(names(fits), function(m) {
  f <- fits[[m]]
  p_all <- mapply(function(n, t, tt) pred(m, f, n, t, tt), ms$numPicN, ms$intervalN, ms$trialType)
  p_e1 <- mapply(function(n, t, tt) pred(m, f, n, t, tt), e1.ms$numPicN, e1.ms$intervalN, e1.ms$trialType)
  lp <- post_mean(f, "lp__")
  pars <- sapply(intersect(c("sigma", "lambda", "gamma"), f@model_pars), function(p) post_mean(f, p))
  data.frame(model = m, lp = lp, bic = -2 * lp + length(pars) * log(sum(n_s, n_o)),
             e1_r2 = cor(e1.ms$mean, p_e1)^2, e12_r2 = cor(ms$mean, p_all)^2,
             params = paste(names(pars), round(pars, 4), sep = "=", collapse = " "))
})
cat("TRANSPOSE =", transpose, "\n")
print(do.call(rbind, out), digits = 4)
