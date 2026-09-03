# Fit each model's parameters to each bundled corpus by maximizing the
# best-threshold F-score of the learned lexicon against the corpus gold
# lexicon.
#
# The bundled corpora have no human accuracy vector, so xsl_fit()'s
# SSE-against-accuracy objective does not apply -- this runs a parallel
# DEoptim search with objective 1 - get_roc_max(matrix, gold). Companion to
# corpus_gold_comparison.R, which uses each model's *default* (registry)
# parameters; here they are fit per corpus.
#
# Models fit:
#   - all deterministic associative models (one xsl_run() per DEoptim eval)
#   - the cheap stochastic models propose_but_verify / guess_and_test /
#     softmax_rl (N_SIM simulations averaged per eval)
#   - fgt2009() by a coarse alpha sweep (a single run is itself an MCMC fit)
# Left at defaults: uncfam_sampling / multi_sampling (their per-simulation
# cost on FM makes a DEoptim search impractical -- ~60 h each) and pursuit.
#
# DEoptim runs in parallel over a local cluster (N_CORES). On this machine the
# whole run is a few hours; the FM stochastic fits dominate. Run from the
# package root against the *installed* package:
#   R CMD INSTALL . && Rscript tests/bakeoff_comparison/corpus_gold_fits.R
# Writes: corpus_gold_fits.rds, corpus_gold_fits.png (this dir).

suppressMessages({
  library(XSLmodels)
  library(DEoptim)
  library(parallel)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

set.seed(1)
out_dir <- "tests/bakeoff_comparison"

# DEoptim's managed parallel backend sizes its cluster from
# parallelly::availableCores(), which honours options(mc.cores).
N_CORES <- max(1L, min(6L, parallel::detectCores() - 2L))
options(mc.cores = N_CORES)
N_SIM <- 60L   # simulations averaged per DEoptim eval, stochastic models

corpora <- list(Rollins = rollins_corpus, FM = fm_corpus)

det_models   <- c("decay", "uncfam", "uncfam_attention", "uncfam_predictive",
                  "fazly", "rescorla_wagner", "tilles", "bayesian_decay",
                  "kalman_filter")
stoch_models <- c("propose_but_verify", "guess_and_test", "softmax_rl")

# --- objective (evaluated on the parallel workers) ----------------------
# Per-fit state lives in globals so DEoptim can re-export it each call
# (parVar); the workers only have the installed package, hence XSLmodels::.

GF_BASE <- NULL; GF_DATA <- NULL; GF_GOLD <- NULL; GF_NSIM <- 1L; GF_DN <- NULL

gold_obj <- function(par) {
  m <- tryCatch(
    suppressWarnings(XSLmodels::xsl_run(
      XSLmodels::update_params(GF_BASE, par), GF_DATA,
      control = XSLmodels::xslControl(n_sim = GF_NSIM))$fits[[1]]$matrix),
    error = function(e) NULL)
  if (is.null(m) || !any(is.finite(m)) || all(m == m[1])) return(2)
  if (is.null(dimnames(m))) dimnames(m) <- GF_DN
  f <- tryCatch(XSLmodels::get_roc_max(m, gold_lexicon = GF_GOLD),
                error = function(e) NA_real_)
  if (is.finite(f)) 1 - f else 2       # minimize; 2 is worse than any real F
}

score_gold <- function(model, corpus, n_sim = 1L) {
  m <- tryCatch(suppressWarnings(
    xsl_run(model, corpus$data, control = xslControl(n_sim = n_sim))$fits[[1]]$matrix),
    error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  if (is.null(dimnames(m)))
    dimnames(m) <- list(sort(unique(unlist(corpus$data$train$words))),
                        sort(unique(unlist(corpus$data$train$objects))))
  f <- tryCatch(get_roc_max(m, gold_lexicon = corpus$gold), error = function(e) NA_real_)
  if (is.finite(f)) f else NA_real_
}

fit_model <- function(model_name, corpus, n_sim) {
  entry <- XSLmodels:::get_model_registry_entry(model_name)
  base  <- entry$constructor()
  n_par <- length(entry$lower)
  par_names <- names(base$params)[seq_len(n_par)]

  GF_BASE <<- base
  GF_DATA <<- corpus$data
  GF_GOLD <<- corpus$gold
  GF_NSIM <<- as.integer(n_sim)
  GF_DN   <<- list(sort(unique(unlist(corpus$data$train$words))),
                   sort(unique(unlist(corpus$data$train$objects))))

  ctrl <- DEoptim::DEoptim.control(
    NP = max(20L, 10L * n_par), itermax = 25L, reltol = 5e-3, steptol = 8L,
    trace = FALSE, parallelType = "parallel",
    parVar = c("GF_BASE", "GF_DATA", "GF_GOLD", "GF_NSIM", "GF_DN"),
    packages = list("XSLmodels"))
  res <- DEoptim::DEoptim(gold_obj, entry$lower, entry$upper, ctrl)

  tibble(
    model     = model_name,
    f_default = score_gold(base, corpus, n_sim),
    f_fitted  = 1 - res$optim$bestval,
    params    = paste(par_names, signif(res$optim$bestmem, 4), sep = "=",
                      collapse = ", ")
  )
}

# --- run -----------------------------------------------------------------

run_set <- function(model_names, n_sim) {
  lapply(names(corpora), function(cn) {
    lapply(model_names, function(mn) {
      message(sprintf("[%s] fitting %s (n_sim = %d) ...", cn, mn, n_sim))
      t0 <- Sys.time()
      out <- fit_model(mn, corpora[[cn]], n_sim)
      out$corpus <- cn
      message(sprintf("    default F = %.3f  ->  fitted F = %.3f  (%s)  [%.0f s]",
                      out$f_default, out$f_fitted, out$params,
                      as.numeric(Sys.time() - t0, units = "secs")))
      out
    }) |> bind_rows()
  }) |> bind_rows()
}

results <- bind_rows(run_set(det_models, 1L), run_set(stoch_models, N_SIM))

# fgt2009(): coarse alpha sweep (serial)
fgt_rows <- lapply(names(corpora), function(cn) {
  corpus <- corpora[[cn]]
  n <- length(corpus$data$train$words)
  alphas <- sort(unique(round(c(1, 2, 3, 5, 8, 12, 20) *
                                 max(1, round(sqrt(n / 600))))))
  message(sprintf("[%s] sweeping fgt2009 alpha over %s ...",
                  cn, paste(alphas, collapse = ", ")))
  sw <- fgt2009_sweep_alpha(corpus$data, alphas = alphas, gold = corpus$gold,
                            gamma = 0.1, kappa = 0.05,
                            n_warmup = 60, n_samples = 150)
  best <- sw[which.max(sw$f_max), ]
  tibble(model = "fgt2009", corpus = cn, f_default = NA_real_,
         f_fitted = best$f_max,
         params = sprintf("alpha=%g (gamma=0.1, kappa=0.05)", best$alpha))
}) |> bind_rows()

results <- bind_rows(results, fgt_rows) |>
  select(corpus, model, f_default, f_fitted, params) |>
  arrange(corpus, desc(f_fitted))

print(as.data.frame(results), row.names = FALSE)
saveRDS(results, file.path(out_dir, "corpus_gold_fits.rds"))

# --- plot: default vs fitted -------------------------------------------

plot_df <- results |>
  pivot_longer(c(f_default, f_fitted), names_to = "which", values_to = "F") |>
  mutate(which = factor(recode(which, f_default = "registry default",
                               f_fitted = "fit to corpus"),
                        levels = c("registry default", "fit to corpus")),
         corpus = factor(corpus, levels = c("Rollins", "FM")),
         model = factor(model, levels = results |>
                          group_by(model) |> summarise(m = mean(f_fitted)) |>
                          arrange(m) |> pull(model)))

p <- ggplot(plot_df, aes(model, F, fill = which)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~corpus) +
  labs(x = NULL, y = "best-threshold F (learned lexicon vs. gold)", fill = NULL,
       title = "Registry defaults vs. fit to each corpus",
       subtitle = "fgt2009: best of an alpha sweep (no default bar)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "corpus_gold_fits.png"), p,
       width = 9, height = 6, dpi = 120)

message("wrote corpus_gold_fits.{rds,png}")
