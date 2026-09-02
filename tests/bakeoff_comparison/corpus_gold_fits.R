# Fit each non-sampling (deterministic) model's parameters to each bundled
# corpus by maximizing the best-threshold F-score of the learned lexicon
# against the corpus gold lexicon.
#
# The bundled corpora have no human accuracy vector, so xsl_fit()'s
# SSE-against-accuracy objective does not apply -- this uses a DEoptim search
# with objective 1 - get_roc_max(matrix, gold). Companion to
# corpus_gold_comparison.R, which uses each model's *default* (registry)
# parameters; here they are fit per corpus.
#
# The stochastic sampling/RL models (uncfam_sampling, multi_sampling,
# propose_but_verify, pursuit, guess_and_test, softmax_rl) are excluded --
# fitting them needs averaging many simulations per evaluation, which is
# impractical on a 4763-utterance corpus. fgt2009() is fit by a coarse alpha
# sweep (fgt2009_sweep_alpha()), since a single run is an MCMC fit and a
# DEoptim search over it is not practical; fgt2009_rsa() is omitted because it
# is identical to fgt2009() on these corpora (the learned lexicon is too
# sparse for the pragmatic layer to bite).
#
# Slow: budget below is ~400 DEoptim evaluations per (model, corpus); on FM
# the per-eval cost ranges from ~0.4 s (decay) to ~5 s (tilles), so the whole
# run is on the order of an hour or two. Run from the package root:
#   Rscript tests/bakeoff_comparison/corpus_gold_fits.R
# Writes: corpus_gold_fits.rds, corpus_gold_fits.png (this dir).

suppressMessages({
  library(XSLmodels)
  library(DEoptim)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

set.seed(1)
out_dir <- "tests/bakeoff_comparison"

corpora <- list(Rollins = rollins_corpus, FM = fm_corpus)

det_models <- c("decay", "uncfam", "uncfam_attention", "uncfam_predictive",
                "fazly", "rescorla_wagner", "tilles", "bayesian_decay",
                "kalman_filter")

# DEoptim: NP scales with the parameter count (the usual >= 10*d guidance),
# with early stopping once the best value stops improving
de_ctrl <- function(n_par) {
  DEoptim::DEoptim.control(NP = max(20L, 10L * n_par), itermax = 25L,
                           reltol = 5e-3, steptol = 8L, trace = FALSE)
}

named_dimnames <- function(m, corpus) {
  if (is.null(dimnames(m))) {
    dimnames(m) <- list(
      sort(unique(unlist(corpus$data$train$words))),
      sort(unique(unlist(corpus$data$train$objects)))
    )
  }
  m
}

score_gold <- function(model, corpus) {
  m <- tryCatch(
    suppressWarnings(xsl_run(model, corpus$data)$fits[[1]]$matrix),
    error = function(e) NULL
  )
  if (is.null(m)) return(NA_real_)
  f <- tryCatch(get_roc_max(named_dimnames(m, corpus), gold_lexicon = corpus$gold),
                error = function(e) NA_real_)
  if (is.finite(f)) f else NA_real_
}

fit_one <- function(model_name, corpus) {
  entry <- XSLmodels:::get_model_registry_entry(model_name)
  base <- entry$constructor()
  n_par <- length(entry$lower)
  par_names <- names(base$params)[seq_len(n_par)]

  obj <- function(par) {
    f <- score_gold(update_params(base, par), corpus)
    if (is.na(f)) 2 else 1 - f          # minimize; 2 is worse than any real F
  }
  res <- DEoptim::DEoptim(obj, lower = entry$lower, upper = entry$upper,
                          de_ctrl(n_par))

  tibble(
    model  = model_name,
    f_default = score_gold(base, corpus),
    f_fitted  = 1 - res$optim$bestval,
    params = paste(par_names, signif(res$optim$bestmem, 4), sep = "=",
                   collapse = ", ")
  )
}

# --- deterministic associative models ------------------------------------

results <- lapply(names(corpora), function(cn) {
  lapply(det_models, function(mn) {
    message(sprintf("[%s] fitting %s ...", cn, mn))
    out <- fit_one(mn, corpora[[cn]])
    out$corpus <- cn
    message(sprintf("    default F = %.3f  ->  fitted F = %.3f  (%s)",
                    out$f_default, out$f_fitted, out$params))
    out
  }) |> bind_rows()
}) |> bind_rows()

# --- fgt2009(): alpha sweep -------------------------------------------------

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
  tibble(model = "fgt2009", corpus = cn,
         f_default = NA_real_, f_fitted = best$f_max,
         params = sprintf("alpha=%g (gamma=0.1, kappa=0.05)", best$alpha))
}) |> bind_rows()

results <- bind_rows(results, fgt_rows) |>
  select(corpus, model, f_default, f_fitted, params) |>
  arrange(corpus, desc(f_fitted))

print(as.data.frame(results), row.names = FALSE)
saveRDS(results, file.path(out_dir, "corpus_gold_fits.rds"))

# --- plot: default vs fitted --------------------------------------------

plot_df <- results |>
  pivot_longer(c(f_default, f_fitted), names_to = "which", values_to = "F") |>
  mutate(which = recode(which, f_default = "registry default", f_fitted = "fit to corpus"),
         model = factor(model, levels = results |>
                          group_by(model) |> summarise(m = mean(f_fitted)) |>
                          arrange(m) |> pull(model)))

p <- ggplot(plot_df, aes(model, F, fill = which)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~corpus) +
  labs(x = NULL, y = "best-threshold F (learned lexicon vs. gold)",
       fill = NULL,
       title = "Deterministic models: registry defaults vs. fit to each corpus") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "corpus_gold_fits.png"), p,
       width = 9, height = 5.5, dpi = 120)

message("wrote corpus_gold_fits.{rds,png}")
