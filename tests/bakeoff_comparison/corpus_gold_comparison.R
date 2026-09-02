# Model comparison on the naturalistic corpora: how well does each model
# recover the gold-standard lexicon of the Rollins and FM corpora?
#
# This is a lightweight comparison, NOT a fitted bake-off:
#   - The associative / sampling models use xsl_model_registry()'s starting
#     parameter values (calibrated defaults, informed by past group fits to
#     xsl_datasets -- see the bounds comments in R/helper.R). They are NOT
#     re-fit here, and certainly not fit to the corpora.
#   - fgt2009() / fgt2009_rsa() use the naturalistic-speech hyperparameters
#     gamma = 0.1, kappa = 0.05 (a corpus is full of non-referential words,
#     unlike a controlled experiment -- see vignette("corpora")), with
#     alpha set by wurwur's two-point heuristic round(7 * sqrt(n / 600)).
#
# Each model is run once per corpus; the learned matrix is scored against the
# corpus gold lexicon with get_roc() (row-normalized, threshold swept), and we
# keep the best-F threshold's precision / recall / F.
#
# Run from the package root:  Rscript tests/bakeoff_comparison/corpus_gold_comparison.R
# Writes: corpus_gold_comparison.rds, corpus_gold_comparison.png (this dir).

suppressMessages({
  library(XSLmodels)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

set.seed(1)
out_dir <- "tests/bakeoff_comparison"
N_SIM <- 50          # simulations averaged for stochastic models

corpora <- list(
  Rollins = rollins_corpus,
  FM      = fm_corpus
)

# --- model set -------------------------------------------------------------

reg <- XSLmodels:::xsl_model_registry()
assoc_names <- setdiff(names(reg), c("fgt2009", "fgt2009_rsa"))

suggest_alpha <- function(n) max(1, round(7 * sqrt(n / 600)))

build_models <- function(corpus) {
  n <- length(corpus$data$train$words)
  a <- suggest_alpha(n)
  c(
    setNames(lapply(assoc_names, function(nm) reg[[nm]]$constructor()), assoc_names),
    list(
      # a lighter sampler than the fgt2009() default keeps the whole
      # comparison to a few minutes; the marginals are noisier but the F
      # ranking is stable
      fgt2009     = fgt2009(alpha = a, gamma = 0.1, kappa = 0.05,
                            n_warmup = 50, n_samples = 120),
      fgt2009_rsa = fgt2009_rsa(alpha = a, gamma = 0.1, kappa = 0.05,
                                n_warmup = 50, n_samples = 120)
    )
  )
}

# --- run + score ----------------------------------------------------------

score_one <- function(mod, corpus) {
  ctrl <- if (isTRUE(mod$stochastic)) xslControl(n_sim = N_SIM) else xslControl()
  tryCatch({
    m <- suppressWarnings(xsl_run(mod, corpus$data, control = ctrl)$fits[[1]]$matrix)
    # some models (e.g. tilles) return an unnamed matrix; voc/ref are the
    # sorted unique labels, exactly as every model builds them internally
    if (is.null(dimnames(m))) {
      dimnames(m) <- list(
        sort(unique(unlist(corpus$data$train$words))),
        sort(unique(unlist(corpus$data$train$objects)))
      )
    }
    roc <- get_roc(m, gold_lexicon = corpus$gold)
    roc <- roc[is.finite(roc$fscore), ]
    if (nrow(roc) == 0) stop("no finite F-score at any threshold")
    best <- roc[which.max(roc$fscore), ]
    tibble(threshold = best$threshold, precision = best$precision,
           recall = best$recall, f_max = best$fscore)
  }, error = function(e) {
    message("   -> failed: ", conditionMessage(e))
    NULL
  })
}

results <- lapply(names(corpora), function(cn) {
  corpus <- corpora[[cn]]
  mods <- build_models(corpus)
  lapply(names(mods), function(mn) {
    message(sprintf("[%s] %s ...", cn, mn))
    s <- score_one(mods[[mn]], corpus)
    if (is.null(s)) return(NULL)
    s$model <- mn
    s$corpus <- cn
    s$stochastic <- isTRUE(mods[[mn]]$stochastic)
    s
  }) |> bind_rows()
}) |> bind_rows() |>
  select(corpus, model, f_max, precision, recall, threshold, stochastic) |>
  arrange(corpus, desc(f_max))

all_models <- c(assoc_names, "fgt2009", "fgt2009_rsa")
failed <- setdiff(all_models, unique(results$model))
if (length(failed)) {
  message("\nproduced no scorable lexicon on either corpus: ",
          paste(failed, collapse = ", "))
}

print(results, n = Inf)
saveRDS(results, file.path(out_dir, "corpus_gold_comparison.rds"))

# a wide F-score table (models x corpora), rounded, for quick reading
wide <- results |>
  select(model, corpus, f_max) |>
  pivot_wider(names_from = corpus, values_from = f_max) |>
  arrange(desc(rowMeans(across(-model), na.rm = TRUE)))
cat("\nbest-threshold F (learned lexicon vs. gold):\n")
print(as.data.frame(wide), digits = 3, row.names = FALSE)

# --- plot ---------------------------------------------------------------

ord <- results |>
  group_by(model) |> summarise(m = mean(f_max)) |> arrange(m) |> pull(model)

p <- results |>
  mutate(model = factor(model, levels = ord),
         corpus = factor(corpus, levels = c("Rollins", "FM")),
         family = case_when(
           grepl("^fgt", model) ~ "Bayesian (fgt2009)",
           stochastic ~ "sampling / RL",
           TRUE ~ "associative"
         )) |>
  ggplot(aes(model, f_max, fill = family)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~corpus) +
  labs(x = NULL, y = "best-threshold F (learned lexicon vs. gold)",
       title = "Gold-lexicon recovery on the naturalistic corpora",
       subtitle = paste("registry default params (assoc.) / gamma = 0.1,",
                        "kappa = 0.05, heuristic alpha (fgt2009)")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "corpus_gold_comparison.png"), p,
       width = 9, height = 5, dpi = 120)

message("wrote corpus_gold_comparison.{rds,png}")
