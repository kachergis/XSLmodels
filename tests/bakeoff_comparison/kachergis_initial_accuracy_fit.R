# Model comparison on the Kachergis, Grimmick, & Gureckis "initial accuracy"
# dataset (data-raw/add_kachergis_initial_accuracy.R): does a model with
# error-driven (prediction error) learning better capture the crossover
# interaction between condition (High/Low Initial Accuracy) and item type
# (initially accurate/inaccurate) than the original biased associative model
# (BAM) or a system-level attention account?
#
# Compares 5 models, jointly fit across both conditions (High/Low Initial
# Accuracy), analogous to how the rest of the package fits one shared
# parameter set across a list of xslData conditions:
#   - baseline():            floor (raw co-occurrence counts, no learning)
#   - uncfam():               original Biased Associative Model (BAM;
#                              Kachergis et al. 2012)
#   - uncfam_attention():     BAM + system-level attention (Fitneva &
#                              Christiansen 2015's account: higher learning
#                              rate on higher-uncertainty trials)
#   - uncfam_predictive():    BAM + item-level prediction error (this
#                              paper's proposed account)
#   - rescorla_wagner():      pure prediction-error learning, no
#                              uncertainty/familiarity bias
#
# Note this differs from the original paper's fitting procedure, which fit
# each *subject* individually and then aggregated -- here we do a standard
# group-level fit (a single shared parameter set per model, across both
# conditions at once), which is both the package's idiomatic workflow and a
# stronger test of whether one parameterization explains both conditions.

pkgload::load_all("..", quiet = TRUE)
library(purrr)
library(dplyr)
library(ggplot2)

hia <- kachergis_initial_accuracy[["High Initial Accuracy"]]
lia <- kachergis_initial_accuracy[["Low Initial Accuracy"]]
conds <- list(`High IA` = hia, `Low IA` = lia)

# item-level classification (word index 1-18), derived from the dataset
# construction (data-raw/add_kachergis_initial_accuracy.R): initially
# accurate items are exactly the ones on the training matrix's diagonal
initially_accurate <- list(
  `High IA` = c(1, 2, 3, 5, 6, 9, 10, 13, 14, 16, 17, 18),
  `Low IA` = c(5, 6, 9, 10, 13, 14)
)
item_type <- function(cond_name, word_idx) {
  ifelse(word_idx %in% initially_accurate[[cond_name]], "accurate", "inaccurate")
}

deoptim_control <- DEoptim::DEoptim.control(reltol = .001, NP = 60, itermax = 60, trace = FALSE)

model_specs <- list(
  baseline = list(model = baseline(), lower = numeric(0), upper = numeric(0)),
  uncfam = list(model = uncfam(X = .1, B = .98, C = 1),
               lower = c(1e-4, .1, .5), upper = c(1, 20, 1)),
  uncfam_attention = list(model = uncfam_attention(X = .1, B = .98, C = 1),
                          lower = c(1e-4, .1, .5), upper = c(1, 20, 1)),
  uncfam_predictive = list(model = uncfam_predictive(X = .1, B = .98, C = 1),
                           lower = c(1e-5, .1, .5), upper = c(1, 10, 1)),
  rescorla_wagner = list(model = rescorla_wagner(C = .98, alpha = .1, beta = 1, lambda = 1),
                        lower = c(.8, .01, .5, .5), upper = c(1.0, .5, 2.0, 2.0))
)

## ---- 1. Joint group-level fit across both conditions ----------------------
cat("=== Fitting each model jointly across High IA + Low IA ===\n")
fits <- imap(model_specs, function(spec, nm) {
  cat("  fitting", nm, "...\n")
  fit <- xsl_fit(spec$model, list(hia, lia), lower = spec$lower, upper = spec$upper,
                deoptim_control = deoptim_control)[[1]]
  fitted_model <- if (length(spec$lower) == 0) spec$model else
    update_params(spec$model, fit$optim$bestmem)
  run <- xsl_run(fitted_model, list(hia, lia))
  list(name = nm, params = fit$optim$bestmem, sse = run$sse,
      pred_hia = run$fits[[1]]$perf, pred_lia = run$fits[[2]]$perf)
})

## ---- 2. Per-item comparison table (human vs. model, by condition x item type) ----
per_item <- map(fits, function(f) {
  bind_rows(
    tibble::tibble(model = f$name, condition = "High IA", word = seq_along(f$pred_hia),
                  human = hia$accuracy, predicted = unname(f$pred_hia)),
    tibble::tibble(model = f$name, condition = "Low IA", word = seq_along(f$pred_lia),
                  human = lia$accuracy, predicted = unname(f$pred_lia))
  )
}) |> list_rbind() |>
  mutate(item_type = mapply(item_type, condition, word))

summary_table <- per_item |>
  group_by(model, condition, item_type) |>
  summarise(human = mean(human), predicted = mean(predicted), .groups = "drop")

overall_sse <- per_item |>
  group_by(model) |>
  summarise(mse = mean((human - predicted) ^ 2),
           r = cor(human, predicted), .groups = "drop") |>
  arrange(mse)

cat("\n=== Mean accuracy by model x condition x item type (human vs. predicted) ===\n")
print(summary_table |> mutate(across(c(human, predicted), \(x) round(x, 3))), n = 100)

cat("\n=== Overall fit quality (lower MSE / higher r = better) ===\n")
print(overall_sse |> mutate(across(c(mse, r), \(x) round(x, 4))))

## ---- 3. Leave-one-condition-out cross-validation ---------------------------
## Does a parameter set fit on one condition transfer to the other?
cat("\n=== 2-fold (leave-one-condition-out) cross-validation ===\n")
cv <- imap(model_specs, function(spec, nm) {
  cat("  cross-validating", nm, "...\n")
  if (length(spec$lower) == 0) {
    # baseline has no free parameters -- cross-validation is degenerate
    r_hia <- xsl_run(spec$model, hia)$sse
    r_lia <- xsl_run(spec$model, lia)$sse
    return(tibble::tibble(model = nm, train_sse = mean(c(r_hia, r_lia)),
                          test_sse = mean(c(r_hia, r_lia))))
  }
  result <- cross_validated_group_fits(spec$model, list(hia, lia), lower = spec$lower,
                                       upper = spec$upper, n_folds = 2, seed = 1,
                                       deoptim_control = deoptim_control)
  tibble::tibble(model = nm, train_sse = mean(result$train_sse), test_sse = mean(result$test_sse))
}) |> list_rbind()
cat("\n")
print(cv |> mutate(across(c(train_sse, test_sse), \(x) round(x, 4))) |> arrange(test_sse))

## ---- 4. Figure: human vs. model accuracy, by condition x item type --------
plot_df <- summary_table |>
  mutate(model = factor(model, levels = c("baseline", "uncfam", "uncfam_attention",
                                         "rescorla_wagner", "uncfam_predictive"),
                        labels = c("Baseline", "Biased Associative Model", "BAM + Attention",
                                  "Rescorla-Wagner", "BAM + Prediction")))

human_df <- plot_df |> filter(model == "Biased Associative Model") |>
  select(condition, item_type, human)

p <- ggplot(plot_df, aes(x = condition, y = human, fill = item_type)) +
  geom_col(position = position_dodge(width = .9), width = .75, alpha = .5) +
  geom_point(aes(y = predicted, color = item_type), position = position_dodge(width = .9),
            size = 2.5, shape = 18) +
  facet_wrap(vars(model)) +
  geom_hline(yintercept = 1 / 18, linetype = "dashed", color = "grey40") +
  labs(x = "Condition", y = "Proportion Correct", fill = "Initially Accurate\n(human, bars)",
      color = "Initially Accurate\n(model, diamonds)",
      title = "Human vs. model accuracy: Initial Accuracy experiment") +
  theme_bw()
print(p)
ggplot2::ggsave("model_comparison.png", plot = p, width = 8, height = 5.5)
cat("\nSaved figure to tests/bakeoff_comparison/model_comparison.png\n")
