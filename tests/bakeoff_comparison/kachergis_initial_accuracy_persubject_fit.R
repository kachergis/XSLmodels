# Verify the paper's own reported result: fit each SUBJECT individually
# (as in initial_accuracyXSL/analysis/model/fit_mturk_bySubj_predict.R's
# fitBySubject()), then aggregate -- rather than the single shared,
# jointly-fit parameter set used in kachergis_initial_accuracy_fit.R. This
# is a much higher-capacity fitting regime (one X/B/C triple per subject,
# 65 subjects, vs. one triple total) and is what the paper's own reported
# numbers (e.g. BAM mean SSE .026, Predictive BAM mean SSE .0098) are based
# on -- reusing the group-level fit for that comparison wasn't apples to
# apples.
#
# Per subject, the paper's own script fits against 2 targets (not 18):
# mean accuracy on that subject's initially-accurate items and mean
# accuracy on their initially-inaccurate items (individual item-level
# binary responses are too noisy per subject to fit 18 separate targets
# against). Each subject's REAL study trial order is used (not the single
# representative order used to build the shared `kachergis_initial_accuracy`
# package dataset).
#
# Requires ../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata
# (a sibling checkout of https://github.com/kachergis/initial_accuracyXSL).

pkgload::load_all("..", quiet = TRUE)
suppressMessages(library(dplyr))
library(purrr)
library(ggplot2)

raw_data_path <- "../../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata"
stopifnot(file.exists(raw_data_path))
load(raw_data_path) # study, test, qdat1, qdat2

memaid <- subset(qdat2, memory_aid == "yes") %>% rename(uniqueId = uniqueid)
test <- subset(test, !is.element(uniqueId, memaid$uniqueId))
study <- subset(study, uniqueId %in% unique(test$uniqueId))

relabel_obj <- function(o) bitwXor(as.integer(o), 1L)

familiarization_trials <- list(words = as.list(1:18), objects = as.list(1:18))

# item-level classification (word index 1-18), same as
# kachergis_initial_accuracy_fit.R / data-raw/add_kachergis_initial_accuracy.R
initially_accurate <- list(
  `High Initial Accuracy` = c(1, 2, 3, 5, 6, 9, 10, 13, 14, 16, 17, 18),
  `Low Initial Accuracy` = c(5, 6, 9, 10, 13, 14)
)

## ---- build one subject's real trial-order xslData (train only) + 2-value target ----
build_subject <- function(uid) {
  s <- subset(study, uniqueId == uid)
  s <- s[order(s$trial), ]
  cond <- s$condition[1]
  study_trials <- list(
    words = lapply(seq_len(nrow(s)), \(i) c(s$w1ind[i], s$w2ind[i]) + 1L),
    objects = lapply(seq_len(nrow(s)), \(i) relabel_obj(c(s$o1ind[i], s$o2ind[i])) + 1L)
  )
  train <- list(
    words = c(familiarization_trials$words, study_trials$words),
    objects = c(familiarization_trials$objects, study_trials$objects)
  )
  dat <- xslData(train = train, label = uid, condition = cond)

  t2 <- subset(test, uniqueId == uid & !is.na(init_acc))
  human <- c(
    accurate = mean(t2$correct[t2$init_acc == "True"]),
    inaccurate = mean(t2$correct[t2$init_acc == "False"])
  )
  list(uid = uid, condition = cond, data = dat, human = human,
      accurate_idx = initially_accurate[[cond]],
      inaccurate_idx = setdiff(1:18, initially_accurate[[cond]]))
}

subjects <- map(unique(study$uniqueId), build_subject)
cat("n subjects:", length(subjects), " (",
   sum(map_chr(subjects, "condition") == "High Initial Accuracy"), "High IA,",
   sum(map_chr(subjects, "condition") == "Low Initial Accuracy"), "Low IA)\n")

## ---- per-subject fitting: collapse the model's 18-item perf to the same ----
## ---- 2 targets (mean over accurate items, mean over inaccurate items) ------
collapse_pred <- function(perf, subj) {
  c(accurate = mean(perf[subj$accurate_idx]), inaccurate = mean(perf[subj$inaccurate_idx]))
}

fit_subject <- function(model, subj, lower, upper, deoptim_control) {
  objective <- function(par) {
    sse <- tryCatch({
      m <- update_params(model, par)
      perf <- xsl_run(m, subj$data)$fits[[1]]$perf
      pred <- collapse_pred(perf, subj)
      sum((pred - subj$human) ^ 2)
    }, error = function(e) NA_real_)
    if (is.na(sse)) Inf else sse
  }
  if (length(lower) == 0) {
    perf <- xsl_run(model, subj$data)$fits[[1]]$perf
    pred <- collapse_pred(perf, subj)
    return(list(par = numeric(0), sse = sum((pred - subj$human) ^ 2), pred = pred))
  }
  fit <- DEoptim::DEoptim(objective, lower = lower, upper = upper, deoptim_control)
  best <- fit$optim$bestmem
  m <- update_params(model, best)
  perf <- xsl_run(m, subj$data)$fits[[1]]$perf
  pred <- collapse_pred(perf, subj)
  list(par = best, sse = fit$optim$bestval, pred = pred)
}

deoptim_control <- DEoptim::DEoptim.control(reltol = .001, NP = 30, itermax = 40, trace = FALSE)

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

cat("=== Fitting each of", length(subjects), "subjects individually, per model ===\n")
results <- imap(model_specs, function(spec, nm) {
  cat("  ", nm, "... ")
  t0 <- Sys.time()
  fits <- map(subjects, \(subj) fit_subject(spec$model, subj, spec$lower, spec$upper, deoptim_control))
  cat(round(as.numeric(Sys.time() - t0, units = "secs")), "s\n")
  tibble::tibble(
    model = nm,
    uid = map_chr(subjects, "uid"),
    condition = map_chr(subjects, "condition"),
    human_accurate = map_dbl(subjects, \(s) s$human["accurate"]),
    human_inaccurate = map_dbl(subjects, \(s) s$human["inaccurate"]),
    pred_accurate = map_dbl(fits, \(f) f$pred["accurate"]),
    pred_inaccurate = map_dbl(fits, \(f) f$pred["inaccurate"]),
    sse = map_dbl(fits, "sse")
  )
}) |> list_rbind()

## ---- aggregate: does this reproduce the paper's per-subject numbers? ------
agg <- results |>
  group_by(model) |>
  summarise(
    median_sse = median(sse), mean_sse = mean(sse), sd_sse = sd(sse),
    r = cor(c(human_accurate, human_inaccurate), c(pred_accurate, pred_inaccurate)),
    .groups = "drop"
  ) |>
  arrange(mean_sse)

cat("\n=== Per-subject fit quality (aggregated across", length(subjects), "subjects) ===\n")
print(agg |> mutate(across(where(is.numeric), \(x) round(x, 4))))

cat("\nPaper's own reported per-subject numbers (noReg, fit_mturk_bySubj_predict.R comments):\n")
cat("  BAM (orig):        median SSE 0.0026, mean 0.0262, r = .94\n")
cat("  BAM + Attention:                                    r = .97\n")
cat("  BAM + Prediction:  median SSE 0.0070, mean 0.0098, r = .984\n")
cat("  Rescorla-Wagner:   median SSE 0.0037, mean 0.0530, r = .885\n")
cat("  (paper's RW used a bespoke 2-parameter model, not this package's rescorla_wagner())\n")

## ---- condition x item-type means, mirroring the paper's Figure 2 ----------
long <- bind_rows(
  results |> transmute(model, condition, item_type = "accurate",
                       human = human_accurate, predicted = pred_accurate),
  results |> transmute(model, condition, item_type = "inaccurate",
                       human = human_inaccurate, predicted = pred_inaccurate)
)
cond_summary <- long |>
  group_by(model, condition, item_type) |>
  summarise(human = mean(human), predicted = mean(predicted), .groups = "drop")

cat("\n=== Mean accuracy by model x condition x item type (human vs. predicted) ===\n")
print(cond_summary |> mutate(across(c(human, predicted), \(x) round(x, 3))), n = 100)

plot_df <- cond_summary |>
  mutate(condition = recode(condition, "High Initial Accuracy" = "High IA",
                            "Low Initial Accuracy" = "Low IA"),
        model = factor(model, levels = c("baseline", "uncfam", "uncfam_attention",
                                        "rescorla_wagner", "uncfam_predictive"),
                       labels = c("Baseline", "Biased Associative Model", "BAM + Attention",
                                 "Rescorla-Wagner", "BAM + Prediction")))

p <- ggplot(plot_df, aes(x = condition, y = human, fill = item_type)) +
  geom_col(position = position_dodge(width = .9), width = .75, alpha = .5) +
  geom_point(aes(y = predicted, color = item_type), position = position_dodge(width = .9),
            size = 2.5, shape = 18) +
  facet_wrap(vars(model)) +
  geom_hline(yintercept = 1 / 18, linetype = "dashed", color = "grey40") +
  labs(x = "Condition", y = "Proportion Correct", fill = "Initially Accurate\n(human, bars)",
      color = "Initially Accurate\n(model, diamonds)",
      title = "Per-subject hierarchical fit: Human vs. model accuracy") +
  theme_bw()
print(p)
ggplot2::ggsave("model_comparison_persubject.png", plot = p, width = 8, height = 5.5)
cat("\nSaved figure to tests/bakeoff_comparison/model_comparison_persubject.png\n")

saveRDS(results, "persubject_fit_results.rds")
cat("Saved per-subject fits to tests/bakeoff_comparison/persubject_fit_results.rds\n")
