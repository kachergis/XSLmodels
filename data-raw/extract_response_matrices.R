# Extract test-response (confusion) matrices from the raw Kachergis-lab data,
# and check them against xsl_datasets.
#
# For every raw data group (source x condition [x block / test type]), counts
# how often each tested word drew each object, giving a word x object matrix
# whose diagonal / row sums is item accuracy. Each xsl_datasets condition is
# then matched to the raw group(s) whose item accuracies reproduce its stored
# `accuracy`: exactly, approximately, or not at all. Conditions are matched
# empirically rather than by name, so mislabelled or pooled conditions show
# up. Nothing in xsl_datasets is changed here; the matrices and the match
# report are saved to data-raw/data/response_matrices.rds for review.
#
# Raw sources: data/asym_test_data.RData (the 2xx/301 conditions),
# agg_data/x4-x13 (Kachergis-lab experiments), data/priming_all_trajectory.RData
# (4 train/test blocks per participant), data/filtering_data.RData (AtoA and
# AtoB tests; see fix_filtering_tests.R), and data/freqCD_item_data.Rdata (the
# frequency / contextual-diversity experiments, from the original analysis).
# The non-Kachergis datasets (Suanda,
# Koehne, ...) have only trial orders, no responses.
#
# Usage (from the package root): Rscript data-raw/extract_response_matrices.R

suppressMessages({devtools::load_all(".", quiet = TRUE); library(dplyr); library(purrr)})

# ---- 1. raw responses in one long format (data-raw/raw_responses.R) ----
source("data-raw/raw_responses.R")
raw <- load_raw_responses()

# ---- 2. per-group checks, response matrices, and item accuracy ----
consistency <- raw |> group_by(group) |>
  summarise(n_rows = n(), n_subj = n_distinct(subject),
            correct_mismatch = sum(correct != (response == correct_obj), na.rm = TRUE),
            na_response = sum(is.na(response)),
            dup_word_tests = sum(table(paste(subject, word)) > 1), .groups = "drop")
raw_groups <- split(raw, raw$group)
n_obj <- function(g) max(c(g$word, g$correct_obj, g$response), na.rm = TRUE)
rmat <- map(raw_groups, function(g) {
  k <- n_obj(g)
  m <- matrix(0L, k, k, dimnames = list(word = 1:k, response = 1:k))
  ok <- !is.na(g$response)
  for (r in which(ok)) m[g$word[r], g$response[r]] <- m[g$word[r], g$response[r]] + 1L
  m
})
item_acc <- map(raw_groups, \(g) g |> group_by(word) |> summarise(acc = mean(correct), .groups = "drop") |> arrange(word))

# ---- 3. match each xsl_datasets condition to raw group(s) ----
cand <- names(item_acc)
match_one <- function(d) {
  acc <- d$accuracy
  n <- length(acc)
  # filtering conditions: 24 items = AtoA (words 1-12) then AtoB (words 1-12)
  if (grepl("^filt", d$label)) {
    ga <- paste0("filtering | ", d$label, " AtoA | b1"); gb <- paste0("filtering | ", d$label, " AtoB | b1")
    a <- c(item_acc[[ga]]$acc, item_acc[[gb]]$acc)
    return(tibble(groups = paste(ga, gb, sep = " + "), n_raw_subj = n_distinct(raw_groups[[ga]]$subject),
                  max_diff = max(abs(a - acc)), r = cor(a, acc)))
  }
  same_n <- cand[map_int(item_acc[cand], nrow) == n & map_lgl(item_acc[cand], \(x) all(x$word == seq_len(n)))]
  if (!length(same_n)) return(tibble(groups = NA_character_, n_raw_subj = NA_integer_, max_diff = NA_real_, r = NA_real_))
  diffs <- map_dbl(same_n, \(g) max(abs(item_acc[[g]]$acc - acc)))
  best <- same_n[which.min(diffs)]
  out <- tibble(groups = best, n_raw_subj = n_distinct(raw_groups[[best]]$subject), max_diff = min(diffs),
                r = cor(item_acc[[best]]$acc, acc))
  if (out$max_diff > 1e-6) {
    # try pooling two groups (e.g. an original experiment + priming block 1)
    top <- same_n[order(diffs)][seq_len(min(8, length(same_n)))]
    pairs <- combn(top, 2, simplify = FALSE)
    pooled <- map(pairs, function(p) {
      g <- bind_rows(raw_groups[p]); a <- g |> group_by(word) |> summarise(acc = mean(correct), .groups = "drop") |> arrange(word)
      tibble(groups = paste(p, collapse = " + "), n_raw_subj = n_distinct(g$subject), max_diff = max(abs(a$acc - acc)), r = cor(a$acc, acc))
    }) |> bind_rows()
    if (min(pooled$max_diff) < out$max_diff) out <- pooled[which.min(pooled$max_diff), ]
  }
  out
}
report <- map(xsl_datasets, function(d) bind_cols(tibble(label = d$label, condition = d$condition, n_items = length(d$accuracy),
                                                         n_subj = d$n_subj), match_one(d))) |>
  bind_rows() |>
  mutate(status = case_when(is.na(max_diff) ~ "no raw data", max_diff < 1e-6 ~ "exact",
                            max_diff < .05 ~ "close", TRUE ~ "mismatch"))

cat("== Match status ==\n"); print(table(report$status))
cat("\n== Per condition ==\n")
print(report |> mutate(max_diff = signif(max_diff, 2), r = round(r, 3)) |> select(label, n_items, n_subj, n_raw_subj, status, max_diff, r, groups),
      n = Inf, width = 250)
cat("\n== Raw groups with internal inconsistencies (scored correctness disagreeing with response == correct object, missing responses, or a word tested twice by one participant) ==\n")
print(consistency |> filter(correct_mismatch > 0 | na_response > 0 | dup_word_tests > 0), n = Inf, width = 200)
used <- unique(unlist(strsplit(na.omit(report$groups), " \\+ ")))
cat("\n== Raw groups not matched to any xsl_datasets condition ==\n")
print(consistency |> filter(!group %in% used) |> select(group, n_rows, n_subj), n = Inf, width = 200)

saveRDS(list(response_matrices = rmat, item_accuracy = item_acc, consistency = consistency, report = report),
        "data-raw/data/response_matrices.rds")
