# Rebuild the frequency / contextual-diversity conditions from their source
# data, drop two duplicate conditions, and add test-response (confusion)
# matrices to every xsl_datasets condition with raw data.
#
# 1. Frequency / CD conditions (Kachergis, Yu & Shiffrin): rebuilt from
#    data-raw/data/freqCD_item_data.Rdata (the original analysis data; copied
#    from the paper's analysis files), pooled by training order file. Their
#    stored n_subj values had been attached to the wrong conditions (the
#    accuracies were right), and three conditions (3_x8_369_4x4,
#    freq369-3x3loCD, orig_4x4) had been pooled with the first test block of
#    the "priming" experiments, which trained on the same sequences but tested
#    four times. Those values could not be reproduced, and the priming data
#    belong in a separate 4-block (trajectory) dataset, so the conditions are
#    now unpooled. The training sequences are identical to the source's order
#    files.
# 2. Dropped:
#    - orig_3x3 duplicated condition 224: identical training sequence and
#      identical item accuracies (both from the same 36 participants), but
#      n_subj 104. The other ~68 participants were reportedly contributed by
#      another lab; their item-level data are not in hand (TODO: track down,
#      then pool them into 224).
#    - orig_4x4's participants (freqCD exp9, n = 26, once pooled with
#      priming) are a subset of condition 223's (n = 77, ~ freqCD exp7 + exp9),
#      and the two training sequences are the same except that words 4 and 16
#      trade places between trials 11 and 24 (TODO: check which is right; the
#      freqCD order file matches orig_4x4's). 223 is kept.
# 3. response_matrix: for each test item (rows, in `accuracy` order) the number
#    of participants choosing each object (columns, by object label). Added
#    for every condition whose raw responses reproduce its accuracy exactly;
#    the Suanda (2014) and Koehne et al. (2013) conditions have no raw data.
#
# Usage (from the package root): Rscript data-raw/fix_freqcd_add_responses.R

suppressMessages({devtools::load_all(".", quiet = TRUE); library(dplyr)})
source("data-raw/raw_responses.R")
raw <- load_raw_responses()
labels <- vapply(xsl_datasets, `[[`, character(1), "label")

# ---- 1-2. rebuild freqCD conditions; drop duplicates ----
freqcd_file <- c("2_x8_39_4x4" = "2_x8_39_4x4", "3_x8_369_4x4" = "3_x8_369_4x4",
                 "freq369-3x3loCD" = "block1_369-3x3loCD", "freq369-3x3hiCD" = "block2_369-3x3hiCD",
                 "freq369_36mx" = "block3_369_36mx", "freq369_39mx" = "block4_369_39mx",
                 "cont_div6-12" = "cont_div6-12")
changes <- list()
for (l in names(freqcd_file)) {
  i <- which(labels == l)
  g <- raw |> filter(source == "freqCD", raw_cond == freqcd_file[[l]])
  acc <- g |> group_by(word) |> summarise(acc = mean(correct), .groups = "drop") |> arrange(word)
  stopifnot(identical(acc$word, 1:18))
  d <- xsl_datasets[[i]]
  changes[[l]] <- tibble(label = l, n_subj_old = d$n_subj, n_subj_new = n_distinct(g$subject),
                         max_acc_change = max(abs(d$accuracy - acc$acc)))
  d$accuracy <- acc$acc
  d$n_subj <- n_distinct(g$subject)
  xsl_datasets[[i]] <- d
}
cat("Rebuilt from freqCD source:\n"); print(bind_rows(changes))

keep <- !labels %in% c("orig_3x3", "orig_4x4")
xsl_datasets <- xsl_datasets[keep]
labels <- labels[keep]

# ---- 3. response matrices ----
groups_for <- function(l) {
  if (l %in% names(freqcd_file)) return(paste0("freqCD | ", freqcd_file[[l]], " | b1"))
  if (grepl("^filt", l)) return(paste0("filtering | ", l, c(" AtoA", " AtoB"), " | b1"))
  if (grepl("^[0-9]{3}$", l)) return(paste0("asym | ", l, " | b1"))
  switch(l,
         "1_max_temp_spat_cont_orig" = "x8 | 1_ | b1", "4_no_spat_orig_max_tc" = "x8 | 4_ | b1",
         "temp_cont_1olap2tr_contr" = "x13 | 1_ | b1", "1olap3tr_contr" = "x13 | 2_ | b1",
         "temp_cont_2olap2tr_contr" = "x13 | 3_ | b1",
         NULL)
}
n_added <- 0
for (i in seq_along(xsl_datasets)) {
  d <- xsl_datasets[[i]]
  gs <- groups_for(d$label)
  if (is.null(gs)) next
  g <- raw |> filter(group %in% gs)
  stopifnot(nrow(g) > 0)
  objs <- sort(unique(unlist(lapply(d$train$objects, \(x) unlist(x, use.names = FALSE)))))
  # test items: (word, correct object) in accuracy order
  items <- if (length(d$test) > 0) {
    tibble(word = as.integer(unlist(d$test$words)),
           correct_obj = as.integer(if (is.null(d$test$correct)) unlist(d$test$words) else unlist(d$test$correct)))
  } else tibble(word = seq_along(d$accuracy), correct_obj = seq_along(d$accuracy))
  m <- matrix(0L, nrow(items), length(objs), dimnames = list(item = seq_len(nrow(items)), object = objs))
  for (k in seq_len(nrow(items))) {
    r <- g$response[g$word == items$word[k] & g$correct_obj == items$correct_obj[k]]
    stopifnot(all(r %in% objs))
    tab <- table(factor(r, levels = objs))
    m[k, ] <- as.integer(tab)
  }
  # every participant answers every test item once, and the matrix reproduces
  # the stored accuracy exactly
  acc_m <- m[cbind(seq_len(nrow(items)), match(items$correct_obj, objs))] / rowSums(m)
  if (!all(rowSums(m) == d$n_subj) || !isTRUE(all.equal(unname(acc_m), unname(d$accuracy)))) {
    stop(sprintf("%s: row sums %s (n_subj %d); max |acc diff| %.3g", d$label,
                 paste(unique(rowSums(m)), collapse = ","), d$n_subj, max(abs(acc_m - d$accuracy))))
  }
  d$response_matrix <- m
  xsl_datasets[[i]] <- validate_xslData(new_xslData(unclass(d)))
  n_added <- n_added + 1
}
cat(sprintf("\n%d conditions (was 56); response matrices added for %d; none for: %s\n", length(xsl_datasets), n_added,
            paste(labels[vapply(xsl_datasets, \(d) is.null(d$response_matrix), logical(1))], collapse = ", ")))

usethis::use_data(xsl_datasets, overwrite = TRUE)
