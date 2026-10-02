# Backfill train/test/accuracy/nsubj in XSL-dataset-fields.csv directly from
# xsl_datasets, for every condition the package actually has data for. These
# four fields are all mechanically derivable from an xslData object, so
# there's no reason to hand-maintain them once a condition is in the package.
#
# sd_accuracy is deliberately NOT touched here: xsl_datasets/combined_data.rda
# only store the already-aggregated per-item accuracy (HumanItemAcc), not
# subject-level scores, so sd(participant accuracy) isn't recoverable from
# them. Where sd_accuracy is already filled in (e.g. Suanda2014, Koehne2013),
# it was transcribed from the source paper, not computed here. Recovering it
# for the package's own conditions would mean re-deriving per-subject
# accuracy from the raw files in data-raw/agg_data, data-raw/data, etc. --
# see export_data.R's summarize_condition(), which sketches but never
# finishes exactly this (subj_perf_sd <- sd(sub_acc$acc), commented out at
# every call site).
#
# Run from the package root: Rscript data-raw/update_dataset_fields_from_xsl_datasets.R

devtools::load_all(quiet = TRUE)
library(readr)

csv_path <- "data-raw/XSL-dataset-fields.csv"
# read everything as character so round-tripping doesn't rewrite unrelated
# columns (readr/read.csv both guess "T"/"F" as logical and base R's
# write.csv quotes every character field, which makes an all-numeric-aware
# round trip rewrite the whole file)
csv <- read_csv(csv_path, col_types = cols(.default = "c"), progress = FALSE)

# a handful of labels were renamed between when this CSV was written (to
# catalog candidates for add_external_datasets.R) and the final xsl_datasets
# slate; see datapage/flatten_xsl_datasets.R for the same map applied the
# other direction.
label_aliases <- c(
  "Koehne2013-aaappp" = "KoehneTrueswellGleitman2013-aaappp",
  "Koehne2013-apapap" = "KoehneTrueswellGleitman2013-apapap",
  "Koehne2013-papapa" = "KoehneTrueswellGleitman2013-papapa",
  "Koehne2013-pppaaa" = "KoehneTrueswellGleitman2013-pppaaa"
  # "orig_4x4" -> "orig_order4x4" and "301" -> "301_training_R" were tried
  # and reverted: xsl_datasets has BOTH a numeric-ID "4x4" design (223, n=77)
  # and an "orig_4x4" one (n=88), with different trial sequences and
  # accuracy patterns -- not duplicates. The CSV's "orig_order4x4" row's
  # nsubj (77) matches 223, not orig_4x4, so aliasing it to orig_4x4 was
  # likely writing data into the wrong row. Needs a human call on which
  # condition the CSV's citation/description actually describes (and
  # whether the other one needs its own new row) -- see summary in NEWS.md
  # or ask the maintainer. Same open question for "301" / "301_training_R"
  # and the parallel "3x3"/"2x2" pairs (224 vs orig_3x3, 225 vs orig_2x2).
)

n_updated <- 0
discrepancies <- list()

for (d in xsl_datasets) {
  csv_key <- if (d$label %in% csv$order_filename) {
    d$label
  } else if (d$label %in% names(label_aliases)) {
    label_aliases[[d$label]]
  } else {
    NA_character_
  }
  if (is.na(csv_key)) next  # no CSV row for this condition at all

  row <- which(csv$order_filename == csv_key)
  if (length(row) != 1) next  # ambiguous (e.g. Medina2013orders has 4 rows, not this kind of match) -- skip

  n_test <- if (is.null(d$test)) 0L else length(d$test$words)
  mean_acc <- if (is.null(d$accuracy) || all(is.na(d$accuracy))) NA_real_ else mean(d$accuracy, na.rm = TRUE)

  old_nsubj <- suppressWarnings(as.numeric(csv$nsubj[row]))
  if (!is.na(old_nsubj) && !is.na(d$n_subj) && old_nsubj != d$n_subj) {
    discrepancies[[d$label]] <- sprintf("nsubj: CSV had %s, xsl_datasets has %s", old_nsubj, d$n_subj)
  }

  csv$train[row] <- as.character(length(d$train$words))
  csv$test[row] <- as.character(n_test)
  csv$accuracy[row] <- if (is.na(mean_acc)) NA_character_ else as.character(mean_acc)
  csv$nsubj[row] <- as.character(d$n_subj)
  n_updated <- n_updated + 1
}

write_csv(csv, csv_path, na = "", eol = "\r\n")  # the file's original line ending

cat("Updated", n_updated, "of", length(xsl_datasets), "xsl_datasets conditions in", csv_path, "\n")
if (length(discrepancies) > 0) {
  cat("\nnsubj discrepancies (CSV value overwritten with xsl_datasets value):\n")
  for (lab in names(discrepancies)) cat(" ", lab, "-", discrepancies[[lab]], "\n")
}
all_labels <- sapply(xsl_datasets, function(d) d$label)
csv_keys <- ifelse(all_labels %in% names(label_aliases), label_aliases[all_labels], all_labels)
no_csv_row <- all_labels[!csv_keys %in% csv$order_filename]
cat("\nxsl_datasets conditions with no matching CSV row (not updated,", length(no_csv_row), "):\n")
print(no_csv_row)
