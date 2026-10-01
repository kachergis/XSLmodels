# Flatten xsl_datasets (53 experimental conditions) into tidy, relational
# tables suitable for uploading to Redivis as the backing data for a
# datapages/datapage (https://github.com/datapages/datapage) site.
#
# Produces three CSVs in datapage/output/:
#   - conditions.csv: one row per condition, with design/sample-size fields
#     from the package itself plus (where available) citation/description
#     metadata from data-raw/XSL-dataset-fields.csv
#   - trials.csv: one row per (condition, trial), words/objects presented
#   - accuracy.csv: one row per (condition, word index), human accuracy
#
# Run from the package root: Rscript datapage/flatten_xsl_datasets.R

devtools::load_all(quiet = TRUE)

out_dir <- "datapage/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# data-raw/XSL-dataset-fields.csv was written to catalog candidate datasets
# for data-raw/add_external_datasets.R; it was never actually joined back
# onto xsl_datasets (DATASET.R reads it but never uses it), its
# `order_filename` only exactly matches 33 of the 53 condition labels, and it
# doesn't cover the original Kachergis 201-225/301 series at all (those
# predate the CSV). This alias map resolves the handful of known near-misses;
# everything else is either an exact match or has no metadata in this CSV.
label_aliases <- c(
  "Koehne2013-aaappp" = "KoehneTrueswellGleitman2013-aaappp",
  "Koehne2013-apapap" = "KoehneTrueswellGleitman2013-apapap",
  "Koehne2013-papapa" = "KoehneTrueswellGleitman2013-papapa",
  "Koehne2013-pppaaa" = "KoehneTrueswellGleitman2013-pppaaa",
  "orig_4x4"          = "orig_order4x4",
  "301"               = "301_training_R"
)

metadata <- read.csv("data-raw/XSL-dataset-fields.csv", stringsAsFactors = FALSE)
metadata_cols <- c("description", "experiment_name", "dataset_name", "citation",
                   "age_years", "link", "item_group", "adult_data", "sd_accuracy")

lookup_metadata <- function(label) {
  key <- if (label %in% metadata$order_filename) {
    label
  } else if (label %in% names(label_aliases)) {
    label_aliases[[label]]
  } else {
    NA_character_
  }
  if (is.na(key)) {
    c(list(metadata_match = "none"), setNames(as.list(rep(NA, length(metadata_cols))), metadata_cols))
  } else {
    row <- metadata[match(key, metadata$order_filename), metadata_cols]
    c(list(metadata_match = if (key == label) "exact" else "alias"), as.list(row))
  }
}

condition_rows <- list()
trial_rows <- list()
accuracy_rows <- list()

for (d in xsl_datasets) {
  voc_sz <- length(unique(unlist(d$train$words)))
  ref_sz <- length(unique(unlist(d$train$objects[!is.na(d$train$objects)])))
  n_trials <- length(d$train$words)

  meta <- lookup_metadata(d$label)
  condition_rows[[d$label]] <- c(
    list(label = d$label, condition = d$condition, n_subj = d$n_subj,
         n_trials = n_trials, voc_sz = voc_sz, ref_sz = ref_sz),
    meta
  )

  for (t in seq_len(n_trials)) {
    tr_w <- unlist(d$train$words[t]); tr_w <- tr_w[!is.na(tr_w)]
    tr_o <- unlist(d$train$objects[t]); tr_o <- tr_o[!is.na(tr_o)]
    trial_rows[[length(trial_rows) + 1]] <- list(
      label = d$label, trial = t,
      words = paste(tr_w, collapse = ","),
      objects = paste(tr_o, collapse = ",")
    )
  }

  if (!is.null(d$accuracy)) {
    for (i in seq_along(d$accuracy)) {
      accuracy_rows[[length(accuracy_rows) + 1]] <- list(
        label = d$label, word_index = i, accuracy = d$accuracy[i]
      )
    }
  }
}

conditions <- do.call(rbind.data.frame, c(condition_rows, list(stringsAsFactors = FALSE)))
trials <- do.call(rbind.data.frame, c(trial_rows, list(stringsAsFactors = FALSE)))
accuracy <- do.call(rbind.data.frame, c(accuracy_rows, list(stringsAsFactors = FALSE)))

write.csv(conditions, file.path(out_dir, "conditions.csv"), row.names = FALSE)
write.csv(trials, file.path(out_dir, "trials.csv"), row.names = FALSE)
write.csv(accuracy, file.path(out_dir, "accuracy.csv"), row.names = FALSE)

cat("conditions:", nrow(conditions), "rows\n")
cat("trials:", nrow(trials), "rows\n")
cat("accuracy:", nrow(accuracy), "rows\n")
cat("\nmetadata match coverage:\n")
print(table(conditions$metadata_match))
cat("\nconditions with no CSV metadata:\n")
print(conditions$label[conditions$metadata_match == "none"])
