# Add 3 more temporal-contiguity conditions to xsl_datasets, from the same
# paper as the existing "1_max_temp_spat_cont_orig"/"4_no_spat_orig_max_tc"
# conditions: Kachergis, Yu, & Shiffrin (2009), "Temporal contiguity in
# cross-situational statistical learning."
#
# data-raw/XSL-dataset-fields.csv documents 6 temporal-contiguity "overlap"
# conditions that were never pulled into xsl_datasets -- 3 matched pairs,
# each a "contr" (control) order and a non-control order at the same
# overlap degree:
#   1olap3tr_contr        / temp_cont_1olap3tr   (1 overlap per 3 trials)
#   temp_cont_1olap2tr_contr / temp_cont_1olap2tr   (1 overlap per 2 trials)
#   temp_cont_2olap2tr_contr / temp_cont_2olap2tr   (2 overlap per 2 trials)
#
# Only the 3 "_contr" conditions are added here. Real subject-level data for
# them was found in data-raw/agg_data/x13_1-21.txt (export_data.R loads this
# as `x13` and comments "Conds: 1_ is 1olap2tr_contr, 2_ is 1olap3tr_contr,
# 3_ is [2]olap2tr_contr" -- its own generic "1_/2_/3_/4_" condition labels
# carry no string evidence of which named order file they are, so this
# mapping is taken from the script author's own comment, not independently
# re-derived here). x13's 4th condition ("369_39mx") is an unrelated
# frequency/context-diversity design already in xsl_datasets as
# "freq369_39mx" and is not touched.
#
# The 3 non-"_contr" conditions (temp_cont_1olap2tr/1olap3tr/2olap2tr) are
# NOT added: despite having their own order files in data-raw/orders/, no
# subject-level accuracy data for them was found anywhere in this repo
# (checked every .txt/.RData/.rda file's condition-name-like columns for a
# literal match). They may never have been run with real participants, or
# their data lives outside this repo -- flagged rather than guessed.
#
# Run from the package root: Rscript data-raw/add_temporal_contiguity_overlap.R

devtools::load_all(quiet = TRUE)

citation_desc <- paste(
  "Kachergis, G., Yu, C., & Shiffrin, R. M. (2009). Temporal contiguity in",
  "cross-situational statistical learning. Proceedings of the 31st Annual",
  "Meeting of the Cognitive Science Society. 4 words and 4 objects per",
  "training trial (symmetric design), 27 trials, 18-word vocabulary -- a",
  "reordering of the original 4x4 schedule manipulating how many trials",
  "separate a pair's repeated co-occurrences (temporal contiguity)."
)

build_condition <- function(label, condition_desc, order_file, x13_cond) {
  ord <- read.table(file.path("data-raw/orders", paste0(order_file, ".txt")),
                    header = FALSE, sep = "\t")
  stopifnot(ncol(ord) == 4, length(unique(unlist(ord))) == 18)
  train <- list(words = lapply(seq_len(nrow(ord)), \(i) unlist(ord[i, ])),
               objects = lapply(seq_len(nrow(ord)), \(i) unlist(ord[i, ])))

  x13 <- read.csv("data-raw/agg_data/x13_1-21.txt", sep = "\t")
  d <- x13[x13$Condition == x13_cond, ]
  acc <- aggregate(Correct ~ CorrectAns, data = d, FUN = mean)
  stopifnot(nrow(acc) == 18)

  xslData(
    train = train,
    test = list(),
    accuracy = acc$Correct[order(acc$CorrectAns)],
    n_subj = length(unique(d$Subject)),
    label = label,
    condition = condition_desc,
    description = paste(citation_desc, "This condition:", condition_desc,
                        "(control order). See data-raw/add_temporal_contiguity_overlap.R.")
  )
}

new_conditions <- list(
  build_condition("1olap3tr_contr", "1 overlap per 3 trials, contr",
                  "1olap3tr_contr", "2_"),
  build_condition("temp_cont_1olap2tr_contr", "1 overlap per 2 trials, contr",
                  "temp_cont_1olap2tr_contr", "1_"),
  build_condition("temp_cont_2olap2tr_contr", "2 overlap per 2 trials, contr",
                  "temp_cont_2olap2tr_contr", "3_")
)

for (cd in new_conditions) print(cd)

xsl_datasets <- c(xsl_datasets, new_conditions)
usethis::use_data(xsl_datasets, overwrite = TRUE)
