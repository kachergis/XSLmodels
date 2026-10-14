# Move the Suanda, Mugwanya & Namy (2014) and Koehne, Trueswell & Gleitman
# (2013) conditions out of xsl_datasets into standalone held-out datasets,
# `suanda2014` and `koehne2013`, as in the original CogSci bake-off paper.
#
# For these conditions only the condition-level mean accuracy is available
# (no per-item or per-participant data; `accuracy` is the mean replicated
# across items), so they can't contribute item-level information to group or
# cross-validated fits. They remain useful as generalization tests: compare a
# fitted model's mean predicted accuracy with the reported condition mean.
# Every remaining xsl_datasets condition has item-level accuracy and a
# response matrix.
#
# Run from the package root: Rscript data-raw/move_condition_means_to_holdout.R
# (then Rscript data-raw/add_xsl_holdout_datasets.R to update the registry).

devtools::load_all(".", quiet = TRUE)

labels <- vapply(xsl_datasets, `[[`, character(1), "label")
is_suanda <- grepl("^Suanda2014-", labels)
is_koehne <- grepl("^Koehne2013-", labels)
stopifnot(sum(is_suanda) == 3, sum(is_koehne) == 4)

suanda2014 <- setNames(xsl_datasets[is_suanda], labels[is_suanda])
koehne2013 <- setNames(xsl_datasets[is_koehne], labels[is_koehne])
# condition-level means only: every item's accuracy is the condition mean
stopifnot(all(vapply(c(suanda2014, koehne2013), \(d) length(unique(d$accuracy)) == 1, logical(1))))

xsl_datasets <- xsl_datasets[!(is_suanda | is_koehne)]
stopifnot(all(vapply(xsl_datasets, \(d) !is.null(d$response_matrix), logical(1))))

usethis::use_data(suanda2014, koehne2013, overwrite = TRUE)
usethis::use_data(xsl_datasets, overwrite = TRUE)
