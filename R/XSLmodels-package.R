#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_rows everything mutate
#' @importFrom ggplot2 aes coord_equal geom_line geom_tile ggplot labs xlim ylim
#' @importFrom purrr list_rbind map map2 map_lgl reduce transpose
#' @importFrom rlang .data set_names
#' @importFrom stats runif
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
## usethis namespace: end
NULL

# xsl_datasets is package data (see R/data.R), referenced by name in
# get_group_model_fit(), get_crossvalidated_model_fit(), and show_datasets()
utils::globalVariables("xsl_datasets")
