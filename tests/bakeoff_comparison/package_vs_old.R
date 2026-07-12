library(XSLmodels)
library(testthat)
library(purrr)
library(rlang)

old <- readRDS("orig_xsl-bakeoff_model_test_runs.rds")

model_map <- list(
  baseline = baseline(),
  Bayesian_decay = inject(bayesian_decay(!!!old$Bayesian_decay$params)),
  kachergis = inject(uncfam(!!!old$kachergis$params, variant = "entropy")),
  novelty = inject(uncfam(!!!old$novelty$params, variant = "novelty")),
  strength = inject(uncfam(X = old$strength$params[1], C = old$strength$params[2],
                           B = 0, variant = "entropy")),
  decay = decay(old$decay$params),
  fazly = inject(fazly(!!!old$fazly$params)),
  fazlyt = inject(fazly(!!!old$fazlyt$params))
)

new <- model_map |> map(\(m) xsl_run(m, xsl_datasets))

check_sse <- \(m) {
  sse_old <- old[[m]]$SSE_per_cond |> unlist()
  sse_new <- new[[m]]$fits |> map_dbl(\(f) f$sse)
  map2_lgl(sse_old, sse_new, \(so, sn) all.equal(so, sn) |> is.logical())
}

sse_comp <- names(new) |> set_names() |> map(check_sse)

sse_comp |> keep(\(mc) all(mc)) |> names() # all right
sse_comp |> keep(\(mc) !any(mc)) |> names() # none right
sse_comp |> keep(\(mc) any(mc) & !all(mc)) |> names() # some but not all right
sse_comp |> map(\(mc) sum(mc)) # how many right

check_perf <- \(m) {
  perf_old <- old[[m]][1:44]
  perf_new <- new[[m]]$fits |> map(\(f) f$perf)
  perf_comp <- map2_lgl(perf_old, perf_new,
                        \(po, pn) all.equal(c(po), unname(pn)) |> is.logical())
  which(perf_comp)
}

check_perf("fazly")
check_perf("Bayesian_decay")

new$fazly$fits[[8]]$matrix


ns <- map(xsl_datasets, \(xd) c(length(unique(unlist(xd$train$words))),
                                length(unique(unlist(xd$train$objects))))) |>
  set_names(map_chr(xsl_datasets, \(xd) xd$label))
ns |> keep(\(x) x[1] != x[2]) # different num words and objects

xsl_datasets |> keep(\(xd) !is.null(xd$test)) # have test data

# old$Bayesian_decay[[1]]
# new$Bayesian_decay$fits[[1]]$perf
# new$Bayesian_decay$fits[[1]]$sims[[1]]$perf
# get_perf(new$Bayesian_decay$fits[[1]]$matrix, 1)

# runs[["fazly"]] <- run_model(combined_data, "fazly", c(1e-5, 8500), print_perf=F)
# runs[["fazly"]]$params <- c(1e-5, 8500)

# old$fazly$`201`
# new$fazly$fits[[1]]$perf

# xd <- list(words = xsl_datasets[[1]]$train$words,
#            objs = xsl_datasets[[1]]$train$objects)
#
# load("../xsl-bakeoff/data/combined_data.RData")
# old_fazly(old$fazly$params, combined_data[[1]]$train)$perf
# semiold_fazly(old$fazly$params, xd)$perf
#
# om <- old_fazly(old$fazly$params, xd)$matrix
# nm <- new$fazly$fits[[1]]$matrix

# TODO:
# check unweighted SSE
# fits not retaining condition name
# maybe sort fits$perf in numeric order ?

