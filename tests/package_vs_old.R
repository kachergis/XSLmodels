require(XSLmodels)
require(testthat)

old <- readRDS("tests/orig_xsl-bakeoff_model_test_runs.rds")


runs <- list()
runs[["baseline"]] <- xsl_run(baseline(), xsl_datasets)

# could compare:
expect_equal(old$baseline$SSE, runs$baseline$sse)
expect_equal(old$baseline$SSE, runs$baseline$unweighted_sse) # ??? - unweighted not dividing by # of conditions?

# TODO:
# check unwieghted SSE
# fits not retaining condition name
# maybe sort fits$perf in numeric order ?

# condition-level comparison:
old$baseline$`201` == runs$baseline$fits[[1]]$perf #

runs[["Bayesian_decay"]] <- run_model(combined_data, "Bayesian_decay", c(.9, 1.1, 1), print_perf=T)
runs[["Bayesian_decay"]]$params <- c(.9, 1.1, 1)

runs[["kachergis"]] <- run_model(combined_data, "kachergis", c(.04, 2, .96), print_perf=T)
runs[["kachergis"]]$params <- c(.04, 2, .96)

runs[["novelty"]] <- run_model(combined_data, "novelty", c(.04, 2, .96), print_perf=T)
runs[["novelty"]]$params <- c(.04, 2, .96)

runs[["strength"]] <- run_model(combined_data, "strength", c(.04, .96), print_perf=T)
runs[["strength"]]$params <- c(.04, .96)

runs[["decay"]] <- run_model(combined_data, "decay", c(.96), print_perf=T)
runs[["decay"]]$params <- c(.96)

runs[["fazly"]] <- run_model(combined_data, "fazly", c(1e-5, 8500), print_perf=F)
runs[["fazly"]]$params <- c(1e-5, 8500)

runs[["fazlyt"]] <- run_model(combined_data, "fazlyt", c(1e-5, 8500), print_perf=F)
runs[["fazlyt"]]$params <- c(1e-5, 8500)
