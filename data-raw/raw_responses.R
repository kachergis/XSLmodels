# Load every raw test response from the Kachergis-lab experiments into one
# long table, for data-raw/extract_response_matrices.R and
# data-raw/fix_freqcd_add_responses.R.
#
# Columns: source, raw_cond, block, subject, word (the tested word),
# correct_obj (the correct object on that test; differs from `word` only on
# the filtering AtoB tests), response, correct, group (source | raw_cond | block).
#
# Exclusions already applied in the sources: in freqCD_item_data.Rdata
# (Kachergis, Yu & Shiffrin's frequency / contextual-diversity experiments;
# copied from the original analysis files), participants whose accuracy across
# all their conditions was at or near chance were excluded (e.g. x8: the 3
# lowest, at .056-.069; participants at .097 were kept). In
# priming_all_trajectory.RData, participants whose accuracy across all 4
# blocks was below 2/18 were excluded (matches all 186 raw participants), and a
# brief extra test of 4 novel items (19-22) in the 3-pairs experiment was
# dropped. Filtering: participants 610 and 621, whose test codes are shifted by
# 6 in the *_6L conditions, are excluded here (see fix_filtering_tests.R).

suppressMessages({library(dplyr)})

load_raw_responses <- function(root = ".") {
  path <- function(...) file.path(root, "data-raw", ...)
  rd <- function(f) read.csv(path("agg_data", f), sep = "\t")
  agg <- function(f, src) {
    x <- rd(f)
    cc <- intersect(c("Condition", "ExperimentName"), names(x))[1]
    tibble(source = src, raw_cond = as.character(x[[cc]]), block = 1L, subject = paste0(src, "_", x$Subject, "_", x$Session),
           word = as.integer(x$CorrectAns), correct_obj = as.integer(x$CorrectAns),
           response = as.integer(x$Response), correct = as.integer(x$Correct))
  }
  raw <- list(
    agg("x4-prob_6-26.txt", "x4"), agg("x6-tc_data9-9.txt", "x6"), agg("x7-data-10-19.txt", "x7"),
    agg("x8_9-22-08.txt", "x8"), agg("x9_10-19.txt", "x9"), agg("x10-11-12.txt", "x10"),
    agg("x12-11-12.txt", "x12"), agg("x13_1-21.txt", "x13")
  )
  e <- new.env(); load(path("data", "asym_test_data.RData"), e)
  raw[["asym"]] <- e$tdat |> transmute(source = "asym", raw_cond = as.character(condnum), block = 1L, subject = as.character(Subject),
                                       word = as.integer(Word), correct_obj = as.integer(Word),
                                       response = as.integer(Response), correct = as.integer(Correct))
  e <- new.env(); load(path("data", "freqCD_item_data.Rdata"), e)
  raw[["freqCD"]] <- e$raw |> transmute(source = "freqCD", raw_cond = file, block = 1L, subject = paste0("freqCD_", Exp, "_", Subject, "_", Session),
                                        word = as.integer(CorrectAns), correct_obj = as.integer(CorrectAns),
                                        response = as.integer(Response), correct = as.integer(Correct))
  e <- new.env(); load(path("data", "priming_all_trajectory.RData"), e)
  raw[["priming"]] <- e$all |> transmute(source = "priming", raw_cond = Exp, block = as.integer(Block), subject = paste0("prime_", Subject),
                                         word = as.integer(CorrectAns), correct_obj = as.integer(CorrectAns),
                                         response = as.integer(Response), correct = as.integer(Correct))
  e <- new.env(); load(path("data", "filtering_data.RData"), e)
  partner_of <- function(w) ifelse(w <= 6, w + 6, w - 6)
  filt <- e$raw |> mutate(order = paste0("filt", EarlyRepetitions, "E_", Late, "L"))
  filt_bad <- filt |> filter((testtype == "lateatoa" & Word <= 6) | CorrectAns > 12) |> distinct(order, Subject, Session)
  raw[["filtering"]] <- filt |> anti_join(filt_bad, by = c("order", "Subject", "Session")) |>
    transmute(source = "filtering", raw_cond = paste0(order, ifelse(AtoA, " AtoA", " AtoB")), block = 1L,
              subject = paste0(Subject, "_", Session), word = as.integer(Word), correct_obj = as.integer(CorrectAns),
              response = as.integer(Response), correct = as.integer(Correct))
  bind_rows(raw) |> mutate(group = paste(source, raw_cond, paste0("b", block), sep = " | "))
}
