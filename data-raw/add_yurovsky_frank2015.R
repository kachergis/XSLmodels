# Build `yurovsky_frank2015`: the three experiments of Yurovsky & Frank (2015),
# "An integrative account of constraints on cross-situational learning",
# Cognition 145, 53-62 (doi:10.1016/j.cognition.2015.07.013).
#
# Source: data-raw/YurovskyFrank2015/exp{1,2,3}_long.csv, copied verbatim from
# github.com/dyurovsky/XSIT-MIN (data/, commit fb44199). See
# data-raw/YurovskyFrank2015/PROVENANCE.md for the reproduction check.
#
# Design (Experiments 1 and 2). Each participant hears 8 novel words twice. On
# a word's first (exposure) trial they see `n_referents` novel objects (2, 3, 4
# or 8) and guess which one it names. Its second (test) trial comes `interval`
# trials later (1 = back-to-back; 1, 2, 4 or 8) and shows one object from the
# exposure trial among novel ones:
# - Same: the object the participant chose;
# - Switch (Exp. 1): an object they did not choose;
# - New Label (Exp. 2): an object they did not choose, but with a new word.
# Each participant gets 4 Same and 4 Switch (or New Label) test trials, in one
# of the 16 between-subjects Referents x Interval conditions. The CSVs hold
# only the test trials, and only included participants (Exp. 1: 1196, Exp. 2:
# 803, as reported). `chose_repeated` is the original `correct`: whether the
# participant chose the object carried over from the exposure trial (for New
# Label trials that is not the "correct" answer, so it gets a neutral name).
#
# `interval` is the original `intervalN + 1`, as in the paper's analyses,
# figures and model (t in M = gamma * t^-lambda): the raw `intervalN` (0, 1, 3,
# 7) counts the intervening trials, so a back-to-back test has interval 1.
# (The paper's Method section says "an Interval of 1, 2, 3, or 8"; the data
# have 1, 2, 4, 8.)
#
# Experiment 3 (the paper's Appendix). 2 or 4 referents (between subjects);
# each test trial for a word A shows one of A's exposure objects (the Target:
# the one the participant chose, Same, or one they didn't, Switch) and one of
# the intervening word B's exposure objects (the Competitor, Same or Switch
# likewise), plus novel objects in the 4-referent condition. `correct`: chose
# the Target; `familiar`: chose the Competitor. 465 workers, but 8 of them
# (`in_both_conditions`) took both the 2- and 4-referent versions and appear in
# both; the paper reports 231 (not 239) 4-referent participants, so it
# presumably meant to exclude their 4-referent session. They are kept here, as
# in the original analysis (which also pooled their two sessions under one
# random effect).
#
# Run from the package root: Rscript data-raw/add_yurovsky_frank2015.R

suppressMessages({
  library(dplyr)
  library(readr)
})

src <- "data-raw/YurovskyFrank2015"
read_raw <- function(f) suppressMessages(read_csv(file.path(src, f), show_col_types = FALSE,
                                                  name_repair = "unique_quiet"))

e12_raw <- bind_rows(read_raw("exp1_long.csv"), read_raw("exp2_long.csv"))
stopifnot(all(e12_raw$include), all(e12_raw$test == 1),
          !anyDuplicated(e12_raw[c("subid", "itemNum")]))

exp12 <- e12_raw |>
  transmute(
    experiment = as.integer(sub("Exp ", "", exp)),
    subject = as.integer(subid),
    n_referents = as.integer(numPicN),
    interval = as.integer(intervalN + 1),
    trial_type = factor(trialType, levels = c("Same", "Switch", "New Label")),
    item = as.integer(itemNum) + 1L,   # 1-8: the word's position in the test order
    trial = as.integer(trial.num),     # position in the whole session (incl. practice)
    chose_repeated = as.logical(correct),
    rt = as.numeric(rt)
  ) |>
  arrange(experiment, subject, trial)

e3_raw <- read_raw("exp3_long.csv") |> filter(trialType != "Practice")
both <- e3_raw |> distinct(worker, numPics) |> count(worker) |> filter(n > 1) |> pull(worker)

exp3 <- e3_raw |>
  transmute(
    subject = as.integer(worker),
    n_referents = as.integer(numPics),
    trial_type = factor(trialType, levels = c("Same vs. Same", "Same vs. Switch",
                                              "Switch vs. Same", "Switch vs. Switch")),
    target = factor(Target, levels = c("Same", "Switch")),
    competitor = factor(Competitor, levels = c("Same", "Switch")),
    item = as.integer(itemNum) + 1L,
    correct = as.logical(correct),
    familiar = as.logical(familiar),
    rt = as.numeric(rt),
    in_both_conditions = worker %in% both
  )

# Checks against the paper.
stopifnot(
  n_distinct(exp12$subject[exp12$experiment == 1]) == 1196,
  n_distinct(exp12$subject[exp12$experiment == 2]) == 803,
  all(count(exp12, subject)$n == 8),
  all(count(exp12, subject, trial_type)$n == 4),
  identical(range(count(distinct(filter(exp12, experiment == 1), subject, n_referents, interval),
                        n_referents, interval)$n), c(71L, 81L)),
  identical(range(count(distinct(filter(exp12, experiment == 2), subject, n_referents, interval),
                        n_referents, interval)$n), c(41L, 55L)),
  n_distinct(exp3$subject) == 465, length(both) == 8,
  identical(count(distinct(exp3, subject, n_referents), n_referents)$n, c(234L, 239L)),
  !any(exp3$correct & exp3$familiar)
)

yurovsky_frank2015 <- list(exp12 = exp12, exp3 = exp3)
usethis::use_data(yurovsky_frank2015, overwrite = TRUE)
