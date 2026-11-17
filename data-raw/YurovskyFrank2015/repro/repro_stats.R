# Re-run of the statistics in XSIT-MIN/analysis/e1and2.analysis.Rmd and
# e3.analysis.Rmd (Yurovsky & Frank, 2015): comparisons with chance and the
# mixed-effects models of Tables 1, 2 and A1.
#
# Run from the package root (needs lme4): Rscript data-raw/YurovskyFrank2015/repro/repro_stats.R

suppressMessages({library(dplyr); library(readr); library(lme4)})
here <- "data-raw/YurovskyFrank2015"
rd <- function(f) read_csv(file.path(here, f), show_col_types = FALSE, name_repair = "unique_quiet")
logit <- function(x) log(x / (1 - x))
glmer_ctrl <- glmerControl(optimizer = "bobyqa")

d <- bind_rows(rd("exp1_long.csv"), rd("exp2_long.csv")) |>
  transmute(exp, subid, correct, numPicN, intervalN = intervalN + 1,
            trialType = factor(trialType, levels = c("Same", "Switch", "New Label")),
            log.numPic = log2(numPicN), log.interval = log2(intervalN), off = logit(1 / numPicN))

# Correct ~ 1 + offset(logit(1/Referents)) [+ term], per condition
chance <- function(data, groups, f, row = 1) data |> group_by(across(all_of(groups))) |>
  group_modify(~ {
    s <- summary(glm(f, offset = off, family = "binomial", data = .x))$coef
    tibble(beta = s[row, 1], z = s[row, 3], p = s[row, 4])
  }) |> ungroup()

cat("Exp 1 vs chance (paper: smallest beta = .393, z = 2.55, all ps <= .01)\n")
print(chance(filter(d, exp == "Exp 1"), c("trialType", "numPicN", "intervalN"), correct ~ 1) |>
        slice_min(beta))
e2 <- chance(filter(d, exp == "Exp 2"), c("trialType", "numPicN", "intervalN"), correct ~ 1)
cat("\nExp 2 Same vs chance (paper: smallest beta = 1.39, z = 8.17)\n")
print(filter(e2, trialType == "Same") |> slice_min(beta))
cat("\nExp 2 New Label vs chance (paper: 2 and 3 Referents below chance, largest beta = -.32, z = 1.95, all ps <= .05;",
    "4 and 8 Referents n.s.)\n")
print(filter(e2, trialType == "New Label") |> arrange(numPicN, intervalN), n = 16)
cat("\nSwitch vs New Label (paper: smallest beta = .74, z = 2.87, all ps <= .001)\n")
print(d |> filter(trialType != "Same") |>
        mutate(trialType = factor(as.character(trialType), levels = c("New Label", "Switch"))) |>
        chance(c("numPicN", "intervalN"), correct ~ 1 + trialType, row = 2) |> arrange(beta) |> head(3))

cat("\nTable 1\n")
m1 <- glmer(correct ~ log.numPic * trialType + trialType * log.interval + (trialType | subid),
            offset = off, family = "binomial", control = glmer_ctrl, data = droplevels(filter(d, exp == "Exp 1")))
print(round(summary(m1)$coef, 2))
cat("\nTable 2\n")
m2 <- glmer(correct ~ log.numPic + trialType + trialType * log.interval + (trialType | subid),
            offset = off, family = "binomial", control = glmer_ctrl, data = droplevels(filter(d, exp == "Exp 2")))
print(round(summary(m2)$coef, 2))

e3 <- rd("exp3_long.csv") |> filter(trialType != "Practice") |> mutate(off = logit(1 / numPics))
cat("\nExp 3 participants per condition (paper: 234 and 231)\n")
print(e3 |> distinct(worker, numPics) |> count(numPics))
cat("\nTable A1\n")
m3 <- glmer(correct ~ log(numPics) * Target + log(numPics) * Competitor + (1 | worker),
            offset = off, family = "binomial", control = glmer_ctrl, data = e3)
print(round(summary(m3)$coef, 2))
