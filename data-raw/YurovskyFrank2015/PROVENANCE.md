# Yurovsky & Frank (2015): provenance and reproduction check

Yurovsky, D., & Frank, M. C. (2015). An integrative account of constraints on
cross-situational learning. *Cognition*, 145, 53–62.
doi:10.1016/j.cognition.2015.07.013

## Source

`exp1_long.csv`, `exp2_long.csv` and `exp3_long.csv` are copied verbatim from
<https://github.com/dyurovsky/XSIT-MIN>, `data/`, at commit
`fb441993e03348006427d2223bd3417411d0bce1` ("added acknowledgements"). The
paper says the raw data and model code are available at that repository. The
repository has no license file.

`data-raw/add_yurovsky_frank2015.R` builds the package dataset
`yurovsky_frank2015` from them. The package's models, `yf2015_predict()` and
`yf2015_fit()` (`R/model-yurovsky_frank2015.R`), port the repository's
`models/*.stan.R` and `models/model.helpers.R`.

## Reproduction check (November 2026)

We re-ran the repository's analyses (`analysis/*.Rmd`) with R 4.6.0, lme4 and
rstan 2.32.7. `repro/repro_stats.R` re-runs the statistics, and
`repro/repro_stan.R` re-runs the model fits. The `repro/*.stan` files are the
original Experiment 1 models. Only their syntax is updated: current Stan no
longer accepts the old array declarations or `<-` assignment.

### What reproduces

- **Data.** The sample sizes match the paper: Experiment 1 has 1196
  participants (71–81 per condition), and Experiment 2 has 803 (41–55). The
  CSVs hold only included participants.
- **Statistics.** Tables 1, 2 and A1 match to two decimals. One z value is
  −6.19 here against −6.20 in Table 1. The extreme values quoted for the
  comparisons with chance also match: β = .393, z = 2.55 (Exp. 1); β = 1.39,
  z = 8.17 (Exp. 2, Same); β = .74, z = 2.87 (Switch vs. New Label).
- **Models (Table 3).** With the original code, the Stan fits reproduce Table
  3 exactly (`TRANSPOSE=1`):

  | Model | "Log Likelihood" | BIC | E1 r² | E1+2 r² |
  |---|---|---|---|---|
  | Statistical Accumulation | −6565 | 13146 (paper: 13145) | .33 | .66 |
  | Single Referent | −5950 | 11915 | .83 | .77 |
  | Integrated | −5590 | 11203 | .95 | .97 |

  The posterior means behind these numbers are not reported in the paper.
  They are: integrated σ = .604, λ = .184, γ = 1.495; accumulation
  λ = .266, γ = 1.846; single referent λ = .118, γ = .873.

### Problems found

1. **The model fits use transposed conditions.** `fit_models.Rmd` builds the
   Stan index matrices (`B_s`, `E_s`, `B_o`, `E_o`) with `t(matrix(...))`.
   That gives an [interval, referents] layout, but the Stan models read them
   as `[PicConds, IntConds]`, i.e. [referents, interval]. So the data of the
   condition with the i-th interval and j-th number of referents are scored
   as the condition with the i-th number of referents and j-th interval.
   - For example, the 2-referent, interval-8 participants are scored as
     8-referent, interval-1.
   - The r²s are still computed against correctly labelled data.
   - With the index fixed (`TRANSPOSE=0`):

     | Model | lp | BIC | E1 r² | E1+2 r² | Posterior means |
     |---|---|---|---|---|---|
     | Statistical Accumulation | −5914 | 11845 | .32 | .67 | λ = .101, γ = 1.769 |
     | Single Referent | −5318 | 10651 | .83 | .77 | λ = .172, γ = .920 |
     | Integrated | −4844 | 9712 | .95 | .98 | σ = .568, λ = .167, γ = 1.617 |

   - The r²s and the model ranking are essentially unchanged, so the paper's
     conclusions stand. The likelihoods, BICs and parameter estimates are
     different, though.
   - `tests/testthat/test-yurovsky_frank2015.R` checks both versions:
     `yf2015_fit()` on the correct data matches the fixed Stan fit, and on
     transposed data it recovers the paper's.
2. **"Log Likelihood" in Table 3 is the mean Stan `lp__`.** That is the log
   posterior up to a constant. It omits the binomial coefficients, adds the
   Jacobians of the parameter constraints, and is averaged over the posterior
   rather than maximized. The BIC is computed from it, with
   n = 2392 participant × trial-type counts.
   - The true maximized log-likelihoods (correct data, with binomial
     coefficients) are −4157.2 (accumulation), −3560.2 (single referent) and
     −3085.5 (integrated).
   - The corresponding BICs are 8329.9, 7136.0 and 6194.2. This is what
     `yf2015_fit()` reports.
3. **The New Label model is undocumented.** The paper says Experiment 2 is
   predicted from Experiment 1's parameters, but never says how New Label
   trials are modelled. The code (`model.helpers.R`, `prop.from.pred()`) uses
   a strength of Switch / 4 and P = (1 − M)/N − M. The 1/4 is not motivated
   anywhere, and the formula is not a proper mixture: it can go negative.
   This choice affects only the E1+2 r². `yf2015_predict()` exposes it as
   `new_label_scale`.
4. **Experiment 3 sample.** The paper reports 231 participants in the
   4-referent condition. The data have 239: 8 worker IDs appear in both the
   2- and 4-referent conditions. They are presumably repeat participants
   whose second HIT should have been excluded. The original analysis kept
   them, and pooled each worker's two sessions under one random intercept.
   The dataset flags them as `in_both_conditions`.
5. **Minor text discrepancies.**
   - The Experiment 1 Method gives the intervals as "1, 2, 3, or 8". The data
     and analyses use 1, 2, 4, 8 (raw intervening trials 0, 1, 3, 7, plus 1).
   - Some p-value claims are slightly off:
     - Exp. 1 vs. chance says "all ps ≤ .01", but the largest p is .011.
     - New Label at 3 referents, interval 1, has p = .051, against "all
       ps ≤ .05".
     - New Label at 4 referents, interval 1, is significantly below chance
       (p = .013), although the text says the 4- and 8-referent differences
       were not reliable.
     - Switch vs. New Label says "all ps ≤ .001", but the largest p is .005.
   - The model text gives "R 3.13" and "rstan 2.60" as software versions.
     Neither version exists.

## The trial-level model

`yf2015()` (`R/model-yf2015.R`) generalizes the Integrated model to any
training sequence. `yf2015_paradigm()` runs it through these experiments.
With `mu = 0` it reproduces `yf2015_predict()`'s Same and Switch predictions
exactly (`tests/testthat/test-yf2015.R`).

It replaces the ad hoc New Label rule with a mutual-exclusivity strength `mu`:
guesses avoid objects associated with other words. With σ, λ and γ held at
the Experiment 1 estimates (σ = .568, λ = .167, γ = 1.617), maximum
likelihood on the data gives:

- **Experiment 2 (New Label).** μ = 1.41. The negative log-likelihood is 49.8,
  against 136.6 with μ = 0 (chance) and 51.8 for the paper's rule. The
  correlation with the 16 condition means is .94, against .92 for the paper's
  rule.
- **Experiment 3.** μ = 0, so mutual exclusivity doesn't help. The data do
  show its signature: learners avoid a competitor they hypothesized for the
  other word more than one they didn't. But they choose competitors more
  often overall than the model predicts (at 2 referents with a Switch target:
  .19 and .36, against .19 for both). That is a pull toward the familiar
  object, which a mutual-exclusivity term can only oppose.
