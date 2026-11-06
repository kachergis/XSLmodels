# Comparing models: SSE, likelihood and cross-validation

This vignette walks through comparing models on the package’s data:
fitting each to a set of conditions, scoring the fits by SSE or by
likelihood, and checking them out of sample. To keep it quick it uses a
few conditions and small DEoptim searches. A real comparison would use
all of `xsl_datasets`, the default DEoptim settings and the registry’s
restarts (see
[`?xsl_fit`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)).

``` r

labels <- vapply(xsl_datasets, `[[`, "", "label")
conds <- xsl_datasets[labels %in% c("201", "212", "215", "220", "filt3E_6L", "freq369-3x3hiCD")]
# DEoptim advises a population of at least 10 per parameter; this one is
# smaller to keep the vignette fast, so its warnings about that are hidden
de <- DEoptim::DEoptim.control(NP = 15, itermax = 8, trace = FALSE)
models <- c("baseline", "decay", "uncfam", "rescorla_wagner")
```

## Fitting by SSE

By default a model is fit by minimizing the squared error between its
accuracy and people’s on each test item, averaged over conditions
weighted by their number of participants.
[`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)
looks up a model’s starting values, free parameters and bounds in the
package’s registry (see
[`show_models()`](https://www.kachergis.com/XSLmodels/reference/show_models.md)):

``` r

sse_fits <- lapply(models, \(m) get_group_model_fit(m, datasets = conds,
                                                    deoptim_control = de, seed = 1))
names(sse_fits) <- models
sapply(sse_fits, \(f) f$fit_result[[1]]$optim$bestval)
#>        baseline           decay          uncfam rescorla_wagner 
#>        1.830754        1.830830        0.729739        1.884718
```

SSE says how close the predictions are, but it isn’t a likelihood, so it
can’t weigh a better fit against extra parameters.

## Fitting by likelihood

Every `xsl_datasets` condition has a `response_matrix`: how many
participants chose each object on each test item. With
`xslControl(objective = "binomial")` a model is instead fit by the
likelihood of how many people got each item right. With
`objective = "multinomial"`, it is fit by the likelihood of which object
each person chose.

Both likelihoods include a lapse rate: the probability that a response
is a uniform guess among the alternatives offered. This keeps one
confident wrong prediction from making the likelihood infinitely bad. It
can be fixed (`xslControl(lapse = )`, default .01) or fit along with the
model’s parameters (`fit_lapse = TRUE`, or `"lapse"` in
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)’s
`free`):

``` r

binom <- xslControl(objective = "binomial")
ll_fits <- lapply(models, \(m) get_group_model_fit(m, datasets = conds, control = binom,
                                                   deoptim_control = de, seed = 1,
                                                   fit_lapse = TRUE))
names(ll_fits) <- models
ic <- \(fits) do.call(rbind, lapply(fits, \(f) {
  r <- f$fit_result[[1]]
  data.frame(k = r$k, loglik = r$loglik, AIC = r$aic, BIC = r$bic,
             lapse = unname(r$optim$bestmem["lapse"]))
}))
ic(ll_fits)
#>                 k    loglik      AIC      BIC     lapse
#> baseline        1 -1272.422 2546.844 2553.270 0.1308089
#> decay           2 -1274.676 2553.352 2566.205 0.1352742
#> uncfam          4  -492.257  992.514 1018.220 0.4548566
#> rescorla_wagner 4 -1290.376 2588.753 2614.458 0.1415091
```

`k` counts the fitted lapse too, and the BIC’s sample size is the number
of human responses (`n_obs`).

The likelihoods treat each response as an independent draw from the
model’s prediction. Real participants differ from one another, so the
absolute size of AIC or likelihood-ratio differences overstates the
evidence; rankings are more trustworthy than magnitudes.

## Which wrong answers?

Accuracy, and the binomial, only ask whether a choice was right. The
multinomial also scores *which* wrong object people chose, and errors
are often where models differ most.
[`choice_probabilities()`](https://www.kachergis.com/XSLmodels/reference/choice_probabilities.md)
gives each test item’s alternatives, the model’s probability of choosing
each one, and how many people did.

Here, for each fitted model, is how well its distribution over the wrong
answers matches people’s, pooled over every item in these conditions
(the correlation between the two across foils):

``` r

foil_cor <- function(mod, data) {
  pairs <- do.call(rbind, lapply(xsl_run(mod, data)$fits, \(f) {
    ch <- choice_probabilities(f$matrix, f$data)
    do.call(rbind, lapply(seq_len(nrow(ch)), \(i) {
      j <- match(ch$correct[i], ch$alternatives[[i]])
      q <- ch$p[[i]][-j]
      h <- ch$counts[[i]][-j]
      if (sum(h) == 0 || sum(q) == 0) return(NULL)
      data.frame(model = q / sum(q), people = h / sum(h))
    }))
  }))
  cor(pairs$model, pairs$people)
}
best <- \(m) ll_fits[[m]]$fit_result[[1]]$optim$bestmem
c(baseline = foil_cor(baseline(), conds),
  decay = foil_cor(decay(C = best("decay")[["C"]]), conds),
  uncfam = foil_cor(uncfam(X = best("uncfam")[["X"]], B = best("uncfam")[["B"]],
                           C = best("uncfam")[["C"]]), conds))
#>  baseline     decay    uncfam 
#> 0.3422153 0.3428880 0.1717520
```

In these rough fits, the co-occurrence models’ errors track people’s
better than
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)’s
do, even though
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
fits accuracy far better. A model fit only to accuracy (or to the
binomial) is never asked to get the errors right, and what fits accuracy
needn’t fit the errors.

Fitting by the multinomial uses all of this information:

``` r

multi <- xslControl(objective = "multinomial")
mn_fits <- lapply(c("decay", "uncfam"), \(m) get_group_model_fit(m, datasets = conds, control = multi,
                                                                  deoptim_control = de, seed = 1,
                                                                  fit_lapse = TRUE))
names(mn_fits) <- c("decay", "uncfam")
ic(mn_fits)
#>        k    loglik      AIC      BIC     lapse
#> decay  2 -3485.020 6974.040 6986.893 0.3163813
#> uncfam 4 -2395.722 4799.444 4825.149 0.4772301
```

## Cross-validation

In-sample fit rewards flexibility. Cross-validation instead fits each
model to all but one fold of conditions and scores it on the held-out
fold, so every condition is scored at parameters fit without it. With a
likelihood objective, it reports the held-out negative log-likelihood
(`test_nll`) alongside the held-out SSE:

``` r

cv <- lapply(c("decay", "uncfam"), \(m) get_crossvalidated_model_fit(
  m, n_folds = 3, datasets = conds, control = binom, deoptim_control = de,
  seed = 1, fit_lapse = TRUE)$cv_result)
names(cv) <- c("decay", "uncfam")
sapply(cv, \(r) c(held_out_nll = sum(r$test_nll), held_out_sse = mean(r$test_sse)))
#>                    decay     uncfam
#> held_out_nll 1520.668705 941.351826
#> held_out_sse    1.937776   2.382165
```

Lower is better for both, though the two criteria needn’t agree. Unlike
AIC and BIC, they need no assumption about how to count parameters. With
these small searches the fits are rough, so don’t read much into the
particular numbers.

## Generalization

The strongest test is data from a different experiment altogether. The
datasets in `xsl_holdout_datasets` are never used by the group or
cross-validated fits.
[`vignette("XSLmodels")`](https://www.kachergis.com/XSLmodels/articles/XSLmodels.md)
(section 11) shows how to score a fitted model on them.
