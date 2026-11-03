# Writing your own model or dataset

Every model in `XSLmodels` is an `xslMod`: a learning function plus its
parameters. Every dataset is an `xslData`: training trials, test trials
and, optionally, human responses. Anything that follows these two
contracts works with
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md),
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md),
cross-validation and the evaluation functions. This vignette builds one
of each.

## A new model

### The learning function

A model function takes three arguments and returns an `xslFit`:

- `params`: a named list of the model’s parameters.
- `data`: the training trials, a list of `words` and `objects` with one
  entry per trial.
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  relabels both as positions `1..n` in their sorted vocabularies before
  calling the model, so the model can index its matrix with them
  directly, and it puts the original labels back afterwards. Missing
  words or objects on a trial are `NA`.
- `control`: an
  [`xslControl()`](https://www.kachergis.com/XSLmodels/reference/xslControl-class.md).
  A model should honour `reps` (train on the sequence this many times,
  recording performance after each block) and `keep_traj` (record the
  matrix after every trial only when asked), and should use
  `start_matrix` if it declares support for it.

It returns `xslFit(perf = , matrix = , traj = )`, where `matrix` is the
word-by-object matrix of association strengths that test trials are
scored on, and `perf` holds each word’s probability of choosing its
referent: one row per training block
([`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md)
computes it with Luce choice).

As an example, here is a “rich get richer” learner. On each trial, each
word shares one unit of associative weight among the objects present, in
proportion to its current association with each, plus a constant `eps`.
Everything decays by `C` per trial. With a small `eps` a word quickly
commits to the objects it has already been associated with; a large
`eps` spreads the weight evenly, which amounts to counting
co-occurrences.

``` r

rich_get_richer_model <- function(params, data, control) {
  C <- params[["C"]]
  eps <- params[["eps"]]
  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects)))
  m <- if (is.null(control$start_matrix)) matrix(0, length(voc), length(ref)) else control$start_matrix
  dimnames(m) <- list(voc, ref)
  perf <- matrix(0, control$reps, length(voc))
  traj <- list()
  for (rep in seq_len(control$reps)) {
    for (t in seq_along(data$words)) {
      w <- stats::na.omit(data$words[[t]])
      o <- stats::na.omit(data$objects[[t]])
      m <- m * C
      if (length(w) && length(o)) {
        share <- m[w, o, drop = FALSE] + eps
        m[w, o] <- m[w, o] + share / rowSums(share)
      }
      if (isTRUE(control$keep_traj)) traj[[length(traj) + 1]] <- m
    }
    perf[rep, ] <- get_perf(m)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}
```

### The constructor

A constructor wraps the function and its parameter values in an
`xslMod`. Set `stochastic = TRUE` if the model draws random numbers, so
that
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
averages `control$n_sim` simulations, and `supports_start_matrix = TRUE`
only if the function really uses `control$start_matrix` (otherwise
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
stops rather than silently ignoring one).

``` r

rich_get_richer <- function(C, eps) {
  xslMod(name = "rich_get_richer",
         description = "Shares each word's weight in proportion to its current associations",
         model = rich_get_richer_model, params = list(C = C, eps = eps),
         stochastic = FALSE, supports_start_matrix = TRUE)
}
```

The new model now runs, and is scored, like any other. Here it is on
five conditions where what a learner already knows matters: the
co-occurrence
[`baseline()`](https://www.kachergis.com/XSLmodels/reference/baseline.md)
fits them badly.

``` r

labels <- vapply(xsl_datasets, `[[`, "", "label")
conds <- xsl_datasets[labels %in% c("215", "216", "219", "220", "225")]
run <- xsl_run(rich_get_richer(C = .97, eps = .5), conds)
run$sse
#> [1] 0.6263171
xsl_run(baseline(), conds)$sse
#> [1] 2.189507
```

### Fitting it

[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
searches the parameters named in `free` (by default, the leading
parameters) within the bounds given, here with two restarts and a seed
for reproducibility. A small DEoptim search keeps this vignette quick;
use the defaults for a real fit.

``` r

fit <- xsl_fit(rich_get_richer(C = .97, eps = .5), conds,
               lower = c(.5, .001), upper = c(1, 10), free = c("C", "eps"),
               n_restarts = 2, seed = 1,
               deoptim_control = DEoptim::DEoptim.control(NP = 20, itermax = 15, trace = FALSE))
fit[[1]]$optim$bestmem
#>         C       eps 
#> 0.9950217 0.2308525
fit[[1]]$optim$bestval
#> [1] 0.2977749
fit[[1]]$restarts
#> [1] 0.2979941 0.2977749
fitted <- update_params(rich_get_richer(C = .97, eps = .5), fit[[1]]$optim$bestmem,
                        free = c("C", "eps"))
```

### Checking it

A few properties are worth checking for any new model. These are the
checks `tests/testthat/test-invariants.R` applies to every model in the
package.

- **Output is valid on every condition:** associations are finite and
  accuracies lie in
  ``` math
  0, 1
  ```
  .
- **Output is valid at the edges of the fitting bounds,** where
  numerical problems tend to hide.
- **Labels don’t matter:** relabelling the words and objects shouldn’t
  change the predictions.

``` r

corners <- list(c(.5, .001), c(.5, 10), c(1, .001), c(1, 10))
runs <- lapply(corners, \(p) xsl_run(rich_get_richer(C = p[1], eps = p[2]), xsl_datasets))
all(vapply(runs, \(r) all(vapply(r$fits, \(f) all(is.finite(f$matrix)) &&
                                   all(f$perf >= 0 & f$perf <= 1), TRUE)), TRUE))
#> [1] TRUE
```

## A new dataset

An `xslData` needs training trials. To compare a model with people, it
also needs the tested items and human accuracy.

- **`train`:** a list of `words` and a list of `objects`, one entry per
  trial. Labels can be numbers or strings, and a word’s correct referent
  is the object with the same label.
- **`test`** (optional): the tested `words` and, for each test trial,
  the `objects` offered. Without a `test`, each word is tested among all
  objects.
- **`test$correct`** (optional): when the correct choice isn’t the
  word’s own object. For example, the filtering conditions test each
  word with its own object absent, where the correct choice is the
  object it was paired with.
- **`accuracy` and `n_subj`:** the proportion correct per test item and
  the number of participants.
- **`response_matrix`** (optional): for each test item, how many
  participants chose each object, with one column per object in sorted
  label order. It supports choice-level likelihoods and
  [`choice_probabilities()`](https://www.kachergis.com/XSLmodels/reference/choice_probabilities.md).

Here is a small three-word design with string labels, tested 3AFC. “dax”
appears on three trials, the others on two:

``` r

my_data <- xslData(
  train = list(words = list(c("dax", "wug"), c("dax", "fep"), c("wug", "fep"), "dax"),
               objects = list(c("dax", "wug"), c("dax", "fep"), c("wug", "fep"), "dax")),
  test = list(words = list("dax", "wug", "fep"),
              objects = list(c("dax", "wug", "fep"), c("dax", "wug", "fep"), c("dax", "wug", "fep"))),
  accuracy = c(.8, .5, .4), n_subj = 10,
  # columns: objects in sorted order (dax, fep, wug)
  response_matrix = rbind(c(8, 1, 1), c(3, 2, 5), c(4, 4, 2)),
  label = "toy", condition = "3 words, 1-2 per trial")
my_data
#> xslData object with label "toy" and condition "3 words, 1-2 per trial"
#>   training trials: 4
#>       test trials: 3
#>             words: 3
#>           objects: 3
#>        accuracies: 3
#>          subjects: 10
```

[`xslData()`](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
checks that the pieces fit together: one accuracy and one
response-matrix row per test item, and one column per object. Each row
of the response matrix should also reproduce the item’s accuracy: here,
8 of 10 participants chose `dax` for “dax”. (Note that 4 of 10 chose
`fep` for “fep”, and `fep` is the second column.)

The model can be scored against it, at the level of accuracy or of the
individual choices:

``` r

f <- xsl_run(fitted, my_data)$fits[[1]]
f$perf
#> [1] 0.7543542 0.6300001 0.6299896
ch <- choice_probabilities(f$matrix, my_data)
ch
#> # A tibble: 3 × 5
#>    item correct alternatives p         counts   
#>   <int> <chr>   <list>       <list>    <list>   
#> 1     1 dax     <chr [3]>    <dbl [3]> <dbl [3]>
#> 2     2 wug     <chr [3]>    <dbl [3]> <dbl [3]>
#> 3     3 fep     <chr [3]>    <dbl [3]> <dbl [3]>
# what the model predicts, and what people chose, for "wug"
wug <- rbind(model = ch$p[[2]], people = ch$counts[[2]] / sum(ch$counts[[2]]))
colnames(wug) <- ch$alternatives[[2]]
round(wug, 2)
#>         dax  wug  fep
#> model  0.25 0.63 0.12
#> people 0.30 0.50 0.20
```

See
[`vignette("model-comparison")`](https://www.kachergis.com/XSLmodels/articles/model-comparison.md)
for fitting and comparing models by choice-level likelihood.
