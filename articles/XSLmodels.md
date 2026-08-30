# Overview of \`XSLmodels\`: Importing cross-situational datasets and fitting models

The main goal of the `XSLmodels` package is to enable researchers to run
and optimize a collection of cross-situational word learning (XSL)
models on one or more datasets from cross-situational word learning
experiments–and relevant corpora. This vignette provides an overview of
the structure and definition of XSL datasets – covering both datasets
provided in the packages, and how to import your own data – and then
turns to discussing how to run, optimize, and even create your own XSL
model.

## Defining and structure of cross-situational learning (XSL) datasets.

Whether you already have human data from an experiment that you would
like to fit models to, or if you would like to simply evaluate model
performance on a given experiment’s training trials, this vignette will
show to 1) structure training trials, 2) examine the structure of an
experiment, 3) run model(s) through the training trials, and 4) specify
how the model(s) should be tested.

### 0. Importing a Dataset

Cross-situational word learning (XSL) datasets are defined with the
`xslData` class, which requires defining minimally a training phase and
a testing phase. Each trial of the training phase presents participants
(or models) with one or more words and one or more referents
(i.e. objects). In this package, each word $`w`$ and object $`o`$ is
referred to by index (i.e., $`w_i`$, $`o_j`$), and the to-be-learned
word-object pairs are assumed to have the same index
(i.e. $`w_i = o_i`$). Thus, each dataset defines a list `train` of
`words` and `objects` indices on each trial, also defined in lists (to
allow for a potentially varying number of words/objects per trial). The
`test` trials are defined by a list of the available objects on each
test trial. A dataset can also be given a short `label`, and a longer
(maybe per-item?) `condition`.

If you already have collected a sample of human participants, you can
optionally include per-item `accuracy`, a vector of length equal to the
number of to-be-learned items) and the number of participants
(`n_subj`). This sample size and accuracy can then be compared to model
performance, and if desired, used to fit model parameters.

Below we show how to define a new dataset.

``` r

xslData(train = list(words = list(c(1, 2), c(2, 3)),
                     objects = list(c(1, 2), c(2, 3))),
        test = list(words = list(1, 2, 3),
                    objects = list(1:3, 1:3, 1:3)),
        label = "example dataset",
        condition = "example condition")
#> xslData object with label "example dataset" and condition "example condition"
#>   training trials: 2
#>       test trials: 3
#>             words: 3
#>           objects: 3
#>        accuracies: 0
```

### 1. Structure of an experimental condition

[`get_example_ambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_ambiguous_condition.md)
and
[`get_example_unambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_unambiguous_condition.md)
return simple example experimental conditions, which consist of a list
of training *trials*, an optional vector *perf* containing the per-word
performance of people on the test trials, and an optional list of test
trials, enumerating each to-be-tested word and the set of referents to
be presented for choosing among. The list of training trials is further
composed of a list of the words presented on each trial, and a list of
the referents (*objs*) presented on each trial.

``` r

ag <- get_example_ambiguous_condition()
ag
#> xslData object with label "example condition" and condition "ambiguous"
#>   training trials: 2
#>       test trials: 0
#>             words: 4
#>           objects: 4
#>        accuracies: 4
```

Shown above, the example ambiguous condition has three training trials
(`ag$trials`). On the first trial words 1 and 2 appear
(`ag$trials$words[[1]]`) alongside referents 1 and 2
(`ag$trials$objs[[1]]`; order of words/referents within a trial is
irrelevant).

### 2. Viewing an experiment’s structure

One common way to summarize the structure of a cross-situational word
learning experiment is to make a matrix of the word-object
co-occurrences, which can be visualized with a heatmap to show the
strength of association between each word and object.
[`create_cooc_matrix()`](https://kachergis.github.io/XSLmodels/reference/create_cooc_matrix.md)
will return a word x object matrix showing the tallied co-occurrences of
each word with each object across all of the training trials.

``` r

create_cooc_matrix(ag$trials)
#> <0 x 0 matrix>
```

### 3. Included experimental conditions

A dataset combining 53 experimental conditions is included in the
package in `xsl_datasets`. For example, here is a summary of one
experimental condition in the dataset: an asymmetric (3x4; i.e. 3 words
and 4 objects per trial) condition with 36 training trials, 18 words and
18 objects with corresponding mean accuracies from 25 subjects. (Note
that `test trials: 0` indicates that this condition used 18AFC test
trials, on which all referents are given for each tested word.
Conditions which show a subset of the trained word-referent pairs
(e.g. 4AFC) specify a list of referents shown on for each tested word.)

``` r

xsl_datasets[[1]]
#> xslData object with label "201" and condition "3x4"
#>   training trials: 36
#>       test trials: 0
#>             words: 18
#>           objects: 18
#>        accuracies: 18
#>          subjects: 25
```

The properties condition can be accessed and examined in detail:

``` r

# properties of an experimental condition:
names(xsl_datasets[[1]])
#> [1] "train"     "test"      "accuracy"  "n_subj"    "label"     "condition"
# e.g., mean accuracy for each tested word (i.e., P(choosing correct referent | word)):
xsl_datasets[[1]]$accuracy
#>  [1] 0.28 0.12 0.16 0.32 0.08 0.08 0.36 0.16 0.04 0.12 0.20 0.40 0.44 0.20 0.12
#> [16] 0.08 0.20 0.08
```

## Running, fitting, and defining XSL models.

### 4. Run model with given parameters through a single dataset and pull SSE.

Now that the definition and structure of the datasets is clear, we will
turn to running and fitting models, before finally discussing the
structure of an `xslMod`, and how to extend an existing model or create
a new one.

``` r

run1 <- xsl_run(uncfam(X = .1, C = 1, B = .98), data = xsl_datasets[[1]])
run1$sse
#> [1] 0.3936524
```

### 5. Run a model with given parameters through multiple datasets and pull SSE.

``` r

run3 <- xsl_run(uncfam(X = .1, C = 1, B = .98), data = xsl_datasets[1:3])
run3$sse
#> [1] 0.2854364
```

### 6. Evaluating model performance

Models return both a word-object matrix of associations (or
‘hypotheses’, i.e. binary-valued associations), as well as the
conditional probability of selecting the intended referent, given each
word (i.e., P(referent \| word)).

``` r

# Test different models on a subset of datasets
gt <- xsl_run(guess_and_test(f = .1, sa = .5), data = xsl_datasets[1:3])
gt$sse
#> [1] 3.767219

pt <- xsl_run(pursuit(gamma = .2, threshold = .3, lambda = .05), data = xsl_datasets[1:3])
pt$sse
#> [1] 8.757726

# Compare with baseline
bl <- xsl_run(baseline(), data = xsl_datasets[1:3])
bl$sse
#> [1] 0.2306
```

### 7. Using the helper functions

The package provides several helper functions to make it easier to work
with models and datasets:

``` r

# See what models are available
models <- show_models()
models
#>  [1] "baseline"           "decay"              "uncfam"            
#>  [4] "uncfam_attention"   "uncfam_predictive"  "uncfam_sampling"   
#>  [7] "multi_sampling"     "propose_but_verify" "pursuit"           
#> [10] "fazly"              "guess_and_test"     "rescorla_wagner"   
#> [13] "tilles"             "bayesian_decay"     "kalman_filter"     
#> [16] "softmax_rl"

# See what datasets are available
datasets <- show_datasets()
head(datasets)
#>   index label condition n_trials n_words n_objects n_subjects has_test
#> 1     1   201       3x4       36      18        18         25    FALSE
#> 2     2   202  3x4 1/.5       36      18        18         25    FALSE
#> 3     3   203 3x4 1/.66       36      18        18         25    FALSE
#> 4     4   204   3x4 +6o       36      18        24         20    FALSE
#> 5     5   205       2x4       54      18        18         33    FALSE
#> 6     6   206 3x3 +1w/o       54      18        18         39    FALSE
```

### 8. Model fitting

Below, we show this data can be used to optimize parameters for existing
models in the package.

``` r

# Fit a simple decay model to a single dataset
fit_result <- xsl_fit(decay(C = .98), data = xsl_datasets[[1]], 
                      lower = .8, upper = 1.0)
#> Iteration: 1 bestvalit: 0.310600 bestmemit:    0.970528
#> Iteration: 2 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 3 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 4 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 5 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 6 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 7 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 8 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 9 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 10 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 11 bestvalit: 0.310600 bestmemit:    0.979374
#> Iteration: 12 bestvalit: 0.310600 bestmemit:    0.977778
#> Iteration: 13 bestvalit: 0.310600 bestmemit:    0.977778
#> Iteration: 14 bestvalit: 0.310600 bestmemit:    0.987768
#> Iteration: 15 bestvalit: 0.310600 bestmemit:    0.991104
#> Iteration: 16 bestvalit: 0.310600 bestmemit:    0.991104
#> Iteration: 17 bestvalit: 0.310600 bestmemit:    0.991104
#> Iteration: 18 bestvalit: 0.310600 bestmemit:    0.978662
#> Iteration: 19 bestvalit: 0.310600 bestmemit:    0.978662
#> Iteration: 20 bestvalit: 0.310600 bestmemit:    0.978662
#> Iteration: 21 bestvalit: 0.310600 bestmemit:    0.978662
#> Iteration: 22 bestvalit: 0.310600 bestmemit:    0.980151
#> Iteration: 23 bestvalit: 0.310600 bestmemit:    0.980151
#> Iteration: 24 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 25 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 26 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 27 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 28 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 29 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 30 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 31 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 32 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 33 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 34 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 35 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 36 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 37 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 38 bestvalit: 0.310600 bestmemit:    0.984310
#> Iteration: 39 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 40 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 41 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 42 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 43 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 44 bestvalit: 0.310600 bestmemit:    0.985816
#> Iteration: 45 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 46 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 47 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 48 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 49 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 50 bestvalit: 0.310600 bestmemit:    0.985998
#> Iteration: 51 bestvalit: 0.310600 bestmemit:    0.989886
#> Iteration: 52 bestvalit: 0.310600 bestmemit:    0.930671
#> Iteration: 53 bestvalit: 0.310600 bestmemit:    0.930671
#> Iteration: 54 bestvalit: 0.310600 bestmemit:    0.930671
#> Iteration: 55 bestvalit: 0.310600 bestmemit:    0.985993
#> Iteration: 56 bestvalit: 0.310600 bestmemit:    0.980990
#> Iteration: 57 bestvalit: 0.310600 bestmemit:    0.980990
#> Iteration: 58 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 59 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 60 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 61 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 62 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 63 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 64 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 65 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 66 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 67 bestvalit: 0.310600 bestmemit:    0.990356
#> Iteration: 68 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 69 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 70 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 71 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 72 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 73 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 74 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 75 bestvalit: 0.310600 bestmemit:    0.982078
#> Iteration: 76 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 77 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 78 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 79 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 80 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 81 bestvalit: 0.310600 bestmemit:    0.990160
#> Iteration: 82 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 83 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 84 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 85 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 86 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 87 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 88 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 89 bestvalit: 0.310600 bestmemit:    0.988925
#> Iteration: 90 bestvalit: 0.310600 bestmemit:    0.988767
#> Iteration: 91 bestvalit: 0.310600 bestmemit:    0.997899
#> Iteration: 92 bestvalit: 0.310600 bestmemit:    0.997899
#> Iteration: 93 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 94 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 95 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 96 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 97 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 98 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 99 bestvalit: 0.310600 bestmemit:    0.978370
#> Iteration: 100 bestvalit: 0.310600 bestmemit:    0.978370
fit_result[[1]]$optim$bestmem
#>      par1 
#> 0.9783702
fit_result[[1]]$optim$bestval
#> [1] 0.3106
```

### 9. Available models

The package includes several types of models:

**Association-based models:** -
[`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md) -
Uncertainty and familiarity biased model -
[`uncfam_attention()`](https://kachergis.github.io/XSLmodels/reference/uncfam_attention.md) -
[`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md)
with learning rate scaled to trial uncertainty -
[`uncfam_predictive()`](https://kachergis.github.io/XSLmodels/reference/uncfam_predictive.md) -
[`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md)
with an added item-level prediction-error term -
[`fazly()`](https://kachergis.github.io/XSLmodels/reference/fazly.md) -
Fazly et al. model -
[`rescorla_wagner()`](https://kachergis.github.io/XSLmodels/reference/rescorla_wagner.md) -
Rescorla-Wagner model -
[`bayesian_decay()`](https://kachergis.github.io/XSLmodels/reference/bayesian_decay.md) -
Bayesian decay model -
[`kalman_filter()`](https://kachergis.github.io/XSLmodels/reference/kalman_filter.md) -
Kalman filter associative model (adaptive, uncertainty-scaled learning
rate)

**Sampling-based models:** -
[`uncfam_sampling()`](https://kachergis.github.io/XSLmodels/reference/uncfam_sampling.md) -
Sampling version of uncfam -
[`multi_sampling()`](https://kachergis.github.io/XSLmodels/reference/multi_sampling.md) -
Multi-hypothesis sampling -
[`guess_and_test()`](https://kachergis.github.io/XSLmodels/reference/guess_and_test.md) -
Guess and test model -
[`propose_but_verify()`](https://kachergis.github.io/XSLmodels/reference/propose_but_verify.md) -
Propose but verify model -
[`pursuit()`](https://kachergis.github.io/XSLmodels/reference/pursuit.md) -
Pursuit model

**Reinforcement learning models:** -
[`softmax_rl()`](https://kachergis.github.io/XSLmodels/reference/softmax_rl.md) -
Q-learning-style model with softmax action selection

**Baseline models:** -
[`baseline()`](https://kachergis.github.io/XSLmodels/reference/baseline.md) -
Simple co-occurrence baseline -
[`decay()`](https://kachergis.github.io/XSLmodels/reference/decay.md) -
Decay model -
[`tilles()`](https://kachergis.github.io/XSLmodels/reference/tilles.md) -
Tilles model

Each model can be run with different parameters and compared to human
data to understand which learning mechanisms best explain
cross-situational word learning behavior.
