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
(i.e. objects). Words and objects can be given any labels – numbers or
strings – and a word’s correct referent is the object with the same
label (i.e. $`w_i`$ is meant to be learned as $`o_i`$). Thus, each
dataset defines a list `train` of the `words` and `objects` presented on
each trial, also defined in lists (to allow for a potentially varying
number of words/objects per trial). The `test` trials are defined by a
list of the tested word and a list of the available objects on each test
trial. A dataset can also be given a short `label`, and a longer (maybe
per-item?) `condition`.

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

[`get_example_ambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_ambiguous_condition.md)
and
[`get_example_unambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_unambiguous_condition.md)
return simple example experimental conditions, which consist of the
training trials (`train`), an optional vector `accuracy` containing
people’s per-word performance on the test trials, and an optional list
of test trials (`test`), enumerating each to-be-tested word and the set
of referents presented for choosing among. `train` is in turn a list of
the words presented on each trial, and a list of the referents
(`objects`) presented on each trial.

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

Shown above, the example ambiguous condition has two training trials
(`ag$train`). On the first trial words 1 and 2 appear
(`ag$train$words[[1]]`) alongside referents 2 and 1
(`ag$train$objects[[1]]`; the order of words/referents within a trial is
irrelevant).

### 2. Viewing an experiment’s structure

One common way to summarize the structure of a cross-situational word
learning experiment is to make a matrix of the word-object
co-occurrences, which can be visualized with a heatmap to show the
strength of association between each word and object.
[`create_cooc_matrix()`](https://www.kachergis.com/XSLmodels/reference/create_cooc_matrix.md)
will return a word x object matrix showing the tallied co-occurrences of
each word with each object across all of the training trials.

``` r

create_cooc_matrix(ag$train)
#>   1 2 3 4
#> 1 1 1 0 0
#> 2 1 1 0 0
#> 3 0 0 1 1
#> 4 0 0 1 1
```

### 3. Included experimental conditions

A dataset combining 56 experimental conditions is included in the
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

Several more datasets ship separately from `xsl_datasets`, so that they
are never seen by
[`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)
/
[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md).
Most are standalone because they don’t fit the “one correct object per
tested word, scored by SSE against human accuracy” convention:

- `kachergis2012_highlighting`: some test items have two legitimate
  targets, so its accuracy is partly `NA`.
- `gangwani2011_category`: a hierarchical design in which every object
  has both a 1-to-1 name and a 1-to-many category label (a 15 word x 12
  object matrix; learners violate mutual exclusivity – see
  [`?gangwani2011_category`](https://www.kachergis.com/XSLmodels/reference/gangwani2011_category.md)).
- `vlach_debrock2017`: only an overall mean accuracy is reported, not
  per word.
- `rollins_corpus` and `fm_corpus`: two naturalistic caregiver-speech
  corpora imported from the [wurwur](https://github.com/mcfrank/wurwur)
  package, with no human referent-selection data. Each bundles the
  training `xslData` in `$data` with a gold-standard lexicon in `$gold`,
  scored with
  [`get_fscore()`](https://www.kachergis.com/XSLmodels/reference/get_fscore.md)
  /
  [`get_roc()`](https://www.kachergis.com/XSLmodels/reference/get_roc.md)
  rather than SSE (see
  [`vignette("corpora")`](https://www.kachergis.com/XSLmodels/articles/corpora.md)).

Two others, `kachergis_initial_accuracy` and `benitez2020`, fit the
convention fine but are held out on purpose, as data for testing how
well a model fit to `xsl_datasets` generalizes (section 11).
`xsl_holdout_datasets` lists every standalone dataset and how to score a
model against it:

``` r

str(xsl_holdout_datasets$benitez2020)
#> List of 3
#>  $ scorable: logi TRUE
#>  $ scoring : chr "xsl_run() + mafc_test() / get_perf() (standard diagonal scoring)"
#>  $ note    : chr "Real per-word accuracy; structurally could join xsl_datasets but kept out deliberately as generalization-test data."
names(xsl_holdout_datasets)[sapply(xsl_holdout_datasets, `[[`, "scorable")]
#> [1] "benitez2020"                "kachergis_initial_accuracy"
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
#> [1] 0.3908695
```

### 5. Run a model with given parameters through multiple datasets and pull SSE.

``` r

run3 <- xsl_run(uncfam(X = .1, C = 1, B = .98), data = xsl_datasets[1:3])
run3$sse
#> [1] 0.2822977
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
#> [1] 3.194255

pt <- xsl_run(pursuit(gamma = .2, threshold = .3, lambda = .05), data = xsl_datasets[1:3])
pt$sse
#> [1] 8.752733

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
#>  [4] "uncfam_gamma"       "uncfam_elimination" "uncfam_attention"  
#>  [7] "uncfam_predictive"  "uncfam_sampling"    "multi_sampling"    
#> [10] "propose_but_verify" "pursuit"            "fazly"             
#> [13] "guess_and_test"     "rescorla_wagner"    "tilles"            
#> [16] "bayesian_decay"     "kalman_filter"      "softmax_rl"        
#> [19] "fgt2009"            "fgt2009_rsa"        "minerva2"          
#> [22] "todam"              "rem"

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
#> Iteration: 1 bestvalit: 0.310600 bestmemit:    0.922301
#> Iteration: 2 bestvalit: 0.310600 bestmemit:    0.928167
#> Iteration: 3 bestvalit: 0.310600 bestmemit:    0.928167
#> Iteration: 4 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 5 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 6 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 7 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 8 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 9 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 10 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 11 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 12 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 13 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 14 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 15 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 16 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 17 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 18 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 19 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 20 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 21 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 22 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 23 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 24 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 25 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 26 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 27 bestvalit: 0.310600 bestmemit:    0.937170
#> Iteration: 28 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 29 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 30 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 31 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 32 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 33 bestvalit: 0.310600 bestmemit:    0.979786
#> Iteration: 34 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 35 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 36 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 37 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 38 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 39 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 40 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 41 bestvalit: 0.310600 bestmemit:    0.947051
#> Iteration: 42 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 43 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 44 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 45 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 46 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 47 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 48 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 49 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 50 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 51 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 52 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 53 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 54 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 55 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 56 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 57 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 58 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 59 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 60 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 61 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 62 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 63 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 64 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 65 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 66 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 67 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 68 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 69 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 70 bestvalit: 0.310600 bestmemit:    0.982466
#> Iteration: 71 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 72 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 73 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 74 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 75 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 76 bestvalit: 0.310600 bestmemit:    0.990610
#> Iteration: 77 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 78 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 79 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 80 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 81 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 82 bestvalit: 0.310600 bestmemit:    0.986762
#> Iteration: 83 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 84 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 85 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 86 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 87 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 88 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 89 bestvalit: 0.310600 bestmemit:    0.991180
#> Iteration: 90 bestvalit: 0.310600 bestmemit:    0.984388
#> Iteration: 91 bestvalit: 0.310600 bestmemit:    0.987745
#> Iteration: 92 bestvalit: 0.310600 bestmemit:    0.987745
#> Iteration: 93 bestvalit: 0.310600 bestmemit:    0.987745
#> Iteration: 94 bestvalit: 0.310600 bestmemit:    0.984830
#> Iteration: 95 bestvalit: 0.310600 bestmemit:    0.984830
#> Iteration: 96 bestvalit: 0.310600 bestmemit:    0.979256
#> Iteration: 97 bestvalit: 0.310600 bestmemit:    0.979256
#> Iteration: 98 bestvalit: 0.310600 bestmemit:    0.979256
#> Iteration: 99 bestvalit: 0.310600 bestmemit:    0.979256
#> Iteration: 100 bestvalit: 0.310600 bestmemit:    0.979256
fit_result[[1]]$optim$bestmem
#>      par1 
#> 0.9792563
fit_result[[1]]$optim$bestval
#> [1] 0.3106
```

### 9. Available models

The package includes several types of models:

**Association-based models:** -
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md) -
Uncertainty and familiarity biased model -
[`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md) -
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
with learning rate scaled to trial uncertainty -
[`uncfam_predictive()`](https://www.kachergis.com/XSLmodels/reference/uncfam_predictive.md) -
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
with an added item-level prediction-error term -
[`uncfam_gamma()`](https://www.kachergis.com/XSLmodels/reference/uncfam_gamma.md) -
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
with a free exponent on familiarity’s contribution to attention -
[`uncfam_elimination()`](https://www.kachergis.com/XSLmodels/reference/uncfam_elimination.md) -
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
with learning by elimination (mutual exclusivity) -
[`fazly()`](https://www.kachergis.com/XSLmodels/reference/fazly.md) -
Fazly et al. model -
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md) -
Rescorla-Wagner model -
[`bayesian_decay()`](https://www.kachergis.com/XSLmodels/reference/bayesian_decay.md) -
Bayesian decay model -
[`kalman_filter()`](https://www.kachergis.com/XSLmodels/reference/kalman_filter.md) -
Kalman filter associative model (adaptive, uncertainty-scaled learning
rate)

**Sampling-based models:** -
[`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md) -
Sampling version of uncfam -
[`multi_sampling()`](https://www.kachergis.com/XSLmodels/reference/multi_sampling.md) -
Multi-hypothesis sampling -
[`guess_and_test()`](https://www.kachergis.com/XSLmodels/reference/guess_and_test.md) -
Guess and test model -
[`propose_but_verify()`](https://www.kachergis.com/XSLmodels/reference/propose_but_verify.md) -
Propose but verify model -
[`pursuit()`](https://www.kachergis.com/XSLmodels/reference/pursuit.md) -
Pursuit model

**Reinforcement learning models:** -
[`softmax_rl()`](https://www.kachergis.com/XSLmodels/reference/softmax_rl.md) -
Q-learning-style model with softmax action selection

**Bayesian models:** -
[`fgt2009()`](https://www.kachergis.com/XSLmodels/reference/fgt2009.md) -
Frank, Goodman & Tenenbaum (2009) intentional model: joint posterior
inference over a word-object lexicon, marginalizing the speaker’s
referential intention (a batch model; ported from the `wordlearn`
package). Tune the lexicon-size prior with
[`fgt2009_sweep_alpha()`](https://www.kachergis.com/XSLmodels/reference/fgt2009_sweep_alpha.md). -
[`fgt2009_rsa()`](https://www.kachergis.com/XSLmodels/reference/fgt2009_rsa.md) -
[`fgt2009()`](https://www.kachergis.com/XSLmodels/reference/fgt2009.md)
with a Rational Speech Act pragmatic speaker layer

**Episodic / distributed memory models:** -
[`minerva2()`](https://www.kachergis.com/XSLmodels/reference/minerva2.md) -
Hintzman’s MINERVA2: stores each trial as a trace of random feature
vectors, and retrieves by similarity -
[`todam()`](https://www.kachergis.com/XSLmodels/reference/todam.md) -
Murdock’s TODAM: a single composite memory vector of convolved
word-object associations -
[`rem()`](https://www.kachergis.com/XSLmodels/reference/rem.md) -
Shiffrin & Steyvers’ REM, adapted to this package’s associative task
(slow on large corpora)

**Baseline models:** -
[`baseline()`](https://www.kachergis.com/XSLmodels/reference/baseline.md) -
Simple co-occurrence baseline -
[`decay()`](https://www.kachergis.com/XSLmodels/reference/decay.md) -
Decay model -
[`tilles()`](https://www.kachergis.com/XSLmodels/reference/tilles.md) -
Tilles model

Each model can be run with different parameters and compared to human
data to understand which learning mechanisms best explain
cross-situational word learning behavior.

### 10. Prior learning: training trials or a start matrix

Some experiments teach part of the vocabulary before the
cross-situational phase – e.g. `kachergis_initial_accuracy`, which first
shows each word with one object. There are two ways to give a model that
prior learning.

The general one is to include it as ordinary training trials, as
`kachergis_initial_accuracy` does (its first 18 trials each show one
word with one object). Every model learns from these with its own update
rule.

Alternatively, a model can start from a given association matrix via
`xslControl(start_matrix = ...)`. Only models with a single
word-by-object association matrix support this:
[`baseline()`](https://www.kachergis.com/XSLmodels/reference/baseline.md),
[`decay()`](https://www.kachergis.com/XSLmodels/reference/decay.md),
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md),
[`softmax_rl()`](https://www.kachergis.com/XSLmodels/reference/softmax_rl.md),
the
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
family (including
[`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md)
and
[`multi_sampling()`](https://www.kachergis.com/XSLmodels/reference/multi_sampling.md)),
[`propose_but_verify()`](https://www.kachergis.com/XSLmodels/reference/propose_but_verify.md),
and
[`pursuit()`](https://www.kachergis.com/XSLmodels/reference/pursuit.md).
Any other model
(e.g. [`fazly()`](https://www.kachergis.com/XSLmodels/reference/fazly.md),
[`kalman_filter()`](https://www.kachergis.com/XSLmodels/reference/kalman_filter.md),
or the episodic-memory models) stops with an error rather than silently
ignoring it. A start matrix with dimnames is matched to the data’s
words/objects by label.

``` r

d <- get_example_ambiguous_condition()
prior <- matrix(0, 4, 4, dimnames = list(1:4, 1:4))
prior["1", "1"] <- .5   # word 1 already partly learned
mod <- uncfam(X = .1, B = .98, C = 1)
xsl_run(mod, d)$fits[[1]]$perf
#>         1         2         3         4 
#> 0.3888889 0.3888889 0.3888889 0.3888889
xsl_run(mod, d, control = xslControl(start_matrix = prior))$fits[[1]]$perf
#>         1         2         3         4 
#> 0.9451155 0.3593462 0.3888889 0.3888889
```

Note that the two encodings aren’t interchangeable: what a start-matrix
value “means” differs by model (a count, a probability, a Q-value), and
prior-learning trials interact with mechanisms such as
[`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md)’s
running average of trial uncertainty.

### 11. Testing generalization on held-out datasets

Parameters fit to `xsl_datasets` can be checked against data the fit
never saw. `xsl_holdout_datasets` says which held-out datasets can be
scored the standard way; here, a model fit to a few `xsl_datasets`
conditions is scored on each `benitez2020` and
`kachergis_initial_accuracy` condition, next to the no-free-parameter
[`baseline()`](https://www.kachergis.com/XSLmodels/reference/baseline.md):

``` r

fit <- xsl_fit(decay(C = .98), data = xsl_datasets[1:5], lower = .8, upper = 1,
               deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 10, trace = FALSE))
fitted_decay <- update_params(decay(C = .98), fit[[1]]$optim$bestmem)

holdout <- c(benitez2020, kachergis_initial_accuracy)
data.frame(
  condition = names(holdout),
  decay_sse = sapply(holdout, \(d) xsl_run(fitted_decay, d)$sse),
  baseline_sse = sapply(holdout, \(d) xsl_run(baseline(), d)$sse),
  row.names = NULL
)
#>                          condition decay_sse baseline_sse
#> 1                Interleaved, kids 0.1560054    0.1758034
#> 2              Interleaved, adults 0.1556145    0.1666667
#> 3                     Massed, kids 0.1374258    0.1390123
#> 4                   Massed, adults 0.1134279    0.1133333
#> 5     Unstructured (order 1), kids 0.4451216    0.4506173
#> 6   Unstructured (order 2/3), kids 0.4965327    0.4843750
#> 7 Unstructured (order 2/3), adults 1.0515461    1.0733333
#> 8            High Initial Accuracy 0.2919295    0.1846444
#> 9             Low Initial Accuracy 0.1920520    0.1146664
```

A model that fits `xsl_datasets` well but does worse than
[`baseline()`](https://www.kachergis.com/XSLmodels/reference/baseline.md)
on held-out data is likely overfitting the particular conditions it was
fit to.
