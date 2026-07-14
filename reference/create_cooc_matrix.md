# Creates co-occurrence matrix from training trials

Given a training order (list of words and objects per trial), this
function returns a matrix tallying word-object co-occurrences across the
trials. This matrix can be used to analyze the frequency with which
certain words and objects appear together during the training phase.

## Usage

``` r
create_cooc_matrix(train)
```

## Arguments

- train:

  A list representing the training data, where each element is a trial
  that includes both words and objects. The structure is expected to
  have sub-elements `words` and `objects` for each trial.

## Value

A matrix where each element represents the count of co-occurrences
between a word (rows) and an object (columns). The row and column names
correspond to the unique words and objects found in the training data,
respectively.

## Examples

``` r
create_cooc_matrix(xsl_datasets[[1]]$train)
#>    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
#> 1  3 1 1 1 1 0 0 1 0  1  1  1  1  0  0  0  0  0
#> 2  0 3 1 1 1 0 1 0 0  0  1  1  0  0  0  1  1  1
#> 3  2 2 3 0 0 0 0 0 0  1  0  1  0  0  1  0  0  2
#> 4  0 1 1 4 1 1 0 0 1  0  1  0  1  2  0  1  2  0
#> 5  1 0 0 0 3 1 0 2 2  1  0  1  0  1  0  0  0  0
#> 6  0 0 1 1 2 3 0 0 2  2  0  0  1  0  0  0  0  0
#> 7  0 0 0 1 0 0 3 1 0  0  1  2  0  0  1  0  2  1
#> 8  0 0 0 0 1 1 1 3 1  0  0  1  0  2  2  0  0  0
#> 9  0 0 1 1 2 2 0 1 3  1  0  0  0  1  0  0  0  0
#> 10 1 1 1 0 1 1 0 0 1  3  0  1  2  0  0  0  0  0
#> 11 1 1 0 1 0 0 2 0 0  0  3  1  1  0  0  1  1  0
#> 12 1 0 0 0 1 0 2 2 1  1  1  4  1  0  1  0  1  0
#> 13 1 0 0 1 1 1 0 0 1  2  1  1  3  0  0  0  0  0
#> 14 0 0 0 2 0 1 0 1 0  0  1  0  1  3  1  1  1  0
#> 15 1 0 1 0 0 1 0 1 0  0  0  0  0  1  2  0  0  1
#> 16 0 1 0 1 0 0 1 0 0  0  1  0  1  1  0  2  0  0
#> 17 0 1 0 3 1 0 1 0 0  0  1  0  0  1  0  0  3  1
#> 18 1 1 2 1 0 0 1 0 0  0  0  1  0  0  1  0  1  3
```
