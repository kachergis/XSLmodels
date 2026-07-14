# Split Data into Training and Testing Sets Based on Indices

This function divides a given set of conditions into training and
testing sets. The division is based on a provided vector of indices,
which specify the conditions to be used for testing.

## Usage

``` r
get_train_test_split(test_inds, conds)
```

## Arguments

- test_inds:

  A numeric vector of indices indicating which elements in `conds`
  should be used for the test set.

- conds:

  A list of conditions (e.g. `xslData` objects), where each condition is
  represented as a separate element.

## Value

A list with two elements: `train` and `test`. Each element is a list of
conditions, where `train` includes the conditions not indexed by
`test_inds`, and `test` includes the conditions indexed by `test_inds`.

## Examples

``` r
split <- get_train_test_split(c(1, 3), xsl_datasets[1:5])
length(split$train)
#> [1] 3
length(split$test)
#> [1] 2
```
