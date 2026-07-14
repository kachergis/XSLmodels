# Get example ambiguous condition

This function creates an example condition with two trials, and two
word-object pairs per trial. Each word-object pair appears only once,
and thus each word has a 50% chance of being associated with the correct
referent. The function returns a list of the trials with nested words
and objects per trial, as well as a vector of the conditional
probability of correctly selecting the correct referent given each word.

## Usage

``` r
get_example_ambiguous_condition()
```

## Value

An object of class `xslData` containing a list of training trials (with
nested words and objects per trial) and accuracy (P(correct referent \|
word)).

## Examples

``` r
get_example_ambiguous_condition()
#> xslData object with label "example condition" and condition "ambiguous"
#>   training trials: 2
#>       test trials: 0
#>             words: 4
#>           objects: 4
#>        accuracies: 4
```
