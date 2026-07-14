# Get example unambiguous condition

This function creates an example condition with three trials and two
word-object pairs per trial. Each word-object pair appears twice, and so
if the most frequently co-occurring referent for each word is selected
then all three words would be learned.

## Usage

``` r
get_example_unambiguous_condition()
```

## Value

An object of class `xslData` containing a list of training trials (with
nested words and objects per trial) and accuracy (P(correct referent \|
word)).

## Examples

``` r
get_example_unambiguous_condition()
#> xslData object with label "example condition" and condition "unambiguous"
#>   training trials: 3
#>       test trials: 0
#>             words: 3
#>           objects: 3
#>        accuracies: 3
```
