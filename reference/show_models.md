# Show available models in the package

Returns a list of all available cross-situational word learning models
that can be used with the package.

## Usage

``` r
show_models()
```

## Value

A character vector of model names

## Examples

``` r
show_models()
#>  [1] "baseline"           "decay"              "uncfam"            
#>  [4] "uncfam_sampling"    "multi_sampling"     "propose_but_verify"
#>  [7] "pursuit"            "fazly"              "guess_and_test"    
#> [10] "rescorla_wagner"    "tilles"             "bayesian_decay"    
```
