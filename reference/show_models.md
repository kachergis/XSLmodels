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
#>  [4] "uncfam_attention"   "uncfam_predictive"  "uncfam_sampling"   
#>  [7] "multi_sampling"     "propose_but_verify" "pursuit"           
#> [10] "fazly"              "guess_and_test"     "rescorla_wagner"   
#> [13] "tilles"             "bayesian_decay"     "kalman_filter"     
#> [16] "softmax_rl"         "fgt2009"            "fgt2009_rsa"       
```
