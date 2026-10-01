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
#>  [4] "uncfam_gamma"       "uncfam_attention"   "uncfam_predictive" 
#>  [7] "uncfam_sampling"    "multi_sampling"     "propose_but_verify"
#> [10] "pursuit"            "fazly"              "guess_and_test"    
#> [13] "rescorla_wagner"    "tilles"             "bayesian_decay"    
#> [16] "kalman_filter"      "softmax_rl"         "fgt2009"           
#> [19] "fgt2009_rsa"        "minerva2"           "todam"             
#> [22] "rem"               
```
