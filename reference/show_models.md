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
#>  [4] "uncfam_general"     "uncfam_gamma"       "uncfam_elimination"
#>  [7] "uncfam_attention"   "uncfam_predictive"  "uncfam_sampling"   
#> [10] "multi_sampling"     "propose_but_verify" "pursuit"           
#> [13] "fazly"              "guess_and_test"     "rescorla_wagner"   
#> [16] "tilles"             "bayesian_decay"     "kalman_filter"     
#> [19] "softmax_rl"         "fgt2009"            "fgt2009_rsa"       
#> [22] "minerva2"           "todam"              "rem"               
```
