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
#>  [4] "uncfam_gamma"       "uncfam_elimination" "uncfam_attention"  
#>  [7] "uncfam_predictive"  "uncfam_sampling"    "multi_sampling"    
#> [10] "propose_but_verify" "pursuit"            "fazly"             
#> [13] "guess_and_test"     "rescorla_wagner"    "tilles"            
#> [16] "bayesian_decay"     "kalman_filter"      "softmax_rl"        
#> [19] "fgt2009"            "fgt2009_rsa"        "minerva2"          
#> [22] "todam"              "rem"               
```
