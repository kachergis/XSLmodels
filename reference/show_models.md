# Show available models in the package

Returns the names of all available cross-situational word learning
models, as accepted by
[`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)
and
[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md).
Most are the package's model functions; a few are variants of them used
in the model comparison paper (`uncfam_novelty`,
`uncfam_uncertainty_only`, `uncfam_strength`, `fazly_threshold`) or with
non-default options and a softmax choice rule
(`rescorla_wagner_softmax`, `rescorla_wagner_discriminative`,
`kalman_filter_full`).

## Usage

``` r
show_models()
```

## Value

A character vector of model names

## Examples

``` r
show_models()
#>  [1] "baseline"                       "decay"                         
#>  [3] "uncfam"                         "uncfam_novelty"                
#>  [5] "uncfam_uncertainty_only"        "uncfam_strength"               
#>  [7] "uncfam_gamma"                   "uncfam_general"                
#>  [9] "uncfam_elimination"             "uncfam_attention"              
#> [11] "uncfam_predictive"              "uncfam_sampling"               
#> [13] "multi_sampling"                 "propose_but_verify"            
#> [15] "pursuit"                        "fazly"                         
#> [17] "fazly_threshold"                "guess_and_test"                
#> [19] "rescorla_wagner"                "tilles"                        
#> [21] "bayesian_decay"                 "kalman_filter"                 
#> [23] "rescorla_wagner_softmax"        "rescorla_wagner_discriminative"
#> [25] "kalman_filter_full"             "softmax_rl"                    
#> [27] "fgt2009"                        "fgt2009_rsa"                   
#> [29] "minerva2"                       "todam"                         
#> [31] "rem"                           
```
