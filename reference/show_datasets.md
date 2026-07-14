# Show available datasets in the package

Returns information about the available cross-situational word learning
datasets included in the package.

## Usage

``` r
show_datasets()
```

## Value

A data frame with dataset information

## Examples

``` r
show_datasets()
#>    index                     label                   condition n_trials n_words
#> 1      1                       201                         3x4       36      18
#> 2      2                       202                    3x4 1/.5       36      18
#> 3      3                       203                   3x4 1/.66       36      18
#> 4      4                       204                     3x4 +6o       36      18
#> 5      5                       205                         2x4       54      18
#> 6      6                       206                   3x3 +1w/o       54      18
#> 7      7                       207                   4x4 +2w/o       54      18
#> 8      8                       211               1x3 6/18 4AFC      108      18
#> 9      9                       212                2x3 6/9 4AFC       54      18
#> 10    10                       213               2x4 6/12 4AFC       54      18
#> 11    11                       214                3x4 6/8 4AFC       36      18
#> 12    12                       215                         1x3      108      18
#> 13    13                       216                         2x3       54      18
#> 14    14                       217                    2x4 6/12       54      18
#> 15    15                       218                     3x4 6/8       36      18
#> 16    16                       219                     2x3 6/9       54      18
#> 17    17                       220                    3x4 9/12       54      18
#> 18    18                       221                         1x4      108      18
#> 19    19                       222                         1x3      108      18
#> 20    20                       223                         4x4       27      18
#> 21    21                       224                         3x3       36      18
#> 22    22                       225                         2x2       54      18
#> 23    23                       301                2x2 5x 24AFC       60      24
#> 24    24                 filt0E_3L                   filt0E_3L       18      12
#> 25    25                 filt3E_3L                   filt3E_3L       27      12
#> 26    26                 filt6E_3L                   filt6E_3L       36      12
#> 27    27                 filt9E_3L                   filt9E_3L       45      12
#> 28    28                 filt0E_6L                   filt0E_6L       36      12
#> 29    29                 filt3E_6L                   filt3E_6L       45      12
#> 30    30                 filt6E_6L                   filt6E_6L       54      12
#> 31    31                 filt9E_6L                   filt9E_6L       63      12
#> 32    32                 filt0E_9L                   filt0E_9L       54      12
#> 33    33                 filt3E_9L                   filt3E_9L       63      12
#> 34    34                 filt6E_9L                   filt6E_9L       72      12
#> 35    35                 filt9E_9L                   filt9E_9L       81      12
#> 36    36               2_x8_39_4x4                 2_x8_39_4x4       27      18
#> 37    37              3_x8_369_4x4                3_x8_369_4x4       27      18
#> 38    38           freq369-3x3loCD             freq369-3x3loCD       36      18
#> 39    39           freq369-3x3hiCD             freq369-3x3hiCD       36      18
#> 40    40              freq369_36mx                freq369_36mx       36      18
#> 41    41              freq369_39mx                freq369_39mx       36      18
#> 42    42                  orig_4x4                    orig_4x4       27      18
#> 43    43                  orig_3x3                    orig_3x3       36      18
#> 44    44              cont_div6-12                cont_div6-12       36      18
#> 45    45 1_max_temp_spat_cont_orig temporal+spatial contiguity       27      18
#> 46    46     4_no_spat_orig_max_tc    temporal contiguity only       27      18
#> 47    47          Suanda2014-lowCD                  2x2 low CD       16       8
#> 48    48          Suanda2014-medCD                  2x2 med CD       16       8
#> 49    49           Suanda2014-hiCD                 2x2 high CD       16       8
#> 50    50         Koehne2013-aaappp   AAAPPP (blocked, A-first)       48       8
#> 51    51         Koehne2013-apapap APAPAP (unblocked, A-first)       48       8
#> 52    52         Koehne2013-papapa PAPAPA (unblocked, P-first)       48       8
#> 53    53         Koehne2013-pppaaa   PPPAAA (blocked, P-first)       48       8
#>    n_objects n_subjects has_test
#> 1         18         25    FALSE
#> 2         18         25    FALSE
#> 3         18         25    FALSE
#> 4         24         20    FALSE
#> 5         18         33    FALSE
#> 6         18         39    FALSE
#> 7         18         39    FALSE
#> 8         18         43     TRUE
#> 9         18         38     TRUE
#> 10        18         31     TRUE
#> 11        18         36     TRUE
#> 12        18         23    FALSE
#> 13        18         23    FALSE
#> 14        18         14    FALSE
#> 15        18         13    FALSE
#> 16        18         32    FALSE
#> 17        18         33    FALSE
#> 18        18         40    FALSE
#> 19        18         40    FALSE
#> 20        18         77    FALSE
#> 21        18         36    FALSE
#> 22        18         19    FALSE
#> 23        24         46    FALSE
#> 24        12         31    FALSE
#> 25        12         30    FALSE
#> 26        12         30    FALSE
#> 27        12         31    FALSE
#> 28        12         27    FALSE
#> 29        12         27    FALSE
#> 30        12         27    FALSE
#> 31        12         27    FALSE
#> 32        12         31    FALSE
#> 33        12         31    FALSE
#> 34        12         31    FALSE
#> 35        12         31    FALSE
#> 36        18         30    FALSE
#> 37        18         74    FALSE
#> 38        18        102    FALSE
#> 39        18         26    FALSE
#> 40        18         62    FALSE
#> 41        18         66    FALSE
#> 42        18         88    FALSE
#> 43        18        104    FALSE
#> 44        18         40    FALSE
#> 45        18         30     TRUE
#> 46        18         32     TRUE
#> 47         8         84     TRUE
#> 48         8         84     TRUE
#> 49         8         84     TRUE
#> 50        17         64     TRUE
#> 51        17         64     TRUE
#> 52        17         64     TRUE
#> 53        17         64     TRUE
```
