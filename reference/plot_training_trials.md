# Create Animated Plot of Co-Occurrence Matrix from Training Trials

This function takes a sequence of training trials, each consisting of
words and objects, and creates an animated plot of the word-object
co-occurrence matrix. The plot evolves over each trial, visually
representing the accumulation of co-occurrences. The animation is saved
to a file if a filename is provided.

## Usage

``` r
plot_training_trials(train, filename = NULL)
```

## Arguments

- train:

  A list representing the training data, where each element is a trial
  containing lists of words (`words`) and objects (`objects`).

- filename:

  A string specifying the filename where the animation should be saved.
  If NULL, no file will be saved.

## Value

A long-format data frame where each row corresponds to a trial and
includes columns for trial number, word, object, and the count of
co-occurrences up to that trial.

## Examples

``` r
# \donttest{
plot_training_trials(xsl_datasets[[1]]$train)
#>   [1] "./gganim_plot0001.png" "./gganim_plot0002.png" "./gganim_plot0003.png"
#>   [4] "./gganim_plot0004.png" "./gganim_plot0005.png" "./gganim_plot0006.png"
#>   [7] "./gganim_plot0007.png" "./gganim_plot0008.png" "./gganim_plot0009.png"
#>  [10] "./gganim_plot0010.png" "./gganim_plot0011.png" "./gganim_plot0012.png"
#>  [13] "./gganim_plot0013.png" "./gganim_plot0014.png" "./gganim_plot0015.png"
#>  [16] "./gganim_plot0016.png" "./gganim_plot0017.png" "./gganim_plot0018.png"
#>  [19] "./gganim_plot0019.png" "./gganim_plot0020.png" "./gganim_plot0021.png"
#>  [22] "./gganim_plot0022.png" "./gganim_plot0023.png" "./gganim_plot0024.png"
#>  [25] "./gganim_plot0025.png" "./gganim_plot0026.png" "./gganim_plot0027.png"
#>  [28] "./gganim_plot0028.png" "./gganim_plot0029.png" "./gganim_plot0030.png"
#>  [31] "./gganim_plot0031.png" "./gganim_plot0032.png" "./gganim_plot0033.png"
#>  [34] "./gganim_plot0034.png" "./gganim_plot0035.png" "./gganim_plot0036.png"
#>  [37] "./gganim_plot0037.png" "./gganim_plot0038.png" "./gganim_plot0039.png"
#>  [40] "./gganim_plot0040.png" "./gganim_plot0041.png" "./gganim_plot0042.png"
#>  [43] "./gganim_plot0043.png" "./gganim_plot0044.png" "./gganim_plot0045.png"
#>  [46] "./gganim_plot0046.png" "./gganim_plot0047.png" "./gganim_plot0048.png"
#>  [49] "./gganim_plot0049.png" "./gganim_plot0050.png" "./gganim_plot0051.png"
#>  [52] "./gganim_plot0052.png" "./gganim_plot0053.png" "./gganim_plot0054.png"
#>  [55] "./gganim_plot0055.png" "./gganim_plot0056.png" "./gganim_plot0057.png"
#>  [58] "./gganim_plot0058.png" "./gganim_plot0059.png" "./gganim_plot0060.png"
#>  [61] "./gganim_plot0061.png" "./gganim_plot0062.png" "./gganim_plot0063.png"
#>  [64] "./gganim_plot0064.png" "./gganim_plot0065.png" "./gganim_plot0066.png"
#>  [67] "./gganim_plot0067.png" "./gganim_plot0068.png" "./gganim_plot0069.png"
#>  [70] "./gganim_plot0070.png" "./gganim_plot0071.png" "./gganim_plot0072.png"
#>  [73] "./gganim_plot0073.png" "./gganim_plot0074.png" "./gganim_plot0075.png"
#>  [76] "./gganim_plot0076.png" "./gganim_plot0077.png" "./gganim_plot0078.png"
#>  [79] "./gganim_plot0079.png" "./gganim_plot0080.png" "./gganim_plot0081.png"
#>  [82] "./gganim_plot0082.png" "./gganim_plot0083.png" "./gganim_plot0084.png"
#>  [85] "./gganim_plot0085.png" "./gganim_plot0086.png" "./gganim_plot0087.png"
#>  [88] "./gganim_plot0088.png" "./gganim_plot0089.png" "./gganim_plot0090.png"
#>  [91] "./gganim_plot0091.png" "./gganim_plot0092.png" "./gganim_plot0093.png"
#>  [94] "./gganim_plot0094.png" "./gganim_plot0095.png" "./gganim_plot0096.png"
#>  [97] "./gganim_plot0097.png" "./gganim_plot0098.png" "./gganim_plot0099.png"
#> [100] "./gganim_plot0100.png"
#> attr(,"frame_vars")
#>     frame nframes   progress transitioning previous_state closest_state
#> 1       1     101 0.00990099         FALSE              0             0
#> 2       2     101 0.01980198         FALSE              0             0
#> 3       3     101 0.02970297          TRUE              0             0
#> 4       4     101 0.03960396         FALSE              1             1
#> 5       5     101 0.04950495         FALSE              1             1
#> 6       6     101 0.05940594          TRUE              1             1
#> 7       7     101 0.06930693         FALSE              2             2
#> 8       8     101 0.07920792         FALSE              2             2
#> 9       9     101 0.08910891          TRUE              2             2
#> 10     10     101 0.09900990         FALSE              3             3
#> 11     11     101 0.10891089         FALSE              3             3
#> 12     12     101 0.11881188          TRUE              3             3
#> 13     13     101 0.12871287         FALSE              4             4
#> 14     14     101 0.13861386         FALSE              4             4
#> 15     15     101 0.14851485          TRUE              4             4
#> 16     16     101 0.15841584         FALSE              5             5
#> 17     17     101 0.16831683         FALSE              5             5
#> 18     18     101 0.17821782          TRUE              5             5
#> 19     19     101 0.18811881         FALSE              6             6
#> 20     20     101 0.19801980         FALSE              6             6
#> 21     21     101 0.20792079          TRUE              6             6
#> 22     22     101 0.21782178         FALSE              7             7
#> 23     23     101 0.22772277         FALSE              7             7
#> 24     24     101 0.23762376          TRUE              7             7
#> 25     25     101 0.24752475         FALSE              8             8
#> 26     26     101 0.25742574         FALSE              8             8
#> 27     27     101 0.26732673          TRUE              8             8
#> 28     28     101 0.27722772         FALSE              9             9
#> 29     29     101 0.28712871         FALSE              9             9
#> 30     30     101 0.29702970          TRUE              9             9
#> 31     31     101 0.30693069         FALSE             10            10
#> 32     32     101 0.31683168         FALSE             10            10
#> 33     33     101 0.32673267          TRUE             10            10
#> 34     34     101 0.33663366         FALSE             11            11
#> 35     35     101 0.34653465         FALSE             11            11
#> 36     36     101 0.35643564          TRUE             11            11
#> 37     37     101 0.36633663         FALSE             12            12
#> 38     38     101 0.37623762         FALSE             12            12
#> 39     39     101 0.38613861          TRUE             12            12
#> 40     40     101 0.39603960         FALSE             13            13
#> 41     41     101 0.40594059         FALSE             13            13
#> 42     42     101 0.41584158          TRUE             13            13
#> 43     43     101 0.42574257         FALSE             14            14
#> 44     44     101 0.43564356         FALSE             14            14
#> 45     45     101 0.44554455          TRUE             14            14
#> 46     46     101 0.45544554         FALSE             15            15
#> 47     47     101 0.46534653         FALSE             15            15
#> 48     48     101 0.47524752          TRUE             15            15
#> 49     49     101 0.48514851         FALSE             16            16
#> 50     50     101 0.49504950         FALSE             16            16
#> 52     52     101 0.51485149         FALSE             17            17
#> 53     53     101 0.52475248         FALSE             17            17
#> 54     54     101 0.53465347          TRUE             17            17
#> 55     55     101 0.54455446         FALSE             18            18
#> 56     56     101 0.55445545         FALSE             18            18
#> 57     57     101 0.56435644          TRUE             18            18
#> 58     58     101 0.57425743         FALSE             19            19
#> 59     59     101 0.58415842         FALSE             19            19
#> 60     60     101 0.59405941          TRUE             19            19
#> 61     61     101 0.60396040         FALSE             20            20
#> 62     62     101 0.61386139         FALSE             20            20
#> 63     63     101 0.62376238          TRUE             20            20
#> 64     64     101 0.63366337         FALSE             21            21
#> 65     65     101 0.64356436         FALSE             21            21
#> 66     66     101 0.65346535          TRUE             21            21
#> 67     67     101 0.66336634         FALSE             22            22
#> 68     68     101 0.67326733         FALSE             22            22
#> 69     69     101 0.68316832          TRUE             22            22
#> 70     70     101 0.69306931         FALSE             23            23
#> 71     71     101 0.70297030         FALSE             23            23
#> 72     72     101 0.71287129          TRUE             23            23
#> 73     73     101 0.72277228         FALSE             24            24
#> 74     74     101 0.73267327         FALSE             24            24
#> 75     75     101 0.74257426          TRUE             24            24
#> 76     76     101 0.75247525         FALSE             25            25
#> 77     77     101 0.76237624         FALSE             25            25
#> 78     78     101 0.77227723          TRUE             25            25
#> 79     79     101 0.78217822         FALSE             26            26
#> 80     80     101 0.79207921         FALSE             26            26
#> 81     81     101 0.80198020          TRUE             26            26
#> 82     82     101 0.81188119         FALSE             27            27
#> 83     83     101 0.82178218          TRUE             27            27
#> 84     84     101 0.83168317         FALSE             28            28
#> 85     85     101 0.84158416          TRUE             28            28
#> 86     86     101 0.85148515         FALSE             29            29
#> 87     87     101 0.86138614          TRUE             29            29
#> 88     88     101 0.87128713         FALSE             30            30
#> 89     89     101 0.88118812          TRUE             30            30
#> 90     90     101 0.89108911         FALSE             31            31
#> 91     91     101 0.90099010          TRUE             31            31
#> 92     92     101 0.91089109         FALSE             32            32
#> 93     93     101 0.92079208          TRUE             32            32
#> 94     94     101 0.93069307         FALSE             33            33
#> 95     95     101 0.94059406          TRUE             33            33
#> 96     96     101 0.95049505         FALSE             34            34
#> 97     97     101 0.96039604          TRUE             34            34
#> 98     98     101 0.97029703         FALSE             35            35
#> 99     99     101 0.98019802          TRUE             35            35
#> 100   100     101 0.99009901         FALSE             36            36
#> 101   101     101 1.00000000          TRUE             36            36
#>     next_state stringsAsFactors
#> 1            0            FALSE
#> 2            0            FALSE
#> 3            1            FALSE
#> 4            1            FALSE
#> 5            1            FALSE
#> 6            2            FALSE
#> 7            2            FALSE
#> 8            2            FALSE
#> 9            3            FALSE
#> 10           3            FALSE
#> 11           3            FALSE
#> 12           4            FALSE
#> 13           4            FALSE
#> 14           4            FALSE
#> 15           5            FALSE
#> 16           5            FALSE
#> 17           5            FALSE
#> 18           6            FALSE
#> 19           6            FALSE
#> 20           6            FALSE
#> 21           7            FALSE
#> 22           7            FALSE
#> 23           7            FALSE
#> 24           8            FALSE
#> 25           8            FALSE
#> 26           8            FALSE
#> 27           9            FALSE
#> 28           9            FALSE
#> 29           9            FALSE
#> 30          10            FALSE
#> 31          10            FALSE
#> 32          10            FALSE
#> 33          11            FALSE
#> 34          11            FALSE
#> 35          11            FALSE
#> 36          12            FALSE
#> 37          12            FALSE
#> 38          12            FALSE
#> 39          13            FALSE
#> 40          13            FALSE
#> 41          13            FALSE
#> 42          14            FALSE
#> 43          14            FALSE
#> 44          14            FALSE
#> 45          15            FALSE
#> 46          15            FALSE
#> 47          15            FALSE
#> 48          16            FALSE
#> 49          16            FALSE
#> 50          16            FALSE
#> 52          17            FALSE
#> 53          17            FALSE
#> 54          18            FALSE
#> 55          18            FALSE
#> 56          18            FALSE
#> 57          19            FALSE
#> 58          19            FALSE
#> 59          19            FALSE
#> 60          20            FALSE
#> 61          20            FALSE
#> 62          20            FALSE
#> 63          21            FALSE
#> 64          21            FALSE
#> 65          21            FALSE
#> 66          22            FALSE
#> 67          22            FALSE
#> 68          22            FALSE
#> 69          23            FALSE
#> 70          23            FALSE
#> 71          23            FALSE
#> 72          24            FALSE
#> 73          24            FALSE
#> 74          24            FALSE
#> 75          25            FALSE
#> 76          25            FALSE
#> 77          25            FALSE
#> 78          26            FALSE
#> 79          26            FALSE
#> 80          26            FALSE
#> 81          27            FALSE
#> 82          27            FALSE
#> 83          28            FALSE
#> 84          28            FALSE
#> 85          29            FALSE
#> 86          29            FALSE
#> 87          30            FALSE
#> 88          30            FALSE
#> 89          31            FALSE
#> 90          31            FALSE
#> 91          32            FALSE
#> 92          32            FALSE
#> 93          33            FALSE
#> 94          33            FALSE
#> 95          34            FALSE
#> 96          34            FALSE
#> 97          35            FALSE
#> 98          35            FALSE
#> 99          36            FALSE
#> 100         36            FALSE
#> 101          0            FALSE
#>                                         frame_source
#> 1   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0001.png
#> 2   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0002.png
#> 3   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0003.png
#> 4   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0004.png
#> 5   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0005.png
#> 6   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0006.png
#> 7   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0007.png
#> 8   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0008.png
#> 9   /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0009.png
#> 10  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0010.png
#> 11  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0011.png
#> 12  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0012.png
#> 13  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0013.png
#> 14  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0014.png
#> 15  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0015.png
#> 16  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0016.png
#> 17  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0017.png
#> 18  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0018.png
#> 19  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0019.png
#> 20  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0020.png
#> 21  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0021.png
#> 22  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0022.png
#> 23  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0023.png
#> 24  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0024.png
#> 25  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0025.png
#> 26  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0026.png
#> 27  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0027.png
#> 28  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0028.png
#> 29  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0029.png
#> 30  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0030.png
#> 31  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0031.png
#> 32  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0032.png
#> 33  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0033.png
#> 34  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0034.png
#> 35  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0035.png
#> 36  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0036.png
#> 37  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0037.png
#> 38  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0038.png
#> 39  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0039.png
#> 40  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0040.png
#> 41  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0041.png
#> 42  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0042.png
#> 43  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0043.png
#> 44  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0044.png
#> 45  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0045.png
#> 46  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0046.png
#> 47  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0047.png
#> 48  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0048.png
#> 49  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0049.png
#> 50  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0050.png
#> 52  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0051.png
#> 53  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0052.png
#> 54  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0053.png
#> 55  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0054.png
#> 56  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0055.png
#> 57  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0056.png
#> 58  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0057.png
#> 59  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0058.png
#> 60  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0059.png
#> 61  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0060.png
#> 62  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0061.png
#> 63  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0062.png
#> 64  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0063.png
#> 65  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0064.png
#> 66  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0065.png
#> 67  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0066.png
#> 68  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0067.png
#> 69  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0068.png
#> 70  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0069.png
#> 71  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0070.png
#> 72  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0071.png
#> 73  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0072.png
#> 74  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0073.png
#> 75  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0074.png
#> 76  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0075.png
#> 77  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0076.png
#> 78  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0077.png
#> 79  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0078.png
#> 80  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0079.png
#> 81  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0080.png
#> 82  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0081.png
#> 83  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0082.png
#> 84  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0083.png
#> 85  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0084.png
#> 86  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0085.png
#> 87  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0086.png
#> 88  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0087.png
#> 89  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0088.png
#> 90  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0089.png
#> 91  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0090.png
#> 92  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0091.png
#> 93  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0092.png
#> 94  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0093.png
#> 95  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0094.png
#> 96  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0095.png
#> 97  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0096.png
#> 98  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0097.png
#> 99  /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0098.png
#> 100 /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0099.png
#> 101 /tmp/RtmpLNGpjZ/1dd017612444/gganim_plot0100.png
#> # A tibble: 11,988 × 4
#>    word  object coocs trial
#>    <chr> <chr>  <dbl> <dbl>
#>  1 1     1          0     0
#>  2 1     2          0     0
#>  3 1     3          0     0
#>  4 1     4          0     0
#>  5 1     5          0     0
#>  6 1     6          0     0
#>  7 1     7          0     0
#>  8 1     8          0     0
#>  9 1     9          0     0
#> 10 1     10         0     0
#> # ℹ 11,978 more rows
# }
```
