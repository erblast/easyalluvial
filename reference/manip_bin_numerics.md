# bin numerical columns

centers, scales and Yeo Johnson transforms numeric variables in a
dataframe before binning into n bins of equal range. Outliers based on
boxplot stats are capped (set to min or max of boxplot stats).

## Usage

``` r
manip_bin_numerics(
  x,
  bins = 5,
  bin_labels = c("LL", "ML", "M", "MH", "HH"),
  center = T,
  scale = T,
  transform = T,
  round_numeric = T,
  digits = 2,
  NA_label = "NA"
)
```

## Arguments

- x:

  dataframe with numeric variables, or numeric vector

- bins:

  number of bins for numerical variables, passed to cut as breaks
  parameter, Default: 5

- bin_labels:

  labels for the bins from low to high, Default: c("LL", "ML", "M",
  "MH", "HH"). Can also be one of c('mean', 'median', 'min_max',
  'cuts'), the corresponding summary function will supply the labels.

- center:

  logical, Default: T

- scale:

  logical, Default: T

- transform:

  logical, apply Yeo Johnson Transformation, Default: T

- round_numeric, :

  logical, rounds numeric results if bin_labels is supplied with a
  supported summary function name.

- digits, :

  integer, number of digits to round to

- NA_label:

  character vector, define label for missing data, Default: 'NA'

## Value

dataframe

## Examples

``` r
summary( mtcars2 )
#>       mpg        cyl         disp             hp             drat      
#>  Min.   :10.40   4:11   Min.   : 71.1   Min.   : 52.0   Min.   :2.760  
#>  1st Qu.:15.43   6: 7   1st Qu.:120.8   1st Qu.: 96.5   1st Qu.:3.080  
#>  Median :19.20   8:14   Median :196.3   Median :123.0   Median :3.695  
#>  Mean   :20.09          Mean   :230.7   Mean   :146.7   Mean   :3.597  
#>  3rd Qu.:22.80          3rd Qu.:326.0   3rd Qu.:180.0   3rd Qu.:3.920  
#>  Max.   :33.90          Max.   :472.0   Max.   :335.0   Max.   :4.930  
#>        wt             qsec       vs             am     gear   carb  
#>  Min.   :1.513   Min.   :14.50   V:18   automatic:19   3:15   1: 7  
#>  1st Qu.:2.581   1st Qu.:16.89   S:14   manual   :13   4:12   2:10  
#>  Median :3.325   Median :17.71                         5: 5   3: 3  
#>  Mean   :3.217   Mean   :17.85                                4:10  
#>  3rd Qu.:3.610   3rd Qu.:18.90                                6: 1  
#>  Max.   :5.424   Max.   :22.90                                8: 1  
#>         ids    
#>  Length   :32  
#>  N.unique :32  
#>  N.blank  : 0  
#>  Min.nchar: 7  
#>  Max.nchar:19  
#>                
summary( manip_bin_numerics(mtcars2) )
#>  mpg    cyl    disp     hp     drat     wt     qsec    vs             am    
#>  LL:3   4:11   LL: 9   LL: 5   LL: 9   LL: 5   LL: 4   V:18   automatic:19  
#>  ML:8   6: 7   ML: 7   ML:10   ML: 4   ML: 6   ML: 2   S:14   manual   :13  
#>  M :9   8:14   M : 2   M : 4   M :12   M :13   M :10                        
#>  MH:7          MH:10   MH: 9   MH: 6   MH: 5   MH: 7                        
#>  HH:5          HH: 4   HH: 4   HH: 1   HH: 3   HH: 9                        
#>                                                                             
#>  gear   carb          ids    
#>  3:15   1: 7   Length   :32  
#>  4:12   2:10   N.unique :32  
#>  5: 5   3: 3   N.blank  : 0  
#>         4:10   Min.nchar: 7  
#>         6: 1   Max.nchar:19  
#>         8: 1                 
summary( manip_bin_numerics(mtcars2, bin_labels = 'mean'))
#>     mpg    cyl        disp         hp       drat       wt        qsec    vs    
#>  11.37:3   4:11   96.56 : 9   62.2  : 5   2.98: 9   1.81: 5   15   : 4   V:18  
#>  15.26:8   6: 7   155.39: 7   103.3 :10   3.18: 4   2.53: 6   16.15: 2   S:14  
#>  19.11:9   8:14   241.5 : 2   136.5 : 4   3.79:12   3.34:13   17.13:10         
#>  22.9 :7          317.14:10   190.56: 9   4.19: 6   3.85: 5   18.26: 7         
#>  30.88:5          443   : 4   272.25: 4   4.93: 1   5.34: 3   19.97: 9         
#>                                                                                
#>          am     gear   carb          ids    
#>  automatic:19   3:15   1: 7   Length   :32  
#>  manual   :13   4:12   2:10   N.unique :32  
#>                 5: 5   3: 3   N.blank  : 0  
#>                        4:10   Min.nchar: 7  
#>                        6: 1   Max.nchar:19  
#>                        8: 1                 
summary( manip_bin_numerics(mtcars2, bin_labels = 'cuts'
  , scale = FALSE, center = FALSE, transform = FALSE))
#>           mpg     cyl            disp              hp              drat   
#>  (10.4,15.1]: 6   4:11   (70.7,151]:12   (51.8,94.4]: 7   (2.76,3.19]:11  
#>  (15.1,19.8]:12   6: 7   (151,231] : 5   (94.4,137] :10   (3.19,3.63]: 4  
#>  (19.8,24.5]: 8   8:14   (231,312] : 6   (137,179]  : 5   (3.63,4.06]:10  
#>  (24.5,29.2]: 2          (312,392] : 5   (179,222]  : 5   (4.06,4.5] : 6  
#>  (29.2,33.9]: 4          (392,472] : 4   (222,264]  : 5   (4.5,4.93] : 1  
#>                                                                           
#>            wt              qsec    vs             am     gear   carb  
#>  (1.51,2.26]: 6   (14.5,15.6]: 4   V:18   automatic:19   3:15   1: 7  
#>  (2.26,3.01]: 6   (15.6,16.8]: 3   S:14   manual   :13   4:12   2:10  
#>  (3.01,3.76]:13   (16.8,17.9]:10                         5: 5   3: 3  
#>  (3.76,4.5] : 4   (17.9,19.1]: 8                                4:10  
#>  (4.5,5.25] : 3   (19.1,20.2]: 7                                6: 1  
#>                                                                 8: 1  
#>         ids    
#>  Length   :32  
#>  N.unique :32  
#>  N.blank  : 0  
#>  Min.nchar: 7  
#>  Max.nchar:19  
#>                
```
