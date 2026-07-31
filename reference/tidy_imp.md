# tidy up dataframe containing model feature importance

returns dataframe with exactly two columns, vars and imp and aggregates
dummy encoded variables. Helper function called by all functions that
take an imp parameter. Can be called manually if formula for aggregating
dummy encoded variables must be modified.

## Usage

``` r
tidy_imp(imp, df, .f = max, resp_var = NULL)
```

## Arguments

- imp:

  dataframe or matrix with feature importance information

- df:

  dataframe, modeling training data

- .f:

  window function, Default: max

- resp_var:

  character, prediction variable, can usually be inferred from imp and
  df. It does not work for all models and needs to be specified in those
  cases.

## Value

dataframe

- vars:

  character column with feature names

- imp:

  numerical column, importance values

## Examples

``` r
# randomforest
df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
m = randomForest::randomForest( disp ~ ., df)
imp = m$importance
tidy_imp(imp, df)
#> # A tibble: 10 × 2
#>    vars      imp
#>    <chr>   <dbl>
#>  1 cyl   105250.
#>  2 mpg    81248.
#>  3 hp     73445.
#>  4 wt     70898.
#>  5 drat   47462.
#>  6 gear   23399.
#>  7 qsec   23065.
#>  8 vs     13312.
#>  9 carb    7973.
#> 10 am      2363.
```
