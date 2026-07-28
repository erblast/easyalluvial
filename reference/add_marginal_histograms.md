# add marginal histograms to alluvial plot

will add density histograms and frequency plots of original data to
alluvial plot

## Usage

``` r
add_marginal_histograms(
  p,
  data_input,
  top = TRUE,
  keep_labels = FALSE,
  plot = TRUE,
  ...
)
```

## Arguments

- p:

  alluvial plot

- data_input:

  dataframe, input data that was used to create dataframe

- top:

  logical, position of histograms, if FALSE adds them at the bottom,
  Default: TRUE

- keep_labels:

  logical, keep title and caption, Default: FALSE

- plot:

  logical if plot should be drawn or not

- ...:

  additional arguments for model response alluvial plot concerning the
  response variable

  pred_train

  :   display training prediction, not necessary if pred_train has
      already been passed to alluvial_model_response()

  scale

  :   int, y-axis distance between the ridge plots, Default: 400

  resp_var

  :   character vector, specify response variable in data_input, if not
      set response variable will try to be inferred, Default: NULL

## Value

gtable

## See also

[`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)

## Examples

``` r
if (FALSE) { # \dontrun{
p = alluvial_wide(mtcars2, max_variables = 3)
p_grid = add_marginal_histograms(p, mtcars2)
} # }
```
