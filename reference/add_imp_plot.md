# add bar plot of important features to model response alluvial plot

adds bar plot of important features to model response alluvial plot

## Usage

``` r
add_imp_plot(grid, p = NULL, data_input, plot = T, ...)
```

## Arguments

- grid:

  gtable or ggplot

- p:

  alluvial plot, optional if alluvial plot has already been passed as
  grid. Default: NULL

- data_input:

  dataframe used to generate alluvial plot

- plot:

  logical if plot should be drawn or not

- ...:

  additional parameters passed to
  [`plot_imp`](https://erblast.github.io/easyalluvial/reference/plot_imp.md)

## Value

gtable

## See also

[`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
[`plot_imp`](https://erblast.github.io/easyalluvial/reference/plot_imp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df = mtcars2[, ! names(mtcars2) %in% 'ids' ]

train = caret::train( disp ~ .
                     , df
                     , method = 'rf'
                     , trControl = caret::trainControl( method = 'none' )
                     , importance = TRUE )

pred_train = caret::predict.train(train, df)

p = alluvial_model_response_caret(train, degree = 4, pred_train = pred_train)

p_grid = add_marginal_histograms(p, data_input = df)

p_grid = add_imp_plot(p_grid, p, data_input = df)
} # }
```
