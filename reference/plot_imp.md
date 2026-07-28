# plot feature importance

plot important features of model response alluvial as bars

## Usage

``` r
plot_imp(p, data_input, truncate_at = 50, color = "darkgrey")
```

## Arguments

- p:

  alluvial plot

- data_input:

  dataframe used to generate alluvial plot

- truncate_at:

  integer, limit number of features to that value, Default: 50

- color:

  character vector, Default: 'darkgrey'

## Value

ggplot object

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

p = alluvial_model_response_caret(train, degree = 3, pred_train = pred_train)

plot_imp(p, mtcars2)

} # }
```
