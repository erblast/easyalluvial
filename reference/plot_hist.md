# plot histogram of alluvial plot variable

helper function used by add_marginal_histograms

## Usage

``` r
plot_hist(var, p, data_input, ...)
```

## Arguments

- var:

  character vector, variable name

- p:

  alluvial plot

- data_input:

  dataframe used to create alluvial plot

- ...:

  additional arguments for specific alluvial plot types: pred_train can
  be used to pass training predictions for model response alluvials

## Value

ggplot object
