# plot marginal histograms of alluvial plot

will create gtable with density histograms and frequency plots of all
variables of a given alluvial plot.

## Usage

``` r
plot_all_hists(p, data_input, top = TRUE, keep_labels = FALSE, ...)
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

- ...:

  additional arguments for specific alluvial plot types: pred_train can
  be used to pass training predictions for model response alluvials

## Value

gtable

## See also

[`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)

[`add_marginal_histograms`](https://erblast.github.io/easyalluvial/reference/add_marginal_histograms.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p = alluvial_wide(mtcars2, max_variables = 3)
plot_all_hists(p, mtcars2)
} # }
```
