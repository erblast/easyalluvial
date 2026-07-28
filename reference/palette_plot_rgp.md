# plot rgb values of palette

grouped bar chart

## Usage

``` r
palette_plot_rgp(palette)
```

## Arguments

- palette:

  any vector containing color hex values

## Value

ggplot2 plot

## See also

[`palette_plot_intensity`](https://erblast.github.io/easyalluvial/reference/palette_plot_intensity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
palette_qualitative() %>%
  palette_filter( thresh = 50) %>%
  palette_plot_rgp()
 }
} # }
```
