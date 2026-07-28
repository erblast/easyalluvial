# plot colour intensity of palette

sum of red green and blue values

## Usage

``` r
palette_plot_intensity(palette)
```

## Arguments

- palette:

  any vector containing color hex values

## Value

ggplot2 plot

## See also

[`palette_plot_rgp`](https://erblast.github.io/easyalluvial/reference/palette_plot_rgp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
palette_qualitative() %>%
  palette_filter( thresh = 25) %>%
  palette_plot_intensity()
 }
} # }
```
