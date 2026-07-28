# color filters for any vector of hex color values

filters are based on rgb values

## Usage

``` r
palette_filter(
  palette = palette_qualitative(),
  similar = F,
  greys = T,
  reds = T,
  greens = T,
  blues = T,
  dark = T,
  medium = T,
  bright = T,
  thresh_similar = 25
)
```

## Arguments

- palette:

  any vector with hex color values, Default: palette_qualitative()

- similar, :

  logical, allow similar colours, similar colours are detected using a
  threshold (thresh_similar), two colours are similar when each value
  for RGB is within threshold range of the corresponding RGB value of
  the second colour, Default: F

- greys, :

  logical, allow grey colours, blue == green == blue , Default: T

- reds, :

  logical, allow red colours, blue \< 50 & green \< 50 & red \> 200 ,
  Default: T

- greens, :

  logical, allow green colours, green \> red & green \> blue, Default: T

- blues, :

  logical, allow blue colours, blue \> green & green \> red, Default: T

- dark, :

  logical, allow colours of dark intensity, sum( red, green, blue) \<
  420 , Default: T

- medium, :

  logical, allow colours of medium intensity, between( sum( red, green,
  blue), 420, 600) , Default: T

- bright, :

  logical, allow colours of bright intensity, sum( red, green, blue) \>
  600, Default: T

- thresh_similar, :

  int, threshold for defining similar colours, see similar, Default: 25

## Value

vector with hex colors

## Examples

``` r

require(magrittr)
#> Loading required package: magrittr

palette_qualitative() %>%
  palette_filter(thresh_similar = 0) %>%
  palette_plot_intensity()


if (FALSE) { # \dontrun{
# more examples---------------------------

palette_qualitative() %>%
  palette_filter(thresh_similar = 25) %>%
  palette_plot_intensity()

palette_qualitative() %>%
  palette_filter(thresh_similar = 0, blues = FALSE) %>%
  palette_plot_intensity()
} # }
```
