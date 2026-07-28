# increases length of palette by repeating colours

works for any vector

## Usage

``` r
palette_increase_length(palette = palette_qualitative(), n = 100)
```

## Arguments

- palette:

  any vector, Default: palette_qualitative()

- n, :

  int, length, Default: 100

## Value

vector with increased length

## Examples

``` r

require(magrittr)

length(palette_qualitative())
#> [1] 77

palette_qualitative() %>%
  palette_increase_length(100) %>%
  length()
#> [1] 100
```
