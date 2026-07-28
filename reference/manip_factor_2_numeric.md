# converts factor to numeric preserving numeric levels and order in character levels.

before converting we check whether the levels contain a number, if they
do the number will be preserved.

## Usage

``` r
manip_factor_2_numeric(vec)
```

## Arguments

- vec:

  vector

## Value

vector

## See also

[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html)

## Examples

``` r
fac_num = factor( c(1,3,8) )
fac_chr = factor( c('foo','bar') )
fac_chr_ordered = factor( c('a','b','c'), ordered = TRUE )

manip_factor_2_numeric( fac_num )
#> [1] 1 3 8
manip_factor_2_numeric( fac_chr )
#> [1] 2 1
manip_factor_2_numeric( fac_chr_ordered )
#> [1] 1 2 3
# does not work for decimal numbers
manip_factor_2_numeric(factor(c("A12", "B55", "10e4")))
#> [1] 2 3 1
manip_factor_2_numeric(factor(c("1.56", "4.56", "8.4")))
#> [1] 1 2 3
```
