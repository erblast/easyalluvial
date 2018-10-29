
<!-- README.md is generated from README.Rmd. Please edit that file -->
easyalluvial
============

[![Travis CI Build Status](https://travis-ci.org/erblast/easyalluvial.svg?branch=master)](https://travis-ci.org/erblast/easyalluvial) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/erblast/easyalluvial?branch=master&svg=true)](https://ci.appveyor.com/project/erblast/easyalluvial) [![Coverage Status](https://img.shields.io/codecov/c/github/erblast/easyalluvial/master.svg)](https://codecov.io/github/erblast/easyalluvial?branch=master)

Alluvial plots are a form of [sankey diagrams](https://en.wikipedia.org/wiki/Sankey_diagram) that are a great tool for exploring categorical data. They group categorical data into flows that can easily be traced in the diagram. Other than sankey diagrams they are constrained to x-y dimensions, however their graphical grammar however is a bit mor complex then that of a regular x-y plot. The [`ggalluvial`](http://corybrunson.github.io/ggalluvial/) package made a great job of translating that grammar into ggplot2 synthax and gives you many option to tweak the appearance of a plot, nevertheless there still remains a multilayered complexity that makes it difficult to use `ggalluvial` for explorative data analysis. `easyalluvial` provides a simple interface to this package that allows you to put out a decent alluvial from any dataframe where data is stored in either long or wide format while automatically binning continuous data. It is meant to allow a quick visualisation of entire dataframes similar to the visualisations created by the [`tabplot`](https://github.com/mtennekes/tabplot) package providing different colouring options which give it the flexibility needed for data exploration.

Installation
------------

You can install easyalluvial from github with:

``` r
# install.packages("devtools")
devtools::install_github("erblast/easyalluvial")
```

### Alluvial from data in long format

#### Prepare sample data

``` r

require(tidyverse)
#> Loading required package: tidyverse
#> -- Attaching packages ------------------------------------------------------------------------ tidyverse 1.2.1 --
#> v ggplot2 3.1.0     v purrr   0.2.5
#> v tibble  1.4.2     v dplyr   0.7.7
#> v tidyr   0.8.2     v stringr 1.3.1
#> v readr   1.1.1     v forcats 0.3.0
#> -- Conflicts --------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

monthly_flights = nycflights13::flights %>%
  group_by(month, tailnum, origin, dest, carrier) %>%
  summarise() %>%
  group_by( tailnum, origin, dest, carrier) %>%
  count() %>%
  filter( n == 12 ) %>%
  select( - n ) %>%
  left_join( nycflights13::flights ) %>%
  .[complete.cases(.), ] %>%
  ungroup() %>%
  mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
          , qu = cut(month, 4)) %>%
  group_by(tailnum, carrier, origin, dest, qu ) %>%
  summarise( mean_arr_delay = mean(arr_delay) ) %>%
  ungroup() %>%
  mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )
#> Joining, by = c("tailnum", "origin", "dest", "carrier")
  
  levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')
  
knitr::kable( head(monthly_flights) )
```

| tailnum           | carrier | origin | dest | qu  | mean\_arr\_delay |
|:------------------|:--------|:-------|:-----|:----|:-----------------|
| N0EGMQ LGA BNA MQ | MQ      | LGA    | BNA  | Q1  | on\_time         |
| N0EGMQ LGA BNA MQ | MQ      | LGA    | BNA  | Q2  | on\_time         |
| N0EGMQ LGA BNA MQ | MQ      | LGA    | BNA  | Q3  | on\_time         |
| N0EGMQ LGA BNA MQ | MQ      | LGA    | BNA  | Q4  | on\_time         |
| N11150 EWR MCI EV | EV      | EWR    | MCI  | Q1  | late             |
| N11150 EWR MCI EV | EV      | EWR    | MCI  | Q2  | late             |

#### Plot

``` r

require(easyalluvial)
#> Loading required package: easyalluvial

alluvial_long( monthly_flights
               , key = qu
               , value = mean_arr_delay
               , id = tailnum
               , fill = carrier )
```

![](README-plot_long-1.png)

### Alluvial from data in wide format

#### Prepare sample data

``` r

data = as_tibble(mtcars)
categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')

data = data %>%
  mutate_at( vars(categoricals), as.factor )
```

#### Plot

``` r

alluvial_wide( data = data
                , max_variables = 5
                , fill_by = 'first_variable' )
```

![](README-wide_plot-1.png)
