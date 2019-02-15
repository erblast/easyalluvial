
<!-- README.md is generated from README.Rmd. Please edit that file -->
easyalluvial <img src="https://www.datisticsblog.com/easyalluvial_logo_square.png" alt="logo" width="180" height="180" align = "right"/>
========================================================================================================================================

[![Travis CI Build Status](https://travis-ci.org/erblast/easyalluvial.svg?branch=master)](https://travis-ci.org/erblast/easyalluvial) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/erblast/easyalluvial?branch=master&svg=true)](https://ci.appveyor.com/project/erblast/easyalluvial) 
[![Coverage Status](https://img.shields.io/codecov/c/github/erblast/easyalluvial/master.svg)](https://codecov.io/github/erblast/easyalluvial?branch=master)
[![CRAN last release](https://www.r-pkg.org/badges/last-release/easyalluvial)](https://CRAN.R-project.org/package=easyalluvial)
[![CRAN total downloads](https://cranlogs.r-pkg.org/badges/grand-total/easyalluvial)](https://CRAN.R-project.org/package=easyalluvial)


Alluvial plots are similar to [sankey diagrams](https://en.wikipedia.org/wiki/Sankey_diagram) and visualise categorical data over multiple dimensions as flows. [Rosval et. al. 2010](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0008694) Their graphical grammar however is a bit more complex then that of a regular x/y plots. The [`ggalluvial`](http://corybrunson.github.io/ggalluvial/) package made a great job of translating that grammar into [`ggplot2`](https://github.com/tidyverse/ggplot2) syntax and gives you many option to tweak the appearance of an alluvial plot, however there still remains a multi-layered complexity that makes it difficult to use 'ggalluvial' for explorative data analysis. 'easyalluvial' provides a simple interface to this package that allows you to produce a decent alluvial plot from any dataframe in either long or wide format from a single line of code while also handling continuous data. It is meant to allow a quick visualisation of entire dataframes with a focus on different colouring options that can make alluvial plots a great tool for data exploration.

Features
--------

-   plot alluvial graph with a single line of code of a given dataframe
-   support for wide and long data format [(wiki, wide vs. long/narrow data)](https://en.wikipedia.org/wiki/Wide_and_narrow_data)
-   automatically transforms numerical to categorical data
-   helper functions for variable selection
-   convenient parameters for coloring and ordering

Installation
------------

### CRAN

``` r
install.packages('easyalluvial')
```

### Development Version

``` r

# install.packages("devtools")
devtools::install_github("erblast/easyalluvial")
```

Tutorials
---------

In order to learn about all the features an how they can be useful check out the following tutorials:

-   [Data exploration with alluvial plots](https://www.datisticsblog.com/2018/10/intro_easyalluvial/#features)

Examples
--------

``` r
suppressPackageStartupMessages( require(tidyverse) )
suppressPackageStartupMessages( require(easyalluvial) )
```

### Alluvial from data in wide format

#### Sample Data

``` r

## mtcars2 is included in the current development version

# mtcars2 <- within(mtcars, {
#   vs <- factor(vs, labels = c("V", "S"))
#   am <- factor(am, labels = c("automatic", "manual"))
#   cyl  <- ordered(cyl)
#   gear <- ordered(gear)
#   carb <- ordered(carb)
# })
# 
# mtcars2$id = row.names(mtcars)
# 
# mtcars2 = dplyr::as_tibble(mtcars2)

knitr::kable( head(mtcars2) )
```

|   mpg| cyl |  disp|   hp|  drat|     wt|   qsec| vs  | am        | gear | carb | id                |
|-----:|:----|-----:|----:|-----:|------:|------:|:----|:----------|:-----|:-----|:------------------|
|  21.0| 6   |   160|  110|  3.90|  2.620|  16.46| V   | manual    | 4    | 4    | Mazda RX4         |
|  21.0| 6   |   160|  110|  3.90|  2.875|  17.02| V   | manual    | 4    | 4    | Mazda RX4 Wag     |
|  22.8| 4   |   108|   93|  3.85|  2.320|  18.61| S   | manual    | 4    | 1    | Datsun 710        |
|  21.4| 6   |   258|  110|  3.08|  3.215|  19.44| S   | automatic | 3    | 1    | Hornet 4 Drive    |
|  18.7| 8   |   360|  175|  3.15|  3.440|  17.02| V   | automatic | 3    | 2    | Hornet Sportabout |
|  18.1| 6   |   225|  105|  2.76|  3.460|  20.22| S   | automatic | 3    | 1    | Valiant           |

#### Plot

Continuous Variables will be automatically binned as follows.

-   High, High (HH)
-   Medium, High (MH)
-   Medium (M)
-   Medium, Low (ML)
-   Low, Low (LL)

``` r

alluvial_wide( data = mtcars2
                , max_variables = 5
                , fill_by = 'first_variable' )
```

![](man/figures/README-wide_plot-1.png)

### Alluvial from data in long format

#### Sample Data

``` r
knitr::kable( head(quarterly_flights) )
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

alluvial_long( quarterly_flights
               , key = qu
               , value = mean_arr_delay
               , id = tailnum
               , fill = carrier )
```

![](man/figures/README-plot_long-1.png)
