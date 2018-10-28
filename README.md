
<!-- README.md is generated from README.Rmd. Please edit that file -->
easyalluvial
============

[![Travis CI Build Status](https://travis-ci.org/erblast/easyalluvial.svg?branch=master)](https://travis-ci.org/erblast/easyalluvial) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/erblast/easyalluvial?branch=master&svg=true)](https://ci.appveyor.com/project/erblast/easyalluvial) [![Coverage Status](https://img.shields.io/codecov/c/github/erblast/easyalluvial/master.svg)](https://codecov.io/github/erblast/easyalluvial?branch=master)

Alluvial plots are a form of sankey diagrams that are a great tool for exploring categorical data. Their graphical grammar however is a bit mor complex then that of a regular x-y plot. The ggalluvial package made a great job of translating that grammar into ggplot2 synthax and gives you many option to tweak the appearance of a plot, however the there still remains a multilayered complexity that makes it difficult to use ggalluvial for explorative data analysis. easyalluvial provides a simple interface to this package that allows you to put out a decent alluvial from any dataframe where data is stored in either long or wide format while also handling contineous data. It is meant to allow a quick visualisation of entire dataframes similar to the visualisations created by the tabplot package with a focus on different colouring options that can make alluvials a great tool for data exploration.

Installation
------------

You can install easyalluvial from github with:

``` r
# install.packages("devtools")
devtools::install_github("erblast/easyalluvial")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```
