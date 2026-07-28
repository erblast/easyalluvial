# Changelog

## Version 0.4.1 20260728

- adapted to vip 0.5.0 move from CRAN to r-universe
- adapt to ggplot2 3.4.0 linewidth changes by introducing
  `flow_border_size` parameter in
  [`alluvial_wide()`](https://erblast.github.io/easyalluvial/reference/alluvial_wide.md)
  and
  [`alluvial_long()`](https://erblast.github.io/easyalluvial/reference/alluvial_long.md)

## Version 0.4.0 20250825

CRAN release: 2025-09-03

- adapted to changes in ggplot2 v4.0.0

## Version 0.3.2 submitted to CRAN 20231207

CRAN release: 2023-12-07

- check for vip pkg installation before executing example of
  alluvial_model_response_parsnip
- option for using geom_text instead of geom_label (by
  <https://github.com/gernophil>)

## Version 0.3.1 submitted to CRAN 20220704

CRAN release: 2022-07-05

- cleanup package dependencies

## Version 0.3.0 submitted to CRAN 20200111

CRAN release: 2021-01-13

- moved modeling packages and vdiffr from imports to suggests
- added parsnip and workflows wrapper

Bug Fixes: - alluvial_wide() now works when all columns are character
columns

## Version 0.2.3 submitted to CRAN

CRAN release: 2020-05-07

Bug Fixes: - compatibility with `dplyr 1.0.0`

New features: - `pkgdown` website

## Version 0.2.1 submitted to CRAN 20190917

CRAN release: 2019-09-17

Bug Fixes: - compatibility with `tidyr 1.0.0` - compatibility with
`ggalluvial 0.10.0` - alluvial_model_response() had issues with factors
<https://github.com/erblast/easyalluvial/issues/13> which is now fixed

## Version 0.2.0 submitted to CRAN 20190331

CRAN release: 2019-04-01

Minor changes: - Default plotting colours have been improved - for
alluvial_wide() stratum fill color of variable determining flow is now
matched with flow color - `parameter_label_size` allows to adjust the
label fint size, as a result label size has been slightly increased

New features: - marginal histograms - alluvial flavoured partial
dependency and model response alluvial plots - new datasets mtcars2,
quarterly_sunspots

Bug Fixes: - Compatibility with `recipes 0.1.5` - NA values in numerical
columns are excepted - correct ggplot2 and ggalluvial versions added as
dependencies - p\$data_key does not contain columns with empty levels
anymore - better support for numerical variables for alluvial_long()
