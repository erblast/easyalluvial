# Package index

## Main Plot Functions

- [`alluvial_long()`](https://erblast.github.io/easyalluvial/reference/alluvial_long.md)
  : alluvial plot of data in long format
- [`alluvial_model_response()`](https://erblast.github.io/easyalluvial/reference/alluvial_model_response.md)
  : create model response plot
- [`alluvial_model_response_caret()`](https://erblast.github.io/easyalluvial/reference/alluvial_model_response_caret.md)
  : create model response plot for caret models
- [`alluvial_model_response_parsnip()`](https://erblast.github.io/easyalluvial/reference/alluvial_model_response_parsnip.md)
  : create model response plot for parsnip models
- [`alluvial_wide()`](https://erblast.github.io/easyalluvial/reference/alluvial_wide.md)
  : alluvial plot of data in wide format

## Add Plot Elements

- [`add_imp_plot()`](https://erblast.github.io/easyalluvial/reference/add_imp_plot.md)
  : add bar plot of important features to model response alluvial plot
- [`add_marginal_histograms()`](https://erblast.github.io/easyalluvial/reference/add_marginal_histograms.md)
  : add marginal histograms to alluvial plot

## Generate Data for Model Response and Partial Dependence Plots

- [`get_data_space()`](https://erblast.github.io/easyalluvial/reference/get_data_space.md)
  : calculate data space
- [`get_pdp_predictions()`](https://erblast.github.io/easyalluvial/reference/get_pdp_predictions.md)
  : get predictions compatible with the partial dependence plotting
  method
- [`get_pdp_predictions_seq()`](https://erblast.github.io/easyalluvial/reference/get_pdp_predictions_seq.md)
  : get predictions compatible with the partial dependence plotting
  method, sequential variant that only works for numeric predictions.

## Helper Functions - Data Manipulation

- [`manip_bin_numerics()`](https://erblast.github.io/easyalluvial/reference/manip_bin_numerics.md)
  : bin numerical columns
- [`manip_factor_2_numeric()`](https://erblast.github.io/easyalluvial/reference/manip_factor_2_numeric.md)
  : converts factor to numeric preserving numeric levels and order in
  character levels.
- [`manip_get_ggplot_data()`](https://erblast.github.io/easyalluvial/reference/manip_get_ggplot_data.md)
  : Get ggplot data
- [`tidy_imp()`](https://erblast.github.io/easyalluvial/reference/tidy_imp.md)
  : tidy up dataframe containing model feature importance
- [`check_pkg_installed()`](https://erblast.github.io/easyalluvial/reference/check_pkg_installed.md)
  : check if package is installed

## Helper Functions - Plotting

- [`plot_all_hists()`](https://erblast.github.io/easyalluvial/reference/plot_all_hists.md)
  : plot marginal histograms of alluvial plot
- [`plot_condensation()`](https://erblast.github.io/easyalluvial/reference/plot_condensation.md)
  : Plot dataframe condensation potential
- [`plot_hist()`](https://erblast.github.io/easyalluvial/reference/plot_hist.md)
  : plot histogram of alluvial plot variable
- [`plot_imp()`](https://erblast.github.io/easyalluvial/reference/plot_imp.md)
  : plot feature importance

## Helper Functions - Color Palette

- [`palette_filter()`](https://erblast.github.io/easyalluvial/reference/palette_filter.md)
  : color filters for any vector of hex color values
- [`palette_increase_length()`](https://erblast.github.io/easyalluvial/reference/palette_increase_length.md)
  : increases length of palette by repeating colours
- [`palette_plot_intensity()`](https://erblast.github.io/easyalluvial/reference/palette_plot_intensity.md)
  : plot colour intensity of palette
- [`palette_plot_rgp()`](https://erblast.github.io/easyalluvial/reference/palette_plot_rgp.md)
  : plot rgb values of palette
- [`palette_qualitative()`](https://erblast.github.io/easyalluvial/reference/palette_qualitative.md)
  : compose palette from qualitative RColorBrewer palettes

## Data Sets

- [`mtcars2`](https://erblast.github.io/easyalluvial/reference/mtcars2.md)
  : mtcars dataset with cyl, vs, am ,gear, carb as factor variables and
  car model names as id
- [`titanic`](https://erblast.github.io/easyalluvial/reference/titanic.md)
  : titanic data set'
- [`quarterly_flights`](https://erblast.github.io/easyalluvial/reference/quarterly_flights.md)
  : Quarterly mean arrival delay times for a set of 402 flights
- [`quarterly_sunspots`](https://erblast.github.io/easyalluvial/reference/quarterly_sunspots.md)
  : Quarterly mean relative sunspots number from 1749-1983
