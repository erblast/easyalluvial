# alluvial plot of data in long format

Plots two variables of a dataframe on an alluvial plot. A third variable
can be added either to the left or the right of the alluvial plot to
provide coloring of the flows. All numerical variables are scaled,
centered and YeoJohnson transformed before binning.

## Usage

``` r
alluvial_long(
  data,
  key,
  value,
  id,
  fill = NULL,
  fill_right = T,
  bins = 5,
  bin_labels = c("LL", "ML", "M", "MH", "HH"),
  NA_label = "NA",
  order_levels_value = NULL,
  order_levels_key = NULL,
  order_levels_fill = NULL,
  complete = TRUE,
  fill_by = "first_variable",
  col_vector_flow = palette_qualitative() %>% palette_filter(greys = F),
  col_vector_value = RColorBrewer::brewer.pal(9, "Greys")[c(3, 6, 4, 7, 5)],
  verbose = F,
  stratum_labels = T,
  stratum_label_type = "label",
  stratum_label_size = 4.5,
  stratum_width = 1/4,
  flow_border_size = 0.3,
  auto_rotate_xlabs = T,
  ...
)
```

## Arguments

- data:

  a dataframe

- key:

  unquoted column name or string of x axis variable

- value:

  unquoted column name or string of y axis variable

- id:

  unquoted column name or string of id column

- fill:

  unquoted column name or string of fill variable which will be used to
  color flows, Default: NULL

- fill_right:

  logical, TRUE fill variable is added to the right FALSE to the left,
  Default: T

- bins:

  number of bins for automatic binning of numerical variables, Default:
  5

- bin_labels:

  labels for bins, Default: c("LL", "ML", "M", "MH", "HH")

- NA_label:

  character vector define label for missing data

- order_levels_value:

  character vector denoting order of y levels from low to high, does not
  have to be complete can also just be used to bring levels to the
  front, Default: NULL

- order_levels_key:

  character vector denoting order of x levels from low to high, does not
  have to be complete can also just be used to bring levels to the
  front, Default: NULL

- order_levels_fill:

  character vector denoting order of color fill variable levels from low
  to high, does not have to be complete can also just be used to bring
  levels to the front, Default: NULL

- complete:

  logical, insert implicitly missing observations, Default: TRUE

- fill_by:

  one_of(c('first_variable', 'last_variable', 'all_flows', 'values')),
  Default: 'first_variable'

- col_vector_flow:

  HEX color values for flows, Default: palette_filter( greys = F)

- col_vector_value:

  HEX color values for y levels/values,
  Default:RColorBrewer::brewer.pal(9, 'Greys')\[c(3,6,4,7,5)\]

- verbose:

  logical, print plot summary, Default: F

- stratum_labels:

  logical, Default: TRUE

- stratum_label_type:

  character, Default: "label"

- stratum_label_size:

  numeric, Default: 4.5

- stratum_width:

  double, Default: 1/4

- flow_border_size:

  numeric, line width of the flow polygon borders passed as `linewidth`
  to
  [`geom_flow`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md).
  Set to a value greater than 0 to draw visible outlines around the
  flows, Default: 0.1

- auto_rotate_xlabs:

  logical, Default: TRUE

- ...:

  additional parameter passed to
  [`manip_bin_numerics`](https://erblast.github.io/easyalluvial/reference/manip_bin_numerics.md)

## Value

ggplot2 object

## See also

[`alluvial_wide`](https://erblast.github.io/easyalluvial/reference/alluvial_wide.md)
,[`geom_flow`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md),
[`geom_stratum`](http://corybrunson.github.io/ggalluvial/reference/geom_stratum.md)
,[`manip_bin_numerics`](https://erblast.github.io/easyalluvial/reference/manip_bin_numerics.md)

## Examples

``` r

if (FALSE) { # \dontrun{
 data = quarterly_flights

 alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )

 # more flow coloring variants ------------------------------------

 alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
 alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
 alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )

 # color by additional variable carrier ---------------------------

 alluvial_long( data, key = qu, value = mean_arr_delay, fill = carrier, id = tailnum )

 # use same color coding for flows and y levels -------------------

 palette = c('green3', 'tomato')

 alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'value'
                , col_vector_flow = palette
                , col_vector_value = palette )


 # reorder levels ------------------------------------------------

 alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
               , order_levels_value = c('on_time', 'late') )

 alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
               , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )

require(dplyr)
require(magrittr)

 order_by_carrier_size = data %>%
   group_by(carrier) %>%
   count() %>%
   arrange( desc(n) ) %>%
   .[['carrier']]

 alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
                , order_levels_fill = order_by_carrier_size )

} # }
```
