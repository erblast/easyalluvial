
#'@title alluvial plot of data in long format
#'@description Plots two variables of a dataframe on an alluvial plot. A third
#'  variable can be added either to the left or the right of the alluvial plot
#'  to provide coloring of the flows. All numerical variables are scaled,
#'  centered and YeoJohnson transformed before binning.
#'@param data a dataframe
#'@param key unqoted column name or string of x axis variable
#'@param value unqoted column name or string of y axis variable
#'@param id unqoted column name or string of id column
#'@param fill unqoted column name or string of fill variable which will be used to
#'  color flows, Default: NULL
#'@param fill_right logical, TRUE fill variable is added to the right FALSE to
#'  the left, Default: T
#'@param bins number of bins for automatic binning of numerical variables,
#'  Default: 5
#'@param bin_labels labels for bins, Default: c("LL", "ML", "M", "MH", "HH")
#'@param order_levels_value character vector denoting order of y levels from low
#'  to high, does not have to be complete can also just be used to bring levels
#'  to the front, Default: NULL
#'@param order_levels_key character vector denoting order of x levels from low
#'  to high, does not have to be complete can also just be used to bring levels
#'  to the front, Default: NULL
#'@param order_levels_fill character vector denoting order of color fill
#'  variable levels from low to high, does not have to be complete can also just
#'  be used to bring levels to the front, Default: NULL
#'@param complete logical, insert implicitly missing observations, Default: TRUE
#'@param NA_label character vector define label for missing data
#'@param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'  'values')), Default: 'first_variable'
#'@param col_vector_flow HEX color values for flows, Default: palette_filter( greys = F)
#'@param col_vector_value HEX color values  for y levels/values,
#'  Default:RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
#'@param verbose logical, print plot summary, Default: F
#'@param stratum_labels logical, Default: TRUE
#'@param stratum_width double, Default: 1/4
#'@param auto_rotate_xlabs logical, Default: TRUE
#'@return ggplot2 object
#'@seealso \code{\link[easyalluvial]{alluvial_wide}}
#'  ,\code{\link[ggalluvial]{geom_flow}}, \code{\link[ggalluvial]{geom_stratum}}
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  # sample data-------------------------------------------------
#'
#'  require(magrittr)
#'  require(dplyr)
#'  require(tidyr)
#'  require(purrr)
#'
#'  monthly_flights = nycflights13::flights %>%
#'  group_by(month, tailnum, origin, dest, carrier) %>%
#'  summarise() %>%
#'  group_by( tailnum, origin, dest, carrier) %>%
#'  count() %>%
#'  filter( n == 12 ) %>%
#'  select( - n ) %>%
#'  left_join( nycflights13::flights ) %>%
#'  .[complete.cases(.), ] %>%
#'  ungroup() %>%
#'  mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
#'          , qu = cut(month, 4)) %>%
#'  group_by(tailnum, carrier, origin, dest, qu ) %>%
#'  summarise( mean_arr_delay = mean(arr_delay) ) %>%
#'  ungroup() %>%
#'  mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )
#'
#'  levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')
#'
#'  data = monthly_flights
#'
#'
#'  # flow coloring variants -----------------------------------------
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )
#'
#'  # use same color coding for flows and y levels -------------------
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'value'
#'  , col_vector_flow = palette_qualitative() %>% palette_filter(greys = F, bright = F)
#'  , col_vector_value = palette_qualitative() %>% palette_filter(greys = F, bright = F) )
#'
#'  # move fill variable to the left ---------------------------------
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, carrier ,fill_right = F )
#'
#'  # reorder levels ------------------------------------------------
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
#'                , order_levels_value = c('on_time', 'late') )
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
#'                , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )
#'
#'  order_by_carrier_size = data %>%
#'    group_by(carrier) %>%
#'    count() %>%
#'    arrange( desc(n) ) %>%
#'    .[['carrier']]
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
#'                 , order_levels_fill = order_by_carrier_size )
#'
#'  }
#' }
#'@rdname alluvial_long
#'@export
#'@importFrom RColorBrewer brewer.pal
#'@importFrom forcats fct_relevel fct_rev
#'@importFrom rlang UQ quo_is_null
#'@import ggalluvial
#'@import dplyr
#'@import purrr
#'@import tidyr
#'@import ggplot2
alluvial_long = function( data
                          , key
                          , value
                          , id
                          , fill = NULL
                          , fill_right = T
                          , bins = 5
                          , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                          , NA_label = 'NA'
                          , order_levels_value = NULL
                          , order_levels_key = NULL
                          , order_levels_fill = NULL
                          , complete = TRUE
                          , fill_by = 'first_variable'
                          , col_vector_flow = palette_qualitative() %>% palette_filter( greys = F)
                          , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
                          , verbose = F
                          , stratum_labels = T
                          , stratum_width = 1/4
                          , auto_rotate_xlabs = T
){

  # quosures

  key = enquo( key )
  value = enquo( value )
  id = enquo( id )
  fill = enquo( fill )

  key_str = quo_name(key)
  value_str = quo_name(value)
  id_str = quo_name(id)

  # in order to make the function parameters compatible with strings we convert the strings
  # extracted from the quosures to symbols and override the quosures

  key = as.name( key_str )
  value = as.name( value_str )
  id = as.name( id_str )

  # fill

  if( rlang::quo_is_null(fill) ){
    fill_str = NULL
  }else{
    fill_str = quo_name(fill)
    fill = as.name( fill_str )
  }

  # transform numerical variables for binning

  data_trans = data %>%
    ungroup() %>%
    select( one_of( c(key_str, value_str, id_str, fill_str) ) ) %>%
    mutate( !! key_str := as.factor( !! key )
            , !! id_str := as.factor( !! id )
            ) %>%
    manip_bin_numerics( bins, bin_labels) %>%
    mutate( !! value_str := as.factor( !! value ) )

  #complete data

  if( is.null(fill_str) ){

    data_trans = data_trans %>%
      complete( !! key , !! id )

  }else{

    id_2_fill_keys = data_trans %>%
      group_by( !! id, !! fill) %>%
      summarise()

    suppressMessages({
      data_trans = data_trans %>%
        complete( !! key , !! id ) %>% ## leaves NA values for fill
        select( - !! fill ) %>%     ## deselect and rejoin fill
        left_join( id_2_fill_keys )
    })

  }

  # preserve order of categorical variables

  ordered_levels_x = c( order_levels_key, levels( select(data_trans, !! key)[[1]] ) ) %>% unique()
  ordered_levels_y = c( order_levels_value, levels( select(data_trans, !! value)[[1]] ) ) %>% unique()

  if( ! is.null(fill_str) ){
    ordered_levels_fill = c( order_levels_fill, levels( select(data_trans, !! fill)[[1]] ) ) %>% unique()
    ordered_levels_y = c( ordered_levels_y, ordered_levels_fill)

    if(fill_right){
      ordered_levels_x = c( ordered_levels_x, fill_str )
    }else{
      ordered_levels_x = c( fill_str, ordered_levels_x )
    }

  }else{
    ordered_levels_fill = NULL
  }

  # convert NA values in value to NA_label

  data_trans = data_trans %>%
    mutate(   !! value_str := as.character( !! value )
              ,  !! value_str := ifelse( is.na( !!  value ), NA_label, !! value )
              ,  !! value_str := as.factor( !! value) ) ##factor label will be restored further down


  suppressWarnings({

    # add alluvial ids
    data_spread = data_trans %>%
      spread( key = !! key, value = !! value )

    data_alluvial_id = data_spread %>%
      select( - !! id ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      mutate( alluvial_id = row_number() )

    data_new = data_alluvial_id %>%
      gather( key = 'x', value = 'value'
              , - one_of(c('alluvial_id','n', fill_str))  ) %>%
      mutate( x = as.factor(x)
              , x = forcats::fct_relevel(x, ordered_levels_x))
  })
  # compose fill columns

  last_x = levels(data_new$x) %>%
    .[ length(.) ]

  first_x = levels(data_new$x)[1]

  if( ! is.null(fill_str) ){

    data_fill = data_new %>%
      filter( x == last_x ) %>%
      mutate( value = !! fill
              , x = fill_str )

    suppressWarnings({

        data_new = data_new %>%
        bind_rows( data_fill )
    })

    data_new$fill = select( data_new, !! fill)[[1]]

  }else if( fill_by %in% c( 'first_variable', 'last_variable') ) {

    data_fill = data_new

    if( fill_by == 'first_variable') data_fill = data_fill %>%
        filter( x == first_x )


    if(fill_by == 'last_variable') data_fill = data_fill %>%
        filter( x == last_x )

    data_fill = data_fill %>%
      select( alluvial_id, value ) %>%
      rename( fill = value )

    suppressMessages({
      data_new = data_new %>%
        left_join( data_fill )
    })

  } else if( fill_by == 'all_flows'){

    data_new$fill = data_new$alluvial_id %>%
      as.factor(.)

  }else if( fill_by == 'value'){

    data_new$fill = data_new$value

  }else{
    warning( 'no valid fill option selected')

    data_new$fill = 'a'

  }

  # reformat factors

  data_new = data_new %>%
    mutate_if( is.character, as.factor ) %>%
    mutate( x =  forcats::fct_relevel( x, ordered_levels_x )
            , value =  forcats::fct_relevel( value, ordered_levels_y )
            , fill =  forcats::fct_relevel( fill , ordered_levels_fill )
            ) %>%
    mutate( value =  forcats::fct_rev(value) )

  n_flows    = max( manip_factor_2_numeric( data_new$alluvial_id) )
  n_per_x    = nrow(data)/length( unique( select(data, !! key) ) )
  reduced_to = round( n_flows/ n_per_x * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/n_per_x * 100, 1 )

  line1 = paste('Number of flows:', n_flows)
  line2 = paste('Original Dataframe reduced to', reduced_to, '%' )
  line3 = paste('Maximum weight of a singfle flow', max_weight_perc, '%')

  if( verbose ){
    print( line1 )
    print( line2 )
    print( line3 )
  }

  caption = paste( line1, line2, line3, sep = '\n' )

  if(n_flows >= 1500){

    print( line1 )
    print( line2 )
    print( line3 )

    warning( paste( n_flows, ' flows are a lot and the plot will take a long time to render') )
  }

  #adjust col_vector length fill flows

  n_colors_needed = length( unique(data_new$fill) )

  col_vector_flow = palette_increase_length( col_vector_flow, n_colors_needed  )

  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )

  suppressMessages({
    data_new = data_new %>%
      left_join( df_fill_flow )
  })

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = palette_increase_length( col_vector_value, n_colors_needed  )

  d_fill_value = tibble( value = unique(data_new$value)
                         , fill_value = col_vector_value )

  suppressMessages({
    data_new = data_new %>%
      left_join( d_fill_value )
  })


  if( ! is.null(fill_str) ){

    data_new = data_new %>%
      mutate( fill_value = ifelse( as.character(value) == as.character(!!fill)
                                   , fill_flow, fill_value ) )
  }

  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , y = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
                          , width = stratum_width
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
                               , width = stratum_width
                               ) +
    theme(legend.position = "none" ) +
    scale_fill_identity() +
    scale_color_identity() +
    labs( x = '', y = 'count', caption = caption)

  if(stratum_labels){
    p = p + geom_label( stat = ggalluvial::StatStratum )
  }


  # angle x labels------------------------------------

  max_length_x_level = levels( data_new$x ) %>%
    map_int( nchar ) %>%
    max()


  if( max_length_x_level > 5 & auto_rotate_xlabs ){
    p = p +
      theme( axis.text.x = element_text( angle = 90, vjust = 0.5 , hjust = 0 ) )
  }

  # attach alluvial_id to id keys

  suppressMessages({

    data_key = data_spread %>%
      left_join( data_alluvial_id )

    p$data_key = data_key

  })

  return(p)
}



