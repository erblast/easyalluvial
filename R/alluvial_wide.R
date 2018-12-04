
# satisfy CMDcheck
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when


if(getRversion() >= "2.15.1"){
  utils::globalVariables( c('x', '.', ':=', 'alluvial_id', 'fill_flow', 'fill_value', 'value', 'fill' ) )
}



#' @title alluvial plot of data in wide format
#' @description plots a dataframe as an alluvial plot. All numerical variables
#'   are scaled, centered and YeoJohnson transformed before binning. Plots all
#'   variables in the sequence as they appear in the dataframe until maximum
#'   number of values is reached.
#' @param data a dataframe
#' @param id unquoted column name of id column or character vector with id
#'   column name
#' @param max_variables maximum number of variables, Default: 20
#' @param bins number of bins for numerical variables, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH")
#' @param NA_label character vector define label for missing data, Default: 'NA'
#' @param order_levels character vector denoting levels to be reorderer from low
#'   to high
#' @param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'   'values')), Default: 'first_variable'
#' @param col_vector_flow HEX colors for flows, Default: palette_filter( greys =
#'   F)
#' @param col_vector_value Hex colors for y levels/values, Default:
#'   RColorBrewer::brewer.pal(9, "Greys")[c(3, 6, 4, 7, 5)]
#' @param verbose logical, print plot summary, Default: F
#' @param stratum_labels logical, Default: TRUE
#' @param stratum_width double, Default: 1/4
#' @param auto_rotate_xlabs logical, Default: TRUE
#' @return ggplot2 object
#' @details Under the hood this function converts the wide format into long
#'   format. ggalluvial also offers a way to make alluvial plots directly from
#'   wide format tables but it does not allow individual colouring of the
#'   stratum segments. The tradeoff is that we can only order levels as a whole
#'   and not individually by variable, Thus if some variables have levels with
#'   the same name the order will be the same. If we want to change level order
#'   independently we have to assign unique level names first.
#' @examples
#'
#' require(magrittr)
#' require(dplyr)
#'
#' data = as_tibble(mtcars)
#' categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
#' numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')
#' max_variables = 5
#'
#' data = data %>%
#'   mutate_at( vars(categoricals), as.factor )
#'
#'
#' alluvial_wide( data = data
#'                 , max_variables = max_variables
#'                 , fill_by = 'first_variable' )
#'
#' # more coloring variants----------------------
#' alluvial_wide( data = data
#'                 , max_variables = max_variables
#'                 , fill_by = 'last_variable' )
#'
#' alluvial_wide( data = data
#'                 , max_variables = max_variables
#'                 , fill_by = 'all_flows' )
#'
#' alluvial_wide( data = data
#'                 , max_variables = max_variables
#'                 , fill_by = 'first_variable' )
#'
#' # manually order variable values---------------
#'
#' alluvial_wide( data = data
#'                  , max_variables = max_variables
#'                  , fill_by = 'values'
#'                  , order_levels = c('1', '0') )
#'
#' @seealso \code{\link[easyalluvial]{alluvial_wide}}
#'   , \code{\link[ggalluvial]{geom_flow}}, \code{\link[ggalluvial]{geom_stratum}}
#' @rdname alluvial_wide
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @importFrom ggalluvial stat_stratum geom_flow geom_stratum StatStratum
#' @importFrom magrittr %>%
alluvial_wide = function( data
                            , id = NULL
                            , max_variables = 20
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , NA_label = 'NA'
                            , order_levels = NULL
                            , fill_by = 'first_variable'
                            , col_vector_flow = palette_qualitative() %>% palette_filter( greys = F)
                            , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(4,7,5,8,6)]
                            , verbose = F
                            , stratum_labels = T
                            , stratum_width = 1/4
                            , auto_rotate_xlabs = T
                            ){

  # quos

  id = enquo( id )

  if( rlang::quo_is_null(id) ){
    id_str = NULL
  }else{
    id_str = quo_name(id)
  }

  # remove  id from variables

  variables = names(data)

  if( ! is_empty(id_str %in% variables) ) variables = variables[ ! variables %in% id_str ]

  # adjust variable length

  if( max_variables > length(variables) ) max_variables = length(variables)

  variables = unique(variables)

  variables = variables[1:max_variables]

  fill_by =  switch (fill_by
                     , first_variable  = variables[1]
                     , last_variable   = variables[ length(variables) ]
                     , all_flows       = 'alluvial_id'
                     , values          = 'value'
  )

  # reduce data to selected variables

  data = data %>%
    select( one_of(variables, id_str) )

  # prepare ID column

  if( rlang::quo_is_null(id) ){

    data = data %>%
      mutate( ID = row_number() )

    id_str = 'ID'
  }

   data = data %>%
     mutate( !! as.name(id_str) := as.character( !! as.name(id_str) ) )

  # transform numerical variables for binning

  data = data %>%
    manip_bin_numerics( bins, bin_labels)

  data_trans = data %>%
    select( one_of(variables) ) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate( alluvial_id = row_number() )

  suppressMessages({
    data_alluvial = data %>%
      left_join( data_trans ) %>%
      select( one_of( id_str, 'alluvial_id') )
  })

  # preserve order of categorical variables

  ordered_levels = data_trans %>%
    select_if( is.factor ) %>%
    head(1) %>%
    mutate_all( function(x) list(levels(x)) ) %>%
    gather( key = 'key', value = 'levels') %>%
    unnest(levels) %>%
    .$levels %>%
    unique()

  if( ! is.null(order_levels) ){
    ordered_levels = ordered_levels[ ! ordered_levels %in% order_levels ]
    ordered_levels = c( order_levels, ordered_levels)

  }

  # Prepare dataframe

  # this code is a bit redundant but the different fill options require
  # different transformations. I have marked the sections where there are
  # differences with ## ***


  suppressWarnings(

    if( fill_by != 'value' ){

      data_new <- data_trans %>%
        mutate(  fill = !! as.name(fill_by) )  %>% ## ***
        gather( key = 'x', value = 'value', - alluvial_id, - n , - fill) %>% ## ***
        mutate( value = ifelse( is.na(value), NA_label, value )
                , value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(fill) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }else{

      data_new <- data_trans %>%
        gather( key = 'x', value = 'value', - alluvial_id, - n ) %>% ## ***
        mutate( value = ifelse( is.na(value), NA_label, value )
                , value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(value) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }

  )

  n_flows    = max( manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/nrow(data) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data) * 100, 1 )

  line1 = paste('Number of flows:', n_flows)
  line2 = paste('Original Dataframe reduced to', reduced_to, '%' )
  line3 = paste('Maximum weight of a single flow', max_weight_perc, '%')

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
      theme( axis.text.x = element_text( angle = 90 ) )
  }

  # attach alluvial_id to id keys

  suppressMessages({

    data_key = data_new %>%
      mutate( alluvial_id = manip_factor_2_numeric(alluvial_id) ) %>%
      left_join( data_alluvial ) %>%
      select( - fill_flow, -fill_value, -fill ) %>%
      spread( key = x, value = value ) %>%
      select( one_of(id_str, variables, 'alluvial_id', 'n' ) )
  })

  p$data_key = data_key


  return(p)
}



