

#' @title alluvial plot of data in wide format
#' @description plots a dataframe as an alluvial plot. All numerical variables
#'   are scaled, centered and YeoJohnson transformed before binning.
#' @param data a dataframe
#' @param id character vector denoting id column
#' @param max_variables maximum number of variables, Default: 20
#' @param bins number of bins for numerical variables, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH")
#' @param NA_label character vector define label for missing data
#' @param order_levels character vector denoting levels to be reorderer from low to high
#' @param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'   'values')), Default: 'first_variable'
#'@param col_vector_flow HEX colors for flows, Default:
#'  f_plot_col_vector74(faint = F, greys = F)
#'@param col_vector_value Hex colors for y levels/values, Default:
#'  RColorBrewer::brewer.pal(9, "Greys")[c(3, 6, 4, 7, 5)]
#'@param verbose logical, print plot summary, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#' data_ls = mtcars %>%
#'   f_clean_data()
#'
#' data = data_ls$data
#' max_variables = 5
#' variables = c( data_ls$categoricals[1:3], data_ls$numericals[1:3] )
#'
#' alluvial_wide( data = data
#'                 , variables = variables
#'                 , max_variables = max_variables
#'                 , fill_by = 'first_variable' )
#'
#' alluvial_wide( data = data
#'                 , variables = variables
#'                 , max_variables = max_variables
#'                 , fill_by = 'last_variable' )
#'
#' alluvial_wide( data = data
#'                 , variables = variables
#'                 , max_variables = max_variables
#'                 , fill_by = 'all_flows' )
#'
#' alluvial_wide( data = data
#'                 , variables = variables
#'                 , max_variables = max_variables
#'                 , fill_by = 'first_variable' )
#'
#' # manually order variable values
#'
#' alluvial_wide( data = data
#'                  , variables = variables
#'                  , max_variables = max_variables
#'                  , fill_by = 'values'
#'                  , order_levels = c('1', '0') )
#' }
#' }
#' @seealso \code{\link[RColorBrewer]{brewer.pal}}
#'   \code{\link[forcats]{fct_relevel}}
#'   \code{\link[ggalluvial]{geom_flow}},\code{\link[ggalluvial]{geom_stratum}}
#' @rdname alluvial_wide
#' @export
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @import ggalluvial
alluvial_wide = function( data
                            , id = NULL
                            , max_variables = 20
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , NA_label = 'NA'
                            , order_levels = NULL
                            , fill_by = 'first_variable'
                            , col_vector_flow = f_plot_col_vector74( faint = F, greys = F )
                            , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
                            , verbose = F
){

  # ggalluvial package needs to be loaded entirely
  require(ggalluvial)

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

  #adjust col_vector length fill flows

  n_colors_needed = length( unique(data_new$fill) )

  col_vector_flow = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_flow )

  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )

  suppressMessages({
    data_new = data_new %>%
      left_join( df_fill_flow )
  })

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = f_plot_adjust_col_vector_length( n_colors_needed, col_vector_value )

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
                  , weight = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
    ) +
    geom_label( stat = 'stratum') +
    theme(legend.position = "none" ) +
    scale_fill_identity() +
    scale_color_identity() +
    labs( x = '', y = 'count', caption = caption)

  # angle x labels------------------------------------

  max_length_x_level = levels( data_new$x ) %>%
    map_int( nchar ) %>%
    max()

  if( max_length_x_level > 5 ){
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



