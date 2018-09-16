#' @title converts factor to numeric preserving numeric levels and order in character levels
#' @param vec vector
#' @return vector
#' @examples
#' fac_num = factor( c(1,3,8) )
#' fac_chr = factor( c('foo','bar') )
#' fac_chr_ordered = factor( c('a','b','c'), ordered = T )
#'
#' manip_factor_2_numeric( fac_num )
#' manip_factor_2_numeric( fac_chr )
#' manip_factor_2_numeric( fac_chr_ordered )
#' @seealso
#'  \code{\link[stringr]{str_detect}}
#' @rdname manip_factor_2_numeric
#' @export
#' @importFrom stringr str_detect
#' @import dplyr
manip_factor_2_numeric = function(vec){

  bool = as.character(vec) %>%
    stringr::str_detect('^\\d+$' ) %>%
    all()

  if( bool ){

    vec = vec %>%
      as.character %>%
      as.numeric()

  } else{
    vec = as.numeric(vec)
  }

  return(vec)
}


#' @title bin numerical columns
#' @description centers, scales and Yeo Johnson transforms numeric variables in
#'   a dataframe before binning into n bins of eqal range. Outliers based on
#'   boxplot stats are capped (set to min or max of boxplot stats).
#' @param df dataframe with numeric variables
#' @param bins number of bins for numerical variables, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH")#' @param center boolean, Default: T
#' @param scale boolean, Default: T
#' @param center boolean, Default: T
#' @param transform boolean, Default: T
#' @return dataframe
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname manip_bin_numerics
#' @import recipes
#' @import broom
#' @export
manip_bin_numerics = function(df
                                , bins = 5
                                , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                                , center = T
                                , scale = T
                                , transform = T){

  require(recipes)

  if( length(bin_labels) != bins ){
    stop( 'bin_labes must be equal to bins')
  }

  numerics = df %>%
    select_if( is.numeric ) %>%
    select_if( function(x) var(x) > 0 ) %>%  ##boxplotstats produces NA if var == 0
    names()

  if( is_empty(numerics) ){
    return( df )
  }

  rec = recipe(df)

  if( center ) rec = rec %>%
    step_center( one_of(numerics) )

  if( scale ) rec = rec  %>%
    step_scale( one_of(numerics) )

  if( transform ) rec = rec %>%
    step_YeoJohnson( one_of(numerics) )

  rec = rec %>%
    prep()

  rename_levels = function(x){
    levels(x) = bin_labels
    return(x)
  }

  data_new <- bake(rec, df ) %>%
    mutate_at( vars(numerics), function(x) ifelse( x > max(boxplot.stats(x)$stats)
                                                   , max(boxplot.stats(x)$stats)
                                                   , x)
               ) %>%
  mutate_at( vars(numerics), function(x) ifelse( x < min(boxplot.stats(x)$stats)
                                                 , min(boxplot.stats(x)$stats)
                                                 , x)
             ) %>%
  mutate_at( vars(numerics), function(x) cut(x, breaks = bins) ) %>%
    mutate_at( vars(numerics),  rename_levels)

  return(data_new)

}





