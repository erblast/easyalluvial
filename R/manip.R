#' @title converts factor to numeric preserving numeric levels and order in
#'   character levels.
#' @description before converting we check whether the levels contain a number,
#'   if they do the number will be preserved.
#' @param vec vector
#' @return vector
#' @examples
#' fac_num = factor( c(1,3,8) )
#' fac_chr = factor( c('foo','bar') )
#' fac_chr_ordered = factor( c('a','b','c'), ordered = TRUE )
#'
#' manip_factor_2_numeric( fac_num )
#' manip_factor_2_numeric( fac_chr )
#' manip_factor_2_numeric( fac_chr_ordered )
#' @seealso \code{\link[stringr]{str_detect}}
#' @rdname manip_factor_2_numeric
#' @export
#' @importFrom stringr str_detect
#' @import dplyr
#' @importFrom grDevices boxplot.stats col2rgb rgb
#' @importFrom stats var
#' @importFrom utils head
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
#'   a dataframe before binning into n bins of equal range. Outliers based on
#'   boxplot stats are capped (set to min or max of boxplot stats).
#' @param x dataframe with numeric variables, or numeric vector
#' @param bins number of bins for numerical variables, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH")#' @param center boolean, Default: T
#' @param scale boolean, Default: T
#' @param center boolean, Default: T
#' @param transform boolean, Default: T
#' @examples
#' summary( mtcars )
#' summary( manip_bin_numerics(mtcars) )
#' @return dataframe
#' @rdname manip_bin_numerics
#' @import recipes
#' @importFrom purrr is_bare_numeric
#' @importFrom tibble is_tibble
#' @export
manip_bin_numerics = function(x
                                , bins = 5
                                , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                                , center = T
                                , scale = T
                                , transform = T){


  if(purrr::is_bare_numeric(x)){
    df = tibble( x = x )
    input_vector = T
  } else if( is.data.frame(x) | is_tibble(x) ){
    df = x
    input_vector = F
  } else{
    return(x)
  }

  requireNamespace('recipes')

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

  if( input_vector ){
    return( data_new$x )
  }else{
    return(data_new)
  }

}





