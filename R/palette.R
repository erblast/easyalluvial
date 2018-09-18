#'@import nycflights13
#'@import ggplot2
#'@import RColorBrewer
#'@importFrom stringr str_extract_all

#' @title generate a most distinctive color scale
#' @description based on RColorBrewer colours of length 74
#' for RGB colors see {rapidtables}(https://www.rapidtables.com/web/color/index.html).
#' Basically strings a couple of RColorBrewer palettes together.
#' @param greys boolean, include grey colors, Default: TRUE
#' @param reds boolean, include red colors, Default: TRUE
#' @param blues boolean, include blue colors, Default: TRUE
#' @param greens boolean, include green colors, Default: TRUE
#' @param faint boolean, include faint colors, Default: TRUE
#' @param only_unique boolean, do not allow color repetitions, Default: FALSE
#' @return vector with HEX colours
#' @rdname f_plot_col_vector74
#' @export
#' @import RColorBrewer
#'
palette_qualitatives = function(){

  qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

}

palette_filter = function( similar = F
                         , greys = T
                         , reds = T
                         , greens = T
                         , dark = T
                         , medium = T
                         , bright = T ){

  # create tibble with RGB
  col = tibble( hex = col_vector ) %>%
    mutate( rgb = map(hex, col2rgb)
            , rgb = map(rgb, t)
            , rgb = map(rgb, as_tibble) ) %>%
    unnest( rgb )

  # for greys R == G == B
  if( greys == F ){
    col = col %>%
      filter( red != green, red != blue)
  }

  if( reds == F ){
    col = col %>%
      filter( ! ( blue < 50 & green < 50  & red > 200 ) )
  }

  if( greens == F ){
    col = col %>%
      filter( ! ( green > red & green > blue ) )
  }

  if( blues == F ){
    col = col %>%
      filter( ! ( blue > green & green > red) )
  }

  if( bright == F){
    col = col %>%
      filter( (red + green + blue ) < 600 )
  }

  if( similar == F ){

  get_similar = function(i, r, g, b, df, thresh = 5){

    df %>%
      filter( i != index ) %>%
      filter( red >= (r - thresh), red <= (r + thresh) ) %>%
      filter( green >= (g - thresh), green <= (g + thresh) ) %>%
      filter( blue >= (b - thresh), blue <= (b + thresh) ) %>%
      .$index
  }

  col = mutate(col, index = row_number() )

  filter_indeces = col %>%
    mutate( similar_index = pmap( list(index, red, green, blue), get_similar, col ) ) %>%
    unnest( similar_index ) %>%
    mutate( larger_index = ifelse( similar_index > index, similar_index, index) ) %>%
    .[['larger_index']] %>%
    unique()

  col = col %>%
    filter( ! index %in% filter_indeces )

  }

  return( col$hex  )

}


#' @title adjust length of color vector, by repeating colors
#' @param n length, Default: 74
#' @param col_vector vector containing colors, Default: f_plot_col_vector74()
#' @return vector containing colors of specified length
#' @examples
#'
#' length( f_plot_adjust_col_vector_length(100) )
#'
#' @rdname f_plot_adjust_col_vector_length
#' @export
palette_increase_length = function( n = 74, col_vector = f_plot_col_vector74() ){

  multiple = n / length(col_vector)
  multiple = ceiling(multiple)

  col_vector = rep( col_vector, multiple )

  return( col_vector[1:n] )

}


palette_plot_rgp = function(col){

  hex = tibble( hex = col ) %>%
    mutate( RGB = map(hex, col2rgb)
            , RGB = map(RGB, t)
            , RGB = map(RGB, as_tibble)
    ) %>%
    # arrange(hex) %>%
    mutate( hex = forcats::as_factor(hex) )

  col = hex %>%
    unnest( RGB ) %>%
    gather( key = 'RGB', value = 'value', - hex )

  ggplot( col, aes(hex, value, group = RGB, fill = hex) ) +
    geom_col( position = 'dodge', color = 'white') +
    coord_flip() +
    scale_fill_manual( values = levels( hex$hex) )
}


palette_plot_intensity = function(col){

  col = unique(col)

  hex = tibble( hex = col ) %>%
    mutate( RGB = map(hex, col2rgb)
            , RGB = map(RGB, t)
            , RGB = map(RGB, as_tibble)
    ) %>%
    mutate( hex = forcats::as_factor(hex) )

  col = hex %>%
    unnest( RGB ) %>%
    gather( key = 'RGB', value = 'value', - hex ) %>%
    group_by( hex ) %>%
    summarise( sum = sum(value) ) %>%
    mutate( hex = forcats::fct_reorder(hex, sum))

  ggplot( col, aes(hex, sum, fill = hex) ) +
    geom_col(  color = 'white') +
    coord_flip() +
    scale_fill_manual( values = levels( col$hex) )
}

