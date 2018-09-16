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
f_plot_col_vector74 = function( greys = T
                                , reds = T
                                , blues = T
                                , greens = T
                                , faint = T
                                , only_unique = F ){

  library(RColorBrewer)

  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

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

  if( faint == F){
    col = col %>%
      filter( (red + green + blue ) < 600 )
  }

  if( only_unique ) col_hex = unique(col$hex)

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
f_plot_adjust_col_vector_length = function( n = 74, col_vector = f_plot_col_vector74() ){

  multiple = n / length(col_vector)
  multiple = ceiling(multiple)

  col_vector = rep( col_vector, multiple )

  return( col_vector[1:n] )

}


