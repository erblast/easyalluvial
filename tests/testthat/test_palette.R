

test_that('test f_plot_colvector'
  ,{

  plot_rgb = function(col){

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

  plot_rgb_sum = function(col){

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


  p = plot_rgb( col = f_plot_col_vector74( only_unique = T ) )
  p = plot_rgb_sum( f_plot_col_vector74( only_unique = T ) )


  p = plot_rgb( f_plot_col_vector74( reds = F) )
  p = plot_rgb( f_plot_col_vector74( greys = F) )
  p = plot_rgb( f_plot_col_vector74( blues = F) )
  p = plot_rgb( f_plot_col_vector74( greens = F) )
  p = plot_rgb( f_plot_col_vector74( faint = F) )

})
