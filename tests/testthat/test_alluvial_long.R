
context('alluvial long')

test_that('alluvial_long'
          ,{

  # sample data

  suppressMessages({
    monthly_flights = nycflights13::flights %>%
      group_by(month, tailnum, origin, dest, carrier) %>%
      summarise() %>%
      group_by( tailnum, origin, dest, carrier) %>%
      count() %>%
      filter( n == 12 ) %>%
      select( - n ) %>%
      left_join( nycflights13::flights ) %>%
      .[complete.cases(.), ] %>%
      ungroup() %>%
      mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
              , qu = cut(month, 4)) %>%
      group_by(tailnum, carrier, origin, dest, qu ) %>%
      summarise( mean_arr_delay = mean(arr_delay) ) %>%
      ungroup() %>%
      mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )
  })

  levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')

  data = monthly_flights

  # flow coloring variants
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier )
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )

  # use same color coding for flows and y levels
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'last_variable'
                       , col_vector_flow = f_plot_col_vector74()
                       , col_vector_value = f_plot_col_vector74() )

  # move fill variable to the left
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier ,fill_right = F )

  # reorder levels
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
                       , order_levels_value = c('on_time', 'late') )

  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
                       , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )

  order_by_carrier_size = data %>%
    group_by(carrier) %>%
    count() %>%
    arrange( desc(n) ) %>%
    .[['carrier']]

  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
                       , order_levels_fill = order_by_carrier_size )


  #check integritiy of returned dataframe
  expect_equivalent( unique(data$tailnum), levels( p$data_key$tailnum ) )

  #check with incomplete data

  data = monthly_flights %>%
    select(tailnum, qu, mean_arr_delay, carrier) %>%
    sample_frac(0.9)

  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
                     , NA_label = 'none'
                     , order_levels_value = 'none')


  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'last_variable'
                           , NA_label = 'none'
                           , order_levels_value = 'none')

})


