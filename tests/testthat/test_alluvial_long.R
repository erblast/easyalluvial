
context('alluvial_long')

test_that('alluvial_long'
          ,{

  # sample data

  data = quarterly_flights

  # flow coloring variants
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier )
  vdiffr::expect_doppelganger('long_fill_carrier', p)
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )
  vdiffr::expect_doppelganger('long_fill_last', p)
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
  vdiffr::expect_doppelganger('long_fill_first', p)
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
  vdiffr::expect_doppelganger('long_fill_value', p)
  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )

  # strings instead of unquoted expressions
  p = alluvial_long( data, key = 'qu', value = 'mean_arr_delay', id = 'tailnum', fill = 'carrier' )


  # use same color coding for flows and y levels
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'value'
                       , col_vector_flow = palette_qualitative() %>% palette_filter(greys = F, bright = F)
                       , col_vector_value = palette_qualitative() %>% palette_filter(greys = F, bright = F) )

  vdiffr::expect_doppelganger('long_sprecify_color', p)
  
  # move fill variable to the left
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier ,fill_right = F )
  
  vdiffr::expect_doppelganger('long_fill_to_right', p)
  
  # reorder levels
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
                       , order_levels_value = c('on_time', 'late') )
  
  vdiffr::expect_doppelganger('long_reorder_y_levels', p)
  
  p = alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
                       , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )
  
  vdiffr::expect_doppelganger('long_reorder_x_levels', p)
  
  order_by_carrier_size = data %>%
    group_by(carrier) %>%
    count() %>%
    arrange( desc(n) ) %>%
    .[['carrier']]

  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
                       , order_levels_fill = order_by_carrier_size )

  vdiffr::expect_doppelganger('long_reorder_carrier_by_size', p)
  
  #check integritiy of returned dataframe
  expect_equivalent( unique(data$tailnum), levels( p$data_key$tailnum ) )

  #check with incomplete data

  data = quarterly_flights %>%
    select(tailnum, qu, mean_arr_delay, carrier) %>%
    sample_frac(0.9)

  p = alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
                     , NA_label = 'none'
                     , order_levels_value = 'none')
  
  # comes up as false positive try again with next vdiffr version
  #vdiffr::expect_doppelganger('long_none_label', p)
  

  # check stratum options

  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier
                     , stratum_labels = FALSE, stratum_width = 1/20)
  
  # comes up as false positive try again with next vdiffr version
  #vdiffr::expect_doppelganger('long_strat_width', p)
  
  # switch off automatic  label angling

  p = alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill = carrier
                     , auto_rotate_xlabs = F )
  
  

  # test warnign for high number of flows

  suppressWarnings({

  data_highflow = ggplot2::diamonds %>%
    mutate( id = as.factor( row_number() ) ) %>%
    manip_bin_numerics() %>%
    gather( key = 'key', value = 'value', -id)

  })

  expect_warning( alluvial_long( data_highflow,  key, value, id ) )
  
  #gouped df
  p = alluvial_long( group_by(data, carrier), key = qu, value = mean_arr_delay, id = tailnum)
  
  # plot attachments
  expect_true( all( c('data_key', 'alluvial_type', 'alluvial_params') %in% names(p) ) )
  
  # numeric sample data
  
  p = alluvial_long(quarterly_sunspots, key = qu, value = spots, id = year)
  vdiffr::expect_doppelganger('long_all_nums', p)
  
  p = alluvial_long(quarterly_sunspots, key = qu, value = spots, id = year, fill = mean_spots_per_year)
  vdiffr::expect_doppelganger('long_all_nums_plus_fill', p)
  

})


