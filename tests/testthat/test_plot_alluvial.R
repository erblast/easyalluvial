
context('Plot alluvial functions')

test_that('f_plot_alluvial'
  ,{


    data = as_tibble(mtcars)
    categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
    numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')
    max_variables = 5

    variables = c( categoricals[1:3], numericals[1:3] )

    data = data %>%
      mutate_at( vars(categoricals), as.factor )

    p = f_plot_alluvial( data = data
                    , variables = variables
                    , max_variables = max_variables
                    , fill_by = 'first_variable' )

    p = f_plot_alluvial( data = data
                    , variables = variables
                    , max_variables = max_variables
                    , fill_by = 'last_variable' )

    p = f_plot_alluvial( data = data
                    , variables = variables
                    , max_variables = max_variables
                    , fill_by = 'all_flows' )

    p = f_plot_alluvial( data = data
                    , variables = variables
                    , max_variables = max_variables
                    , fill_by = 'values' )

    # manually order variable values

    p = f_plot_alluvial( data = data
                    , variables = variables
                    , max_variables = max_variables
                    , fill_by = 'values'
                    , order_levels = c('1', '0') )


    #check integritiy of returned dataframe
    expect_equal( nrow(data), nrow(p$data_key) )

    #check automatic angling of x axis labels

    data = ISLR::Auto %>%
      as_tibble() %>%
      mutate( name_x = row_number()
              , name_x = paste( name, name_x ) ) %>%
      select( - name ) %>%
      mutate_at( vars( c('cylinders', 'year', 'origin' ) ), as.factor  )

    variables = names(data)

    p = f_plot_alluvial( data, col_id = 'name_x', max_variables = 5 )

    # check NA behavoir, rename label ando order to front

    data$cylinders[1:4] = NA

    p = f_plot_alluvial( data = data
                         , variables = variables
                         , max_variables = max_variables
                         , fill_by = 'first_variable'
                         , NA_label = 'none'
                         , order_levels = 'none' )
  })


test_that('f_plot_alluvial_1v1'
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

  col_x = 'qu'
  col_y = 'mean_arr_delay'
  col_fill = 'carrier'
  col_id = 'tailnum'

  # flow coloring variants
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'all_flows' )
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'value' )

  # use same color coding for flows and y levels
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable'
                       , col_vector_flow = f_plot_col_vector74()
                       , col_vector_value = f_plot_col_vector74() )

  # move fill variable to the left
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill, fill_right = F )

  # reorder levels
  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
                       , order_levels_y = c('on_time', 'late') )

  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'first_variable'
                       , order_levels_x = c('Q4', 'Q3', 'Q2', 'Q1') )

  order_by_carrier_size = data %>%
    group_by(carrier) %>%
    count() %>%
    arrange( desc(n) ) %>%
    .[['carrier']]

  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill
                       , order_levels_fill = order_by_carrier_size )


  #check integritiy of returned dataframe
  expect_equivalent( unique(data$tailnum), levels( p$data_key$tailnum ) )

  #check with incomplete data

  data = monthly_flights %>%
    select(tailnum, qu, mean_arr_delay, carrier) %>%
    sample_frac(0.9)

  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, col_fill  = 'carrier'
                           , NA_label = 'none'
                           , order_levels_y = 'none')


  p = f_plot_alluvial_1v1( data, col_x, col_y, col_id, fill_by = 'last_variable'
                           , NA_label = 'none'
                           , order_levels_y = 'none')

})


