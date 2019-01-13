
context('alluvial wide')

test_that('alluvial_wide'
  ,{

    data = as_tibble(mtcars) %>%
      mutate( ids = row_number() )

    categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
    numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')
    max_variables = 5

    data = data %>%
      mutate_at( vars(categoricals), as.factor )

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'first_variable' )
    
    
    vdiffr::expect_doppelganger('wide_first', p)

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'last_variable' )
    
    vdiffr::expect_doppelganger('wide_last', p)
    
    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'all_flows' )

    vdiffr::expect_doppelganger('wide_all_flows', p)
    
    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values' )

    vdiffr::expect_doppelganger('wide_values', p)
    
    # manually order variable values

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values'
                    , order_levels = c('8', '4', '6') )
    
    vdiffr::expect_doppelganger('wide_reorder_y_levels', p)
    
    #check integritiy of returned dataframe
    expect_equal( nrow(data), nrow(p$data_key) )

    # ids

    p = alluvial_wide(data, id = ids )
    expect_true( ! 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ids) ) == nrow(p$data_key)  )

    p = alluvial_wide(data, id = 'ids' )
    expect_true( ! 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ids) ) == nrow(p$data_key)  )

    p = alluvial_wide(data, id = NULL)
    expect_true( 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ID) ) == nrow(p$data_key)  )

    #check automatic angling of x axis labels

    data = ISLR::Auto %>%
      as_tibble() %>%
      mutate( name_x = row_number()
              , name_x = paste( name, name_x ) ) %>%
      select( - name ) %>%
      mutate_at( vars( c('cylinders', 'year', 'origin' ) ), as.factor  )

    p = alluvial_wide( data, id = name_x, max_variables = 5 ) 
    
    vdiffr::expect_doppelganger('wide_ISLR_cars', p)
    
    p = alluvial_wide( data, id = name_x, max_variables = 5, auto_rotate_xlabs = F )
    
    vdiffr::expect_doppelganger('wide_ISLR_cars_rotate_labels', p)
    
    # check NA behavoir, rename label ando order to front

    data$cylinders[1:4] = NA

    p = alluvial_wide( data = data
                         , max_variables = max_variables
                         , fill_by = 'first_variable'
                         , NA_label = 'none'
                         , order_levels = 'none' )
    
    #vdiffr detects difference when rendered with different OS
    #vdiffr::expect_doppelganger('wide_NA_label', p)
    
    # test statum options

    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'first_variable'
                       , stratum_labels = F
                       , stratum_width = 1/20 )
    
    #vdiffr detects difference when rendered with different OS
    #vdiffr::expect_doppelganger('wide_Strat_width', p)

    # test warning for high flow numbers

    expect_warning( alluvial_wide( data = ggplot2::diamonds) )
    
    alluvial_wide(data, max_variables = 3, col_vector_flow = c('red', 'green', 'orange', 'yellow', 'blue')
    , col_vector_value =  c('red', 'green', 'orange', 'yellow', 'blue'), fill_by = 'last_variable' )

  })


