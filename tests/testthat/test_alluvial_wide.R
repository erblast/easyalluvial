
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

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'last_variable' )

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'all_flows' )

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values' )

    # manually order variable values

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values'
                    , order_levels = c('8', '4', '6') )


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

    # check NA behavoir, rename label ando order to front

    data$cylinders[1:4] = NA

    p = alluvial_wide( data = data
                         , max_variables = max_variables
                         , fill_by = 'first_variable'
                         , NA_label = 'none'
                         , order_levels = 'none' )


  })


