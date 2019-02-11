context('alluvial_model_response')

test_that('alluvial_model_response'
          ,{

    df = select(mtcars2, -id)
    
    m = randomForest::randomForest( disp ~ ., df)
    
    imp = m$importance
    
    imp_conv = check_imp(imp, df)
    
    dspace = get_data_space(df, imp, degree = 3)
    
    expect_equal( length( names(dspace) ), length( imp_conv$vars ) )
    expect_true( all( names(dspace) %in% imp_conv$vars ) )
    
    pred = predict(m, newdata = dspace)
    
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    # renders differently on each run of vdiffr::manage_cases()
    # vdiffr::expect_doppelganger('model_response', p)
    
    expect_equal( length( levels(p$data$x) ) - 1, 3 )
    
    expect_equal( length( levels(p$data$x) ) - 1, length( arrange(imp_conv, desc(imp))$vars[1:3] ) )
    
    expect_equivalent( levels(p$data$x)[2:( 3 + 1 )],  arrange(imp_conv, desc(imp))$vars[1:3] )
    
    
    # checks
    
    # importance df contains unknown variable

    imp_no_match = bind_rows(imp_conv, tibble( vars = 'xxx', imp = 0.99) )
    
    expect_error( get_data_space(df, imp_no_match) )
    expect_error( alluvial_model_response( pred, dspace, imp_no_match) )
  
    # importance as less variables then degrees
    expect_warning( get_data_space(df, imp_conv[0:2,], degree = 3) )
    expect_warning( alluvial_model_response(pred, dspace, imp_conv[0:2,], degree = 3) )
    
    # number of flows to high
    dspace = get_data_space(df, imp, degree = 6)
    pred = predict(m, newdata = dspace)
    expect_error( alluvial_model_response(pred, dspace, imp) )
    
    # ordered instead of factors
    
    df_ord = df %>%
      mutate_if( is.factor, as.ordered ) 
    
    m = lm( disp ~ ., df_ord)
    
    imp = tibble( var = names( coef(m) )
                  , imp = abs( coef(m) ) ) %>%
      filter(var != '(Intercept)')
    
    imp_conv = check_imp(imp, df)
    
    dspace = get_data_space(df, imp)
    
    pred = predict(m, newdata = dspace)
    
    degree = 3
    
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    # change bin labels
    
    p = alluvial_model_response(pred, dspace, imp, bin_labels = c('A','B','C','D','E'), degree = 3 )
    
    vdiffr::expect_doppelganger('model_response_new_labs', p)
    
    
})

test_that('alluvial_model_response_caret'
          , {
            
  df = select(mtcars2, -id)
  
  train = caret::train( disp ~ ., df, method = 'lm',trControl = caret::trainControl(method = 'none') )
  p = alluvial_model_response_caret(train, degree = 3)
  
  vdiffr::expect_doppelganger('model_response_caret_lm', p)
  
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  
  # renders differently on each run of vdiffr::manage_cases()
  # vdiffr::expect_doppelganger('model_response_caret_rf', p)
  
  # change bin labels
  p = alluvial_model_response_caret(train, degree = 3, bin_labels =  c('A','B','C','D','E') )
  
  # renders differently on each run of vdiffr::manage_cases()
  # vdiffr::expect_doppelganger('model_response_caret_new_labs', p)
  

})

#TODO 
# vdiffr
# check