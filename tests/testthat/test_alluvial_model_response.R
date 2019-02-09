context('alluvial_model_response')

test_that('alluvial_model_response'
          ,{

                
    df = select(mtcars_factor, -id)
    
    m = randomForest::randomForest( disp ~ ., df)
    
    imp = m$importance
    
    imp_conv = check_imp(imp, df)
    
    dspace = get_data_space(df, imp)
    
    expect_equal( length( names(dspace) ), length( imp_conv$vars ) )
    expect_true( all( names(dspace) %in% imp_conv$vars ) )
    
    pred = predict(m, newdata = dspace)
    
    degree = 3
    
    p = alluvial_model_response(pred, dspace, imp, degree = degree)
    
    expect_equal( length( levels(p$data$x) ) - 1, degree )
    
    expect_equal( length( levels(p$data$x) ) - 1, length( arrange(imp_conv, desc(imp))$vars[1:degree] ) )
    
    expect_equivalent( levels(p$data$x)[2:( degree + 1 )],  arrange(imp_conv, desc(imp))$vars[1:degree] )
    
    
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
    
    p = alluvial_model_response(pred, dspace, imp, degree = degree)
    
    
})

test_that('alluvial_model_response_caret'
          , {
            
  df = select(mtcars_factor, -id)
  
  train = caret::train( disp ~ ., df, method = 'lm',trControl = caret::trainControl(method = 'none') )
  alluvial_model_response_caret(train)
  
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  alluvial_model_response_caret(train)
  
})

#TODO 
# better colours
#, pass ... to alluvial_wide
#, change dataset for test
#, documentation
#, allow different bin labels