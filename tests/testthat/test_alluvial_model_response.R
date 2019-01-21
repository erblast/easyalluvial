context('alluvial_model_response')

test_that('alluvial_model_response'
          ,{
    
    require('mlbench')
            
    data('BostonHousing')
    
    df = as_tibble( BostonHousing )
    
    m = randomForest::randomForest( lstat ~ ., df)
    
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
    
    
    # with factor variable
    
    m = randomForest::randomForest( lstat ~ chas + medv + rm, df)
    
    imp = m$importance
    
    imp_df = check_imp(imp, df)
    
    dspace = get_data_space(df, imp)
    
    pred = predict(m, newdata = dspace)
    
    p = alluvial_model_response(pred, dspace, imp)
    
    expect_equivalent( levels(p$data$x)[2:( degree + 1 )],  arrange(imp_df, desc(imp))$vars[1:degree] )
    
    # checks
    
    imp_no_match = bind_rows(imp_df, tibble( vars = 'xxx', imp = 0.99) )
    
    expect_error( get_data_space(df, imp_no_match) )
    expect_error( alluvial_model_response( pred, dspace, imp_no_match) )
    
    expect_warning( get_data_space(df, imp_df[0:2,], degree = 3) )
    expect_warning( alluvial_model_response(pred, dspace, imp_df[0:2,], degree = 3) )
    
    m = randomForest::randomForest( lstat ~ ., df)
    imp = m$importance
    dspace = get_data_space(df, imp, degree = 6)
    pred = predict(m, newdata = dspace)
    expect_error( alluvial_model_response(pred, dspace, imp) )
    
    
  })

test_that('alluvial_model_response_caret'
          , {
            
  data('BostonHousing')
  
  df = as_tibble( BostonHousing )
  
  train = caret::train( lstat ~ ., df, method = 'lm',trControl = trainControl(method = 'none') )
  alluvial_model_response_caret(train)
  
  train = caret::train( lstat ~ ., df, method = 'rf',trControl = trainControl(method = 'none'), importance = T )
  alluvial_model_response_caret(train)
  
})

#TODO 
# better colours
#, pass ... to alluvial_wide
#, change dataset for test
#, documentation
#, allow different bin labels