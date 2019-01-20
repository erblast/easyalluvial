context('alluvial_model_response')

test_that('alluvial_model_response'
          ,{
    
    require('mlbench')
            
    data('BostonHousing')
    
    df = as_tibble( BostonHousing )
    
    m = randomForest::randomForest( lstat ~ ., df)
    
    imp = tibble( vars = row.names( m$importance ), imp = m$importance[,1] ) %>%
      arrange( desc(imp) ) %>%
      mutate( cum_imp = cumsum(imp ) )
    
    dspace = get_data_space(df, imp)
    
    pred = predict(m, newdata = dspace)
    
    alluvial_model_response(pred, dspace, imp)
            
  
})