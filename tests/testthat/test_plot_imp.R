context('plot_imp')

test_that('plot_hist'
          ,{
            
  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
  
  train = caret::train( disp ~ .
                      , df
                      , method = 'rf'
                      , trControl = caret::trainControl( method = 'none' )
                      , importance = TRUE )
  
  p = alluvial_model_response_caret(train, degree = 3)
  
  plot_imp(p)          
})