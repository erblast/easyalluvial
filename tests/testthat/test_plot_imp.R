context('plot_imp')

test_that('plot_imp'
          ,{
            
  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
  set.seed(1)
  train = caret::train( disp ~ .
                      , df
                      , method = 'rf'
                      , trControl = caret::trainControl( method = 'none' )
                      , importance = TRUE )
  
  p = alluvial_model_response_caret(train, df, degree = 3)
  
  p_imp = plot_imp(p, df)
  
  expect_doppelganger('plot_imp', p_imp)
  
})

test_that('add_importance_plot'
          ,{
            
  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
  
  train = caret::train( disp ~ .
                        , df
                        , method = 'rf'
                        , trControl = caret::trainControl( method = 'none' )
                        , importance = TRUE )
  
  pred_train = caret::predict.train(train, df)
  
  # we can silence the warning by adjusting bins and labels but we want to 
  # maintain more bins for pred_train
  expect_warning(p <- alluvial_model_response_caret(train, df, degree = 4, pred_train = pred_train))
  
  p_grid = add_marginal_histograms(p, data_input = df, plot = F)
  
  expect_true( 'gtable' %in% class(p_grid) )
  
  p_grid = add_imp_plot(p_grid, p, data_input = df, plot = F)
  
  p_grid = add_imp_plot(p, data_input = df, plot = F)
  
})

