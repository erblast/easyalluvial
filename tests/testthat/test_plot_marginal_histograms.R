

context('marginal histograms')

test_that('plot_hist_as_margins',{
  
  # wide
  p = alluvial_wide( data = mtcars2, id = ids
                     , max_variables = 5                 
                     , fill_by = 'first_variable'
                     , colorful_fill_variable_stratum = T)
  
  plot_hist('cyl', p, mtcars2)
  plot_hist_wide('cyl', p , mtcars2)
  plot_hist('disp',p, mtcars2)
  plot_hist_wide('disp',p, mtcars2)
  
  p_grid = add_marginal_histograms(p, mtcars2) 
  
  # model response
  df = select(mtcars2, -ids)
  train = caret::train( disp ~ ., df, method = 'lm',trControl = caret::trainControl(method = 'none') )
  
  p = alluvial_model_response_caret(train, degree = 3)
  
  plot_hist('cyl', p, df)
  plot_hist('wt',p, df)
  plot_hist('pred', p, df)
  pred_train = caret::predict.train(train)
  plot_hist('pred', p, df, pred_train = pred_train)
  plot_hist('carb', p, df)
  plot_hist_model_response('pred', p, df)
  
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  
  p = alluvial_model_response_caret(train, degree = 3, pred_train = pred_train
                                    , bins = 7, c('LLL','LL', 'ML', 'M', 'MH', 'HH', 'HHH'))
  
  plot_hist('pred', p, df)
  
  
  add_marginal_histograms(p, df)
  add_marginal_histograms(p, df, pred_train = pred_train)
  add_marginal_histograms(p, df, pred_train = pred_train, keep_labels = T)
  
  add_marginal_histograms(p, mtcars2)
  
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  add_marginal_histograms(p, df, pred_train = predict(train) )
  
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 4, method = 'pdp')
  add_marginal_histograms(p, df)
  
  train = caret::train( cyl ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  add_marginal_histograms(p, df)
  
  df = select(mtcars2, disp, cyl, carb, mpg, wt)
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  add_marginal_histograms(p, df)
  
  df = ggplot2::diamonds
  train = caret::train( price ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 4, method = 'pdp')
  add_marginal_histograms(p, df)
  
})