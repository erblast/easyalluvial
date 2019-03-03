

context('marginal histograms')

test_that('plot_hist_as_margins',{
  
  # wide----------------------------------------------
  
  p_wide = alluvial_wide( data = mtcars2, id = ids
                     , max_variables = 5                 
                     , fill_by = 'first_variable'
                     , colorful_fill_variable_stratum = T)
  
  set.seed(1)
  p = plot_hist('cyl', p_wide, mtcars2)
  vdiffr::expect_doppelganger('plot_hist_wide_cat', p)

  set.seed(1)
  p = plot_hist('disp',p_wide, mtcars2)
  vdiffr::expect_doppelganger('plot_hist_wide_num', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_wide, mtcars2)
  vdiffr::expect_doppelganger('marg_hist_wide', p)
  
  
  # long numeric---------------------------------------
  
  p_long = alluvial_long(quarterly_sunspots, key = qu, value = spots
                    , id = year)
  
  set.seed(1)
  p = plot_hist('Q1',p_long, quarterly_sunspots)
  vdiffr::expect_doppelganger('plot_hist_long_num', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_sunspots)
  vdiffr::expect_doppelganger('marg_hist_long', p)
  
  p_long = alluvial_long(quarterly_sunspots, key = qu, value = spots
                     , id = year, fill = mean_spots_per_year)
  set.seed(1)
  p = plot_hist('Q1',p_long, quarterly_sunspots)
  vdiffr::expect_doppelganger('plot_hist_long_num_has_fill', p)
  
  set.seed(1)
  p = plot_hist('mean_spots_per_year',p_long, quarterly_sunspots)
  vdiffr::expect_doppelganger('plot_hist_long_num_is_fill', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_sunspots)
  vdiffr::expect_doppelganger('marg_hist_long_num_fill', p)
  
  
  # long categoric --------------------------------------
  
  p_long = alluvial_long(quarterly_flights, key = qu, value = mean_arr_delay
                    , id = tailnum, fill = carrier)
  
  set.seed(1)
  p = plot_hist('Q1', p_long, quarterly_flights)
  vdiffr::expect_doppelganger('plot_hist_long_cat', p)
  
  set.seed(1)
  p = plot_hist('carrier', p_long, quarterly_flights)
  vdiffr::expect_doppelganger('plot_hist_long_cat_fill', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_flights)
  vdiffr::expect_doppelganger('marg_hist_long_cat-fill', p)
  
  # model response numeric ----------------------------------
  # set.seed(1)
  # df = select(mtcars2, -ids)
  # train = caret::train( disp ~ ., df, method = 'lm',trControl = caret::trainControl(method = 'none') )
  # p = alluvial_model_response_caret(train, degree = 3)
  # p = add_marginal_histograms(p, df)
  # 
  # plot_hist('cyl', p, df)
  # plot_hist('wt',p, df)
  # plot_hist('pred', p, df)
  # 
  # pred_train = caret::predict.train(train)
  # plot_hist('pred', p, df, pred_train = pred_train)
  # plot_hist('carb', p, df)
  # plot_hist_model_response('pred', p, df)
  # 
  # train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # 
  # p = alluvial_model_response_caret(train, degree = 3, pred_train = pred_train
  #                                   , bins = 7, c('LLL','LL', 'ML', 'M', 'MH', 'HH', 'HHH'))
  # 
  # plot_hist('pred', p, df)
  # 
  # 
  # add_marginal_histograms(p, df)
  # add_marginal_histograms(p, df, pred_train = pred_train)
  # add_marginal_histograms(p, df, pred_train = pred_train, keep_labels = T)
  # 
  # add_marginal_histograms(p, mtcars2)
  # 
  # train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # p = alluvial_model_response_caret(train, degree = 3)
  # add_marginal_histograms(p, df, pred_train = predict(train) )
  # 
  # train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # p = alluvial_model_response_caret(train, degree = 4, method = 'pdp')
  # add_marginal_histograms(p, df)
  # 
  # train = caret::train( cyl ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # p = alluvial_model_response_caret(train, degree = 3)
  # add_marginal_histograms(p, df)
  # 
  # df = select(mtcars2, disp, cyl, carb, mpg, wt)
  # train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # p = alluvial_model_response_caret(train, degree = 3)
  # add_marginal_histograms(p, df)
  # 
  # df = ggplot2::diamonds
  # train = caret::train( price ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  # p = alluvial_model_response_caret(train, degree = 4, method = 'pdp')
  # add_marginal_histograms(p, df)
  
})