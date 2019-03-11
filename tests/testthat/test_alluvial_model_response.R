context('alluvial_model_response')

test_that('get_data_space'
          ,{
            
  set.seed(0)
            
  df = select(mtcars2, -ids)
  m = randomForest::randomForest( disp ~ ., df)
  imp = m$importance
  
  dspace = get_data_space(df, imp, degree = 3, set_to_row_index = 0)
  
  expect_true( all( complete.cases(dspace) ) )
  
  dspace = get_data_space(df, imp, degree = 3, set_to_row_index = 1)
  
  expect_true( all( complete.cases(dspace) ) )
  
  rest = dspace[1, 4:ncol(dspace) ]
  
  expect_equal( rest, select(df,one_of( names(rest) ) )[1,] )
  
  # check that correct number of combinations is returned
  df_num = select_if(df, is.numeric )
  m = randomForest::randomForest( disp ~ ., df_num)
  imp = m$importance
  
  dspace = get_data_space(df_num, imp, degree = 3, set_to_row_index = 0)
  
  expect_equal( nrow(dspace), 5^3 )

})

test_that('pdp_methods'
  ,{
    
    set.seed(0)
    
    df = select(mtcars2, -ids)
    m = randomForest::randomForest( disp ~ ., df)
    imp = m$importance
    
    pred = get_pdp_predictions(df, imp
                                , .f_predict = randomForest:::predict.randomForest
                                , m
                                , degree = 3
                                , bins = 5)
    
    dspace = get_data_space(df, imp, degree = 3)
    
    p = alluvial_model_response(pred, dspace, imp, degree = 3, method = 'pdp')
    
    vdiffr::expect_doppelganger('model_response_pdb', p)
    
})

test_that('alluvial_model_response'
          ,{

    set.seed(0)
    df = select(mtcars2, -ids)
    m = randomForest::randomForest( disp ~ ., df)
    imp = m$importance
    imp_conv = check_imp(imp, df)
    dspace = get_data_space(df, imp, degree = 3)
    
    expect_equal( length( names(dspace) ), length( imp_conv$vars ) )
    expect_true( all( names(dspace) %in% imp_conv$vars ) )
    
    pred = predict(m, newdata = dspace)
    
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    vdiffr::expect_doppelganger('model_response', p)
    
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
    
    # change bin_numerics parameter for pred and bins
    
    p = alluvial_model_response(pred, dspace, imp, degree = 3
                                , bins = 3
                                , bin_labels = c('L','M', 'H')
                                , params_bin_numeric_pred = list(center = F, scale = F, transform = F) )
    

    expect_true( p$alluvial_params$bins == 3 )
    
    vdiffr::expect_doppelganger('model_response_new_change_bins_3', p)
    
    dspace = get_data_space(df, imp, degree = 4)
    pred = predict(m, newdata = dspace)
    alluvial_model_response(pred, dspace, imp, degree = 4, bins = 7, c('LLL','LL', 'ML', 'M', 'MH', 'HH', 'HHH') )
    
    pred_train = predict(m)
    
    p = alluvial_model_response(pred, dspace, imp, degree = 4, bins = 7
                            , bin_labels = c('LLL','LL', 'ML', 'M', 'MH', 'HH', 'HHH')
                            , pred_train = pred_train )
    
    vdiffr::expect_doppelganger('model_response_new_change_bins_7', p)
    
    # bivariate categorical response
    
    set.seed(0)
    df = select(mtcars2, -ids)
    m = randomForest::randomForest( am ~ ., df)
    imp = m$importance
    dspace = get_data_space(df, imp, degree = 3)
    
    pred = predict(m, newdata = dspace,type = 'response')
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    vdiffr::expect_doppelganger('model_response_cat_bi', p)
    
    # multivariate categorical response
    
    set.seed(0)
    df = select(mtcars2, -ids)
    m = randomForest::randomForest( cyl ~ ., df)
    imp = m$importance
    dspace = get_data_space(df, imp, degree = 3)
    
    pred = predict(m, newdata = dspace,type = 'response')
    p = alluvial_model_response(pred, dspace, imp, degree = 3)
    
    vdiffr::expect_doppelganger('model_response_cat_multi', p)
    
})

test_that('alluvial_model_response_caret'
          , {
            
  df = select(mtcars2, -ids)
  
  set.seed(1)
  train = caret::train( disp ~ ., df, method = 'lm',trControl = caret::trainControl(method = 'none') )
  p = alluvial_model_response_caret(train, degree = 3)
  
  vdiffr::expect_doppelganger('model_response_caret_lm', p)
  
  p = alluvial_model_response_caret(train, degree = 3, method = 'pdp')
  vdiffr::expect_doppelganger('model_response_caret_lm_pdp', p)
  
  
  set.seed(0)
  train = caret::train( disp ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  
  vdiffr::expect_doppelganger('model_response_caret_rf', p)
  
  # change bin labels
  p = alluvial_model_response_caret(train, degree = 3, bin_labels =  c('A','B','C','D','E') )
  
  vdiffr::expect_doppelganger('model_response_caret_new_labs', p)
  
  # categorical bivariate response 
  set.seed(1)
  train = caret::train( am ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  vdiffr::expect_doppelganger('model_response_caret_cat_bi', p)
  
  
  # categorical multivariate response
  set.seed(1)
  train = caret::train( cyl ~ ., df, method = 'rf',trControl = caret::trainControl(method = 'none'), importance = T )
  p = alluvial_model_response_caret(train, degree = 3)
  vdiffr::expect_doppelganger('model_response_caret_cat_multi', p)
  
          
  })

