
check_degree = function(degree, imp, df){

  if( degree > nrow(imp) ){
    degree = nrow(imp)
    warning('degree higher than number of important variables, degrees adjusted')
  }

  return(degree)
}

check_imp = function(imp, df, .f = max){

  if( ! "data.frame" %in% class(imp) & ! 'matrix' %in% class(imp) ){
    stop( paste('imp needs to be of class "data.frame" instead passed object of class'
                , paste( class(imp), collapse = ', ' ) ) )
  }
  
  imp = as.data.frame(imp)

  if( ncol( select_if(imp, is.numeric) ) != 1 ){
    stop( paste('"imp" must have at least one but not more than one numeric columns.'
                , 'Number numeric columns:', ncol( select_if(imp, is.numeric)) ) )
  }
  
  if( ncol( select_if(imp, is.character) ) > 1 ){
    stop('"imp" must not have more than one character column')
  }

  if( ncol(imp) == 2 ){
    imp = imp %>%
      mutate_if( is.factor, as.character ) %>%
      rename_if( is.numeric, function(x) 'imp' ) %>%
      rename_if( is.character, function(x) 'vars')
  }

  if( ncol(imp) == 1 ){
    imp = tibble( vars = row.names( imp ), imp = imp[,1] )
  }

  # correct dummyvariable names back to original name

  df_ori_var = tibble( ori_var = names( select_if(df, is.factor) ) ) %>%
    mutate( len = map_int( ori_var, nchar ) ) %>%
    arrange(len)

  imp = imp %>%
    mutate( ori = vars )

  # go from shortest variabe name to longest, matches with longer variable
  # names will overwrite matches from shorter variable names

  for( ori_var in df_ori_var$ori_var ){

    imp = imp %>%
      mutate( ori = ifelse( str_detect(ori, ori_var), ori_var, ori ) )
  }

  imp = imp %>%
    mutate( vars = ori ) %>%
    select( - ori ) %>%
    group_by( vars ) %>%
    summarise( imp = .f(imp) ) %>%
    arrange( desc(imp) )



  # final checks

  if( ncol(imp) != 2 | ! all( c('vars', 'imp') %in% names(imp) ) ){
    stop( 'supplied imp data.frame could not be converted to right format' )
  }

  if( ! all( imp$vars %in% names(df) ) ){
    stop('not all listed important variables found in input data')
  }

  return(imp)
}

#'@title calculate data space
#'@description calculates a dataspace based on the modelling dataframe and the
#'  importance of the explanatory variables. It selects a the most important
#'  variables and calculates a set number of bins using
#'  \code{\link[easyalluvial]{manip_bin_numerics}} for each numeric variable and
#'  calculates the median of each bin. With the default setting this gives 5
#'  values per numeric variable which are spread over its relevant range. These
#'  values are then used to create all possible combinations of all values in
#'  the set of the most important variables including factor variables. All
#'  other variables which are not in the set of the most important variables are
#'  set to median (numeric variables) of mode (factor variables). The result is
#'  then returned as a dataframe that can be used to get prediction from a model
#'  trained on the input data.
#'@param df dataframe, training data
#'@param imp dataframe, with not more then two columns one of them numeric
#'  containing importance measures and one character or factor column containing
#'  corresponding variable names as found in training data.
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, increasing this
#'  number might result in too many flows, Default: 5
#'@param set_to_row_index integer, set variables which are not set as variable
#'  of top importance by the degree parameter are set to values found at this
#'  row index. If set_to_row_index = 0 median mode is calculated instead.
#'  Default: 0
#'@return data frame
#'@details this model visualisation approach follows the "visualising the model
#'  in the dataspace" principle as described in Wickham H, Cook D, Hofmann H
#'  (2015) Visualizing statistical models: Removing the blindfold. Statistical
#'  Analysis and Data Mining 8(4) <doi:10.1002/sam.11271>
#' @examples
#' df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#' m = randomForest::randomForest( disp ~ ., df)
#' imp = m$importance
#' dspace = get_data_space(df, imp)
#'@rdname get_data_space
#'@export
#'@seealso \code{\link[easyalluvial]{alluvial_wide}},
#'  \code{\link[easyalluvial]{manip_bin_numerics}}
get_data_space = function(df, imp, degree = 4, bins = 5, set_to_row_index = 0){

  degree = check_degree(degree, imp, df)

  imp = check_imp(imp, df)

  imp = arrange(imp, desc(imp) )

  imp_top = imp[1:degree,]

  df_top = select(df, one_of(imp_top$vars) )

  numerics_top = names( select_if( df_top, is.numeric ) )

  df_facs = manip_bin_numerics(df_top, bin_labels = 'median', bins = bins) %>%
    distinct() %>%
    tidyr::complete( !!! map( names(df_top) , as.name) ) %>%
    mutate_at( vars(one_of(numerics_top)), function(x) as.numeric( as.character(x)) ) %>%
    mutate_if( is.factor, fct_lump, n = bins )

  mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  if( nrow(imp) > degree ){
    imp_rest = imp[(degree + 1):nrow(imp), ]

    df_rest = select(df, one_of(imp_rest$vars) )

    if( set_to_row_index == 0){
      df_rest = df_rest %>%
        mutate_if( is.numeric, median ) %>%
        mutate_if( function(x) is.factor(x) | is.character(x), mode) %>%
        head(1) %>%
        sample_n(nrow(df_facs), replace = T)

    } else{
      df_rest = df_rest[ set_to_row_index, ] %>%
        sample_n(nrow(df_facs), replace = T)
    }

    dspace = bind_cols( df_facs, df_rest)

  }else{
    dspace = df_facs
  }

  dspace = select( dspace, one_of( imp$vars[1:degree] ), everything() )

  return(dspace)
}

#'@title get predictions compatibel with the partial dependency plotting method
#'@description the partial dependency plotting method uses the averaged
#'  predictions of all observations in the training data in which the values of
#'  the variables of interest have been modified. The variables of interest
#'  being those for which we want to plot the model response. This is a wrapper
#'  that iterates over all rows in the training data and calls
#'  \code{\link[easyalluvial]{get_data_space}}, feeds it into the supplied
#'  predict function and averages the results.
#'@param df dataframe, training data
#'@param imp dataframe, with not more then two columns one of them numeric
#'  containing importance measures and one character or factor column containing
#'  corresponding variable names as found in training data.
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, increasing this
#'  number might result in too many flows, Default: 5
#'@param .f_predict corresponding model predict() function. Often predict
#'  functions are undocumented and can be found using `:::`. For example
#'  `randomForest:::predict.randomForest`. Predictict functions needs to accept
#'  `m` as the first parameter and use the `newdata` parameter. Supply a wrapper
#'  for predict functions with x-y synthax.
#'@param m model object
#'@return vector, predictions
#'@details DETAILS
#' @examples
#'  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#'  m = randomForest::randomForest( disp ~ ., df)
#'  imp = m$importance
#'
#'  pred = get_pdp_predictions(df, imp
#'                             , .f_predict = randomForest:::predict.randomForest
#'                             , m
#'                             , degree = 3
#'                             , bins = 5)
#'
#'@seealso \code{\link[progress]{progress_bar}}
#'@rdname get_pdp_predictions
#'@export
#'@importFrom progress progress_bar
get_pdp_predictions = function(df, imp, .f_predict, m, degree = 4, bins = 5){

  pb = progress::progress_bar$new(total = nrow(df))

  pred_results = rep(0, nrow(get_data_space(df, imp, degree, bins ) ) )

  for( i in seq(1, nrow(df) ) ){

    sub_dspace = get_data_space(df, imp, degree, bins, set_to_row_index = i)

    pred = .f_predict(m, newdata = sub_dspace)

    pred = pred * 1/nrow(df)

    pred_results = pred_results + pred

    pb$tick()
  }

  return(pred_results)
}


get_cuts = function( from, target, scale = T, center = T, transform = T, ... ){
  
  cuts = levels( manip_bin_numerics(from, bin_labels = 'min_max', ... ) )%>%
    paste( collapse = ',') %>%
    str_replace_all('\\ -\n ', ',') %>%
    str_split(',') %>%
    map(unique) %>%
    map(as.numeric) %>%
    map(sort) %>%
    unlist()
  
  merge1 = seq(2, length(cuts)-1, 2 )
  merge2 = seq(3, length(cuts)-1, 2 )
  
  merged = (cuts[merge1] + cuts[merge2]) / 2
  
  new_cuts = sort( c( cuts[1] - 1 , merged, cuts[length(cuts)] + 1 ) )
  
  if(min(target) < min(new_cuts)){
    new_cuts[1] <- (min(target) - 1)
  }
  
  if(max(target) > max(new_cuts)){
    new_cuts[ length(new_cuts) ] <- (max(target) + 1)
  }
  
  return(new_cuts)
} 

#'@title create model response plot
#'@description alluvial plots are capable of displaying higher dimensional data
#'  on a plane, thus lend themselves to plot the response of a statistical model
#'  to changes in the input data across multiple dimensions. The practical limit
#'  here is 4 dimensions. We need the data space (a sensible range of data
#'  calculated based on the importance of the explanatory variables of the model
#'  as created by \code{\link[easyalluvial]{get_data_space}} and the predictions
#'  returned by the model in response to the data space.
#'@param pred vector, predictions, if method = 'pdp' use
#'  \code{\link[easyalluvial]{get_pdp_predictions}} to calculate predictions
#'@param dspace data frame, returned by
#'  \code{\link[easyalluvial]{get_data_space}}
#'@param imp dataframe, with not more then two columns one of them numeric
#'  containing importance measures and one character or factor column containing
#'  corresponding variable names as found in training data.
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, increasing this
#'  number might result in too many flows, Default: 5
#'@param bin_labels labels for the bins from low to high, Default: c("LL", "ML",
#'  "M", "MH", "HH")
#'@param col_vector_flow, character vector, defines flow colours, Default:
#'  c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500')
#'@param method, character vector, one of c('median', 'pdp') \describe{
#'  \item{median}{sets variables that are not displayed to median mode, use with
#'  regular predictions} \item{pdp}{partial dependency plot method, for each
#'  observation in the training data the displayed variableas are set to the
#'  indicated values. The predict function is called for each modified
#'  observation and the result is averaged, calculate predictions using
#'  \code{\link[easyalluvial]{get_pdp_predictions}} } }. Default: 'median'
#'@param params_bin_numeric_pred list, additional parameters passed to
#'  \code{\link[easyalluvial]{manip_bin_numerics}} which is applied to the pred
#'  parameter. Default: list( bins = 5, center = T, transform = T, scale = T)
#'@param pred_train numeric vector, base the automated binning of the pred vector on
#'  the distribution of the training predictions. This is useful if marginal
#'  histograms are added to the plot later. Default = NULL
#'@param force logical, force plotting of over 1500 flows, Default: FALSE
#'@param stratum_label_size numeric, Default: 3.5
#'@param ... additional parameters passed to
#'  \code{\link[easyalluvial]{alluvial_wide}}
#'@return ggplot2 object
#'@details this model visualisation approach follows the "visualising the model
#'  in the dataspace" principle as described in Wickham H, Cook D, Hofmann H
#'  (2015) Visualizing statistical models: Removing the blindfold. Statistical
#'  Analysis and Data Mining 8(4) <doi:10.1002/sam.11271>
#' @examples
#' df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#' m = randomForest::randomForest( disp ~ ., df)
#' imp = m$importance
#' dspace = get_data_space(df, imp, degree = 3)
#' pred = predict(m, newdata = dspace)
#' alluvial_model_response(pred, dspace, imp, degree = 3)
#'
#' # partial dependency plotting method
#' \dontrun{
#'  pred = get_pdp_predictions(df, imp
#'                             , .f_predict = randomForest:::predict.randomForest
#'                             , m
#'                             , degree = 3
#'                             , bins = 5)
#'
#'
#'  alluvial_model_response(pred, dspace, imp, degree = 3, method = 'pdp')
#'  }
#'@seealso \code{\link[easyalluvial]{alluvial_wide}},
#'  \code{\link[easyalluvial]{get_data_space}},
#'  \code{\link[easyalluvial]{alluvial_model_response_caret}}
#'@rdname alluvial_model_response
#'@export
#'@importFrom stringr str_wrap str_replace_all str_split
alluvial_model_response = function(pred, dspace, imp, degree = 4, bins = 5
                                   , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                                   , col_vector_flow = c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500', '#7B5380', '#9DD1D1')
                                   , method = 'median'
                                   , force = FALSE
                                   , params_bin_numeric_pred = list( center = T, transform = T, scale = T)
                                   , pred_train = NULL
                                   , stratum_label_size = 3.5
                                   , ...){

  params = list(pred = pred
                , pred_train = pred_train
                , dspace = dspace
                , imp = imp
                , degree = degree
                , bins = bins
                , bin_labels = bin_labels
                , col_vector_flow = col_vector_flow
                , method = method
                , params_bin_numeric_pred = params_bin_numeric_pred
                , force = force)
  # checks ----------------------------------------------------------------------------

  if( length(bin_labels) != bins & ! bin_labels[1] %in% c('median', 'cuts', 'mean', 'min_max') ){
    stop( "bin_labels length must be equal to bins or one of  c('median', 'cuts', 'mean', 'min_max')")
  }
  
  if( is.factor(pred) ){
    bin_labels = abbreviate( levels(pred), minlength = 1 )
  }
  
  if( ! is.numeric(pred) & ! is.factor(pred) ){
    stop( '"pred" needs to be a numeric or a factor vector')
  }

  if( nrow(dspace) > 1500 & ! force){
    stop( paste('this plot will produce', nrow(dspace), 'flows. More than 1500 flows are not'
                , 'recommended. Keep bins between 3-5 and degrees between 2-4 and use fct_lump()'
                , 'on factors with many levels to reduce the number of flows. Plotting can be'
                , 'forced by setting force = TRUE.') )
  }

  imp = check_imp(imp, dspace)

  degree = check_degree(degree, imp, dspace)

  if( length(pred) != nrow(dspace) ){
    stop('pred needs to be the same length as the number of rows in dspace')
  }

  if( ! method %in% c('median', 'pdp') ){
    stop( paste('parameter method needs to be one of c("median","pdp") instead got:', method) )
  }
  
  if( bins > 7){
    warning('if bins > 7 default colors will be repeated, adjust "col_vector_flow" parameter manually')
  }
  
  

  # internal function -------------------------------------------------------------------
  # will be applied to each column in df creates a suitable label

  make_level_labels = function(col, df, bin_labels){

    levels( df$pred ) <- paste0( bin_labels, ':')

    labels = df %>%
      select(!! as.name(col), pred) %>%
      count(pred, !! as.name(col)) %>%
      tidyr::complete(!! as.name(col), pred) %>%
      arrange( desc(pred) ) %>%
      mutate( n = ifelse( is.na(n), 0 , n ) ) %>%
      group_by(pred) %>%
      mutate( total = sum(n) ) %>%
      ungroup() %>%
      mutate(perc = n / total ) %>%
      mutate( label = map2_chr(pred, round(perc,2), paste) ) %>%
      group_by( !! as.name(col) ) %>%
      summarise( label = paste( label, collapse = '\n')) %>%
      mutate( label = map2_chr(!! as.name(col), label
                               , function(x,y) paste( c(as.character(x),y), collapse = '\n') ) ) %>%
      .$label

    return( labels)
  }

  # setup input df for alluvial from dspace and apply make_level_labels() function -------------
  # make bins for the prediction either based on pred or pred_train if supplied
  
  # setup input df for alluvial plot from dspace ----------------
  df = dspace %>%
    mutate_if( is.factor, fct_drop ) %>%
    mutate_all( as.factor ) %>%
    mutate( pred = pred )
  
  # prepare bins for numerical pred ----------------------------
  
  if( is.numeric(pred) ){
  
    if( is_null(pred_train) ){
      pred_train = pred
    }
    
    new_cuts = do.call( get_cuts, c(from = list(pred_train), target = list(pred)
                                    , params_bin_numeric_pred, bins = bins) )
    
    params$new_cuts = new_cuts
  
    df = df %>%
      manip_bin_numerics( bins = new_cuts, bin_labels = 'cuts'
                          , scale = F, center = F, transform = F)
    
    # create new label for response variable -----------------------------
    
    new_levels =  tibble( lvl = levels(df$pred)
                          , prefix = bin_labels ) %>%
      mutate( new = map2_chr( prefix, lvl, function(x,y) paste0(x,'\n',y) ) ) %>%
      .$new
    
    levels(df$pred) <- new_levels
    
    
  }
    
  # create new factor labels for variables --------------------------------
  
  for(col in names(dspace[0:degree]) ){

    labels = make_level_labels(col, df, bin_labels)

    levels( df[[col]] ) <-  labels

  }

  

  # create alluvial plot ---------------------------------------------------

  p = select(df, pred, one_of( names( dspace[0:degree] ) ) ) %>%
    alluvial_wide( fill_by = 'first_variable'
                   , col_vector_flow = col_vector_flow
                   , colorful_fill_variable_stratum = T
                   , stratum_label_size = stratum_label_size
                   , ... )

  # add info to plot---------------------------------------------------------


  percent_imp = imp %>%
    arrange( desc(imp) ) %>%
    mutate( cum_imp = cumsum(imp )
            , cum_imp_perc = cum_imp / max(cum_imp) ) %>%
    .$cum_imp_perc %>%
    .[degree]

  subtitle = paste('Presented Variables account for', round( percent_imp * 100, 1)
                   , '% of Variable Importance')

  if(method == 'median'){

    title = 'Model Response Plot'

    if(ncol(dspace) > degree){

      others = select(dspace, - one_of( names( dspace[0:degree] ) ) ) %>%
        mutate_if( is.numeric, round, 3) %>%
        mutate_all( as.character ) %>%
        distinct() %>%
        gather( key = 'variable', value = 'value') %>%
        mutate( label = map2_chr(variable, value, function(x,y) paste0(x ,': ', y) ) ) %>%
        summarise( label = paste(label, collapse = '; ') ) %>%
        .$label

      caption = paste( 'Variables not shown have been set to median or mode:', others) %>%
        str_wrap( width = 180 )

    }else{
      caption = ''
    }

  } else{

    title = 'Mean Model Response Plot'
    caption = 'the indicated variables have been set to the indicated values for each
    observation in the data set and model response has been averaged' %>%
      str_wrap( width = 180 )

  }

  p = p +
    labs(title = title, subtitle = subtitle, caption = caption)
  
  p$alluvial_type = 'model_response'
  p$alluvial_params = c(p$alluvial_params[! names(p$alluvial_params) %in% names(params)], params)
  
  return(p)

}


#'@title create model response plot for caret models
#'@description Wraps \code{\link[easyalluvial]{alluvial_model_response}} and
#'  \code{\link[easyalluvial]{get_data_space}} into one call for caret models.
#'@param train caret train object
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, increasing this
#'  number might result in too many flows, Default: 5
#'@param bin_labels labels for the bins from low to high, Default: c("LL", "ML",
#'  "M", "MH", "HH")
#'@param col_vector_flow, character vector, defines flow colours, Default:
#'  c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500')
#'@param method, character vector, one of c('median', 'pdp') \describe{
#'  \item{median}{sets variables that are not displayed to median mode, use with
#'  regular predictions} \item{pdp}{partial dependency plot method, for each
#'  observation in the training data the displayed variableas are set to the
#'  indicated values. The predict function is called for each modified
#'  observation and the result is averaged} }. Default: 'median'
#'@param params_bin_numeric_pred list, additional parameters passed to
#'  \code{\link[easyalluvial]{manip_bin_numerics}} which is applied to the pred
#'  parameter. Default: list( bins = 5, center = T, transform = T, scale = T)
#'@param force logical, force plotting of over 1500 flows, Default: FALSE
#'@param pred_train numeric vector, base the automated binning of the pred vector on
#'  the distribution of the training predictions. This is useful if marginal
#'  histograms are added to the plot later. Default = NULL
#'@param stratum_label_size numeric, Default: 3.5
#'@param ... additional parameters passed to
#'  \code{\link[easyalluvial]{alluvial_wide}}
#'@return ggplot2 object
#'@details this model visualisation approach follows the "visualising the model
#'  in the dataspace" principle as described in Wickham H, Cook D, Hofmann H
#'  (2015) Visualizing statistical models: Removing the blindfold. Statistical
#'  Analysis and Data Mining 8(4) <doi:10.1002/sam.11271>
#' @examples
#' df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#'
#' train = caret::train( disp ~ .
#'                      , df
#'                      , method = 'rf'
#'                      , trControl = caret::trainControl( method = 'none' )
#'                      , importance = TRUE )
#'
#' alluvial_model_response_caret(train, degree = 3)
#'
#' # partial dependency plotting method
#' \dontrun{
#' alluvial_model_response_caret(train, degree = 3, method = 'pdp')
#'  }
#'@seealso \code{\link[easyalluvial]{alluvial_wide}},
#'  \code{\link[easyalluvial]{get_data_space}}, \code{\link[caret]{varImp}},
#'  \code{\link[caret]{extractPrediction}},
#'  \code{\link[easyalluvial]{get_data_space}},
#'  \code{\link[easyalluvial]{get_pdp_predictions}}
#'@rdname alluvial_model_response_caret
#'@export
#'@importFrom caret varImp predict.train
alluvial_model_response_caret = function(train, degree = 4, bins = 5
                                         , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                                         , col_vector_flow = c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500', '#7B5380', '#9DD1D1')
                                         , method = 'median'
                                         , params_bin_numeric_pred = list( center = T, transform = T, scale = T)
                                         , pred_train = NULL
                                         , stratum_label_size = 3.5
                                         , force = F, ...){

  
  if( ! 'train' %in% class(train) ){
    stop( paste( 'train needs to be of class "train" instead got object of class'
                 , paste( class(train), collapse = ', ' ) ) )
  }

  if( ! method %in% c('median', 'pdp') ){
    stop( paste('parameter method needs to be one of c("median","pdp") instead got:', method) )
  }


  imp = caret::varImp( train )
  imp = imp$importance
  
  # for categorical response imp is calculated for each value
  # and has is own column in imp. In this case we average them
  
  imp_df = tibble( var = row.names(imp)
                   , imp = apply(imp, 1, sum) / ncol(imp) )
  

  dspace = get_data_space(train$trainingData, imp_df, degree = degree, bins = bins)

  if( method == 'median'){

    pred = caret::predict.train(train, newdata = dspace)
  }

  if( method == 'pdp'){

    pred = get_pdp_predictions(train$trainingData, imp_df
                               , .f_predict = caret::predict.train
                               , m = train
                               , degree = degree
                               , bins = bins)

  }

  p = alluvial_model_response(pred = pred
                              , dspace = dspace
                              , imp = imp_df
                              , degree = degree
                              , bins = bins
                              , bin_labels = bin_labels
                              , col_vector_flow = col_vector_flow
                              , method = method
                              , params_bin_numeric_pred = params_bin_numeric_pred
                              , force = force
                              , pred_train = pred_train
                              , stratum_label_size = stratum_label_size
                              , ... )

  
  return(p)
}
