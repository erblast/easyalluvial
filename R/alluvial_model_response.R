
check_degree = function(degree, imp, df){

  if( degree > nrow(imp) ){
    degree = nrow(imp)
    warning('degree higher than number of important variables, degrees adjusted')
  }

  return(degree)
}

#' @title tidy up dataframe containing model feature importance
#' @description returns dataframe with exactly two columns, vars and imp and
#'   aggregates dummy encoded variables. Helper function called by all functions
#'   that take an imp parameter. Can be called manually if formula for
#'   aggregating dummy encoded variables must be modified.
#' @param imp dataframe or matrix with feature importance information
#' @param df dataframe, modelling training data
#' @param .f window function, Default: max
#' @return dataframe \describe{ \item{vars}{character column with feature names}
#'   \item{imp}{numerical column, importance values} }
#' @examples
#' # randomforest
#' df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#' m = randomForest::randomForest( disp ~ ., df)
#' imp = m$importance
#' tidy_imp(imp, mtcars2)
#' 
#' @rdname tidy_imp
#' @export
tidy_imp = function(imp, df, .f = max){

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

  df_ori_var = tibble( ori_var = names( select_if(df, ~ is.factor(.) | is.character(.) ) ) ) %>%
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
#'  importance of the explanatory variables. It only considers the most
#'  important variables as defined by the degree parameter. It selects a number
#'  (defined by bins) of sensible single values spread over the range of the
#'  numeric variables and creates all possible value combinations among the most
#'  important variables. The values of the remaining variables are set to
#'  mode(factors) or median(numerics).
#'@details It selects a the top most important variables based on the degree
#'  parameter and bins the numeric variables using
#'  \code{\link[easyalluvial]{manip_bin_numerics}}, while leaving categoric
#'  variables unchanged. The number of bins for each numeric variable is set to
#'  bins -2. Next the median is picked for each of the bins and the min and the
#'  max value is added for each numeric variable So that we get { median(bin) X
#'  bins -2, max, min} for each numeric variable. Then all possible combinations
#'  between those values and the  categoric factor levels are created. The total
#'  number of all possible combinations defines the range of the data space. The
#'  values of the remaining variables are set to mode(factors) or
#'  median(numerics).
#'@param df dataframe, training data
#'@param imp dataframe, with not more then two columns one of them numeric
#'  containing importance measures and one character or factor column containing
#'  corresponding variable names as found in training data.
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, and maximum number
#'  of levels for factor variables, increasing this number might result in too
#'  many flows, Default: 5
#'@param max_levels integer, maximum number of levels per factor variable, Default: 10
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
get_data_space = function(df, imp, degree = 4, bins = 5, max_levels = 10){

  degree = check_degree(degree, imp, df)

  imp = tidy_imp(imp, df)

  imp = arrange(imp, desc(imp) )

  imp_top = imp[1:degree,]

  df_top = select(df, one_of(imp_top$vars) )

  numerics_top = names( select_if( df_top, is.numeric ) )
  factors_top = names( select_if( df_top, is.factor) )
  
  # generate warning if number of levels is > max_levels
  if( ! is_empty(factors_top) ){
    
    n_levels = select(df, one_of(factors_top) ) %>%
    summarise_all( ~ length( levels(.) ) ) %>%
    unlist() 
  
    most_levels = n_levels[ which(n_levels == max(n_levels)) ]
  
  
    if(most_levels[1] > max_levels){
      warning( paste('factor', names(most_levels), 'contains', most_levels
                     , 'levels. Data space will only be created for top', max_levels
                     , 'levels. Adjust this behaviour using the `max_levels` parameter ') )
    }
  }
  
  summarise_top = function(x, agg){
    if(is.numeric(x)){
      return( agg(x) )
    }
    else{
      return( x[1] )
    }
  }
  
  # mix factors and numerics in df_top -----------------------------------------------------------
  if( ! is_empty(numerics_top) & ! is_empty(factors_top) ){
    
    df_facs = manip_bin_numerics(df_top, bin_labels = 'median', bins = bins-2) %>%
      mutate_if( is.factor, fct_lump, n = max_levels, other_level = 'easyalluvial_factor_cap' ) %>%
      distinct() %>%
      mutate_at( vars( one_of(numerics_top) ), ~ as.numeric( as.character(.) ) ) %>%
      bind_rows( summarise_all(df_top, summarise_top, agg = max) ) %>%
      bind_rows( summarise_all(df_top, summarise_top, agg = min) ) %>%
      mutate_at( vars( one_of(numerics_top) ), as.factor ) %>%
      tidyr::complete( !!! map( names(df_top) , as.name) ) %>%
      mutate_at( vars(one_of(numerics_top)), function(x) as.numeric( as.character(x)) ) %>%
      filter_at( vars(one_of(factors_top)), ~ . != 'easyalluvial_factor_cap') %>%
      mutate_at(vars(one_of(factors_top)), fct_drop )
  
  # only factors ---------------------------------------------------------------------------------  
  }else if( ! is_empty(factors_top) ){
    
    df_facs = df_top %>%
      mutate_if( is.factor, fct_lump, n = max_levels, other_level = 'easyalluvial_factor_cap' ) %>%
      distinct() %>%
      tidyr::complete( !!! map( names(df_top) , as.name) ) %>%
      filter_at( vars(one_of(factors_top)), ~ . != 'easyalluvial_factor_cap') %>%
      mutate_at(vars(one_of(factors_top)), fct_drop )
    
  }
  
  # make sure levels are the same as in input data ---------------------------------------------
  if(  ! is_empty(factors_top) ){
    for(fac in factors_top){
      missing_levels = levels(df[[fac]])[ ! levels(df[[fac]]) %in% levels(df_facs[[fac]]) ]
      df_facs[[fac]] = fct_expand(df_facs[[fac]], missing_levels)
    }
  }
  
  # only numerics ------------------------------------------------------------------------------
  if( ! is_empty(numerics_top) ){
    
    df_facs = manip_bin_numerics(df_top, bin_labels = 'median', bins = bins-2) %>%
      distinct() %>%
      mutate_at( vars( one_of(numerics_top) ), ~ as.numeric( as.character(.) ) ) %>%
      bind_rows( summarise_all(df_top, summarise_top, agg = max) ) %>%
      bind_rows( summarise_all(df_top, summarise_top, agg = min) ) %>%
      mutate_at( vars( one_of(numerics_top) ), as.factor ) %>%
      tidyr::complete( !!! map( names(df_top) , as.name) ) %>%
      mutate_at( vars(one_of(numerics_top)), function(x) as.numeric( as.character(x)) )
  }
  
  # Add remaining features as mode or median -----------------------------------------
  mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  if( nrow(imp) > degree ){
    imp_rest = imp[(degree + 1):nrow(imp), ]

    df_rest = select(df, one_of(imp_rest$vars) )

    df_rest = df_rest %>%
      mutate_if( is.numeric, median ) %>%
      mutate_if( function(x) is.factor(x) | is.character(x), mode) %>%
      head(1) %>%
      sample_n(nrow(df_facs), replace = T)

    dspace = bind_cols( df_facs, df_rest)

  }else{
    dspace = df_facs
  }

  dspace = select( dspace, one_of( imp$vars[1:degree] ), everything() )

  return(dspace)
}

#'@title get predictions compatible with the partial dependence plotting method
#'@description Alluvial plots are capable of displaying higher dimensional data
#'  on a plane, thus lend themselves to plot the response of a statistical model
#'  to changes in the input data across multiple dimensions. The practical limit
#'  here is 4 dimensions while conventional partial dependence plots are limited
#'  to 2 dimensions.
#'
#'  Briefly the 4 variables with the highest feature importance for a given
#'  model are selected and 5 values spread over the variable range are selected
#'  for each. Then a grid of all possible combinations is created. All
#'  none-plotted variables are set to the values found in the first row of the
#'  training data set. Using this artificial data space model predictions are
#'  being generated. This process is then repeated for each row in the training
#'  data set and the overall model response is averaged in the end. Each of the
#'  possible combinations is plotted as a flow which is coloured by the bin
#'  corresponding to the average model response generated by that particular
#'  combination.
#'
#'@param df dataframe, training data
#'@param imp dataframe, with not more then two columns one of them numeric
#'  containing importance measures and one character or factor column containing
#'  corresponding variable names as found in training data.
#'@param degree integer,  number of top important variables to select. For
#'  plotting more than 4 will result in two many flows and the alluvial plot
#'  will not be very readable, Default: 4
#'@param bins integer, number of bins for numeric variables, increasing this
#'  number might result in too many flows, Default: 5
#'@param .f_predict corresponding model predict() function. Needs to accept `m`
#'  as the first parameter and use the `newdata` parameter. Supply a wrapper for
#'  predict functions with x-y synthax.
#'@param m model object
#'@return vector, predictions
#'@details see https://christophm.github.io/interpretable-ml-book/pdp.html
#' @examples
#'  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#'  m = randomForest::randomForest( disp ~ ., df)
#'  imp = m$importance
#'
#'  pred = get_pdp_predictions(df, imp
#'                             , m
#'                             , degree = 3
#'                             , bins = 5)
#'
#'@seealso \code{\link[progress]{progress_bar}}
#'@rdname get_pdp_predictions
#'@export
#'@importFrom progress progress_bar
get_pdp_predictions = function(df, imp, m, degree = 4, bins = 5, .f_predict = predict){
  
  pb = progress::progress_bar$new(total = nrow(df))
  
  dspace = get_data_space(df, imp, degree, bins)
  
  if(degree == nrow(imp) ){
    return( .f_predict(m, newdata = dspace) )
  }
  
  df_trunc = select(df, one_of( names(dspace)[(degree + 1) : ncol(dspace)] ) )
  
  pred_results = rep(0, nrow(dspace ) )
  
  for( i in seq(1, nrow(df) ) ){
    
    sub_dspace = df_trunc[i,] %>%
      sample_n( nrow(dspace), replace = T )
    
    sub_dspace = dspace[,1:degree] %>%
      bind_cols(sub_dspace)
    
    pred = .f_predict(m, newdata = sub_dspace)
    
    pred = pred * 1/nrow(df)
    
    pred_results = pred_results + pred
    
    pb$tick()
  }
  
  return(pred_results)
}


get_cuts = function( from, target, ... ){

  cuts = levels( manip_bin_numerics(from, bin_labels = 'min_max', ... ) )%>%
    paste( collapse = ',') %>%
    str_replace_all('\\ -\n ', ',') %>%
    str_split(',') %>%
    # map(unique) %>%
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

  imp = tidy_imp(imp, dspace)

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

    pred = predict(train, newdata = dspace)
  }

  if( method == 'pdp'){

    pred = get_pdp_predictions(train$trainingData, imp_df
                               , .f_predict = predict
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

#' @title calls e1071::skewness
#' @description if e1071 is not listed a a dependency I get an error. I assume
#'   caret uses it to calculate feature importance. However, e1071 is not listed
#'   as a caret dependency. I have to add a function that directly calls it, so
#'   I do not get a NOTE from RMD Check on Linux.
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @seealso \code{\link[e1071]{skewness}}
#' @rdname use_e1071
#' @noRd
#' @importFrom e1071 skewness
use_e1071 = function(x){
  e1071::skewness(x)
}