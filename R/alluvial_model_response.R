
check_degree = function(degree, imp, df){
  
  if( degree > nrow(imp) ){
    degree = nrow(imp)
    warning('degree higher than number of important variables, degrees adjusted')
  }
  
  return(degree)
}

check_imp = function(imp, df){
  
  if( ! "data.frame" %in% class(imp) & ! 'matrix' %in% class(imp) ){
    stop( paste('imp needs to be of class "data.frame" instead passed object of class'
                , paste( class(imp), collapse = ', ' ) ) )
  }
  
  if( ncol(imp) > 2 ){
    stop('imp must not have more than 2 columns')
  }
  
  if( ncol(imp) == 2 ){
    imp = imp %>%
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
    summarise( imp = sum(imp) ) %>%
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param imp PARAM_DESCRIPTION
#' @param degree PARAM_DESCRIPTION, Default: 3
#' @param bins PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_data_space
#' @export 
get_data_space = function(df,imp, degree = 3, bins = 5){
  
  imp = check_imp(imp, df)
  
  degree = check_degree(degree, imp, df)
  
  imp = arrange(imp, desc(imp) )
  
  imp_top = imp[1:degree,]
  
  df_top = select(df, one_of(imp_top$vars) )
  
  numerics_top = names( select_if( df_top, is.numeric ) )
  
  df_facs = manip_bin_numerics(df_top, bin_labels = 'median', bins = bins) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate_at( vars(one_of(numerics_top)), function(x) as.numeric( as.character(x)) ) %>%
    select(-n) %>%
    mutate_if( is.factor, fct_lump, n = bins )
  
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pred PARAM_DESCRIPTION
#' @param dspace PARAM_DESCRIPTION
#' @param imp PARAM_DESCRIPTION
#' @param degree PARAM_DESCRIPTION, Default: 3
#' @param bins PARAM_DESCRIPTION, Default: 5
#' @param force PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[easyalluvial]{alluvial_wide}}
#' @rdname alluvial_model_response
#' @export 
#' @importFrom stringr str_wrap
alluvial_model_response = function(pred, dspace, imp, degree = 3, bins = 5, force = FALSE){
  
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
  
  make_level_labels = function(col, df){
    
    levels( df$pred ) <- c('LL:','ML:',' M:','MH:','HH:')
    
    labels = df %>%
      select(!! as.name(col), pred) %>%
      # arrange( desc( !!as.name(col) ) ) %>%
      count(pred, !! as.name(col)) %>%
      arrange( desc(pred) ) %>%
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
  
  df = dspace %>%
    mutate_if( is.factor, fct_drop ) %>%
    mutate_all( as.factor ) %>%
    mutate( pred = pred ) %>%
    manip_bin_numerics( bin_labels = 'min_max' )

  # create new factor labels for variables
  for(col in names(dspace[0:degree]) ){
    
    labels = make_level_labels(col, df)
    
    levels( df[[col]] ) <-  labels 
    
  }
  
  # create new label for response variable
  
  new_levels =  tibble( lvl = levels(df$pred)
                        , prefix = c('LL','ML',' M','MH','HH') ) %>%
    mutate( new = map2_chr( prefix, lvl, function(x,y) paste0(x,'\n',y) ) ) %>%
    .$new
  
  levels(df$pred) <- new_levels
  
  p = select(df, pred, one_of( names( dspace[0:degree] ) ) ) %>%
    easyalluvial::alluvial_wide( fill_by = 'first_variable')
  
  # add info -----------------------------
  
  title = 'Model Response Plot'
  
  percent_imp = imp %>%
    arrange( desc(imp) ) %>%
    mutate( cum_imp = cumsum(imp )
            , cum_imp_perc = cum_imp / max(cum_imp) ) %>%
    .$cum_imp_perc %>%
    .[degree]
  
  subtitle = paste('Presented Variables account for', round( percent_imp * 100, 1)
                   , '% of Variable Importance')  
  
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
    
  p = p +
    labs(title = title, subtitle = subtitle, caption = caption)

  return(p)
  
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param train PARAM_DESCRIPTION
#' @param degree PARAM_DESCRIPTION, Default: 3
#' @param bins PARAM_DESCRIPTION, Default: 5
#' @param force PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[caret]{varImp}},\code{\link[caret]{extractPrediction}}
#' @rdname alluvial_model_response_caret
#' @export 
#' @importFrom caret varImp predict.train
alluvial_model_response_caret = function(train, degree = 3, bins = 5, force = F){
  
  imp = caret::varImp( train )
  imp = imp$importance
  dspace = get_data_space(train$trainingData, imp, degree = degree, bins = bins)
  pred = caret::predict.train(train, newdata = dspace)
  p = alluvial_model_response(pred, dspace, imp, degree, bins, force)
  return(p)
}
