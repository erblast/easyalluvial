

get_data_space = function(df,imp, degree = 3, bins = 5){
  
  imp_top = imp[1:degree,]
  imp_rest = imp[(degree + 1):nrow(imp), ]
  
  df_top = select(df, one_of(imp_top$vars) )
  df_rest = select(df, - one_of(imp_top$vars) )
  
  numerics_top = names( select_if( df_top, is.numeric ) )
  
  df_facs = manip_bin_numerics(df_top, bin_labels = 'median') %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate_at( vars(one_of(numerics_top)), function(x) as.numeric( as.character(x)) ) %>%
    select(-n)
  
  mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  df_rest = df_rest %>%
    mutate_if( is.numeric, median ) %>%
    mutate_if( function(x) is.factor(x) | is.character(x), mode) %>%
    head(1) %>%
    sample_n(nrow(df_facs), replace = T)
  
  dspace = bind_cols( df_facs, df_rest)
}



alluvial_model_response = function(pred, dspace, imp, degree = 3, bins = 5){
  
  
  make_level_labels = function(col, df_data_key){
    
    df_data_key = df_data_key %>%
      mutate_if( is.factor, fct_drop )
    
    levels( df_data_key$pred ) <- rev( c('LL:','ML:',' M:','MH:','HH:') )
    
    labels = df_data_key %>%
      select(!! as.name(col), pred) %>%
      count(pred, !! as.name(col)) %>%
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
    
    return(labels)
  }
  
  df = dspace %>%
    mutate_all( as.factor ) %>%
    mutate( pred = pred ) %>%
    manip_bin_numerics( bin_labels = 'min_max' )

  # create new factor labels for variables
  for(col in names(dspace[0:degree]) ){
    
    labels = make_level_labels(col, df)
    
    levels( df[[col]] ) <- rev( labels )
    
  }
  
  # creat new label for response variable
  
  new_levels =  tibble( lvl = levels(df$pred)
                        , prefix = c('LL','ML',' M','MH','HH') ) %>%
    mutate( new = map2_chr( prefix, lvl, function(x,y) paste0(x,'\n',y) ) ) %>%
    .$new
  
  levels(df$pred) <- new_levels
  
  p = select(df, pred, one_of( names( dspace[0:degree] ) ) ) %>%
    easyalluvial::alluvial_wide( fill_by = 'first_variable')

  return(p)
  
}
