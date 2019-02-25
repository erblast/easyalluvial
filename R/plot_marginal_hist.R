
apply_cuts = function(x, cuts){
  
  if( min(x) < cuts[1] ){
    cuts[1] <-  min(x) - 1
  }
  
  if( max(x) > cuts[length(cuts)] ){
    cuts[length(cuts)] <-  max(x) + 1
  }
  
  return( cut(x, cuts) )
  
}

plot_hist = function( var, p, data_input, ... ){
  
  p_ori = p
  
  if( p$alluvial_type == 'wide'){
    
    p = plot_hist_wide( var, p_ori, data_input)
    
  }else if( p$alluvial_type == 'model_response'){
    
    p = plot_hist_model_response( var, p_ori, data_input, ...)
    
  } else if(p$alluvial_type == 'long'){
    
    p = plot_hist_long( var, p_ori, data_input)
    
  }else{
    stop('Plot not supported')
  }
  
  p = p +
    scale_fill_identity() +
    scale_color_identity() 
  
  if( p_ori$alluvial_type == 'model_response' & var == 'pred'){
    p = p +
      theme( line = element_blank()
             , axis.title = element_blank()
             , panel.background = element_blank() ) 
  }else {
    p = p +
      theme( line = element_blank()
             , axis.text.y = element_blank()
             , axis.title = element_blank()
             , panel.background = element_blank() ) 
  }
  
  return(p)
}

plot_hist_long = function(var, p, data_input){
  
  is_num = is.numeric(data_input[[p$alluvial_params$value]])
  key_str = as.character(p$alluvial_params$key)
  id_str = as.character(p$alluvial_params$id)
  value_str = as.character(p$alluvial_params$value)
  
  var_order = levels(p$data$value)
  
  data_input[[id_str]] <- as.character( data_input[[id_str]] )
  data_input[[key_str]] <- as.character( data_input[[key_str]] )
  
  df_col = p$data %>%
    select(value, fill_value) %>%
    distinct() %>%
    arrange(value) %>%
    mutate(rwn = row_number() ) %>%
    select( - value )

  suppressMessages({
    df_match = p$data_key %>%
      select( -n, -alluvial_id) %>%
      gather( key = !! as.name(key_str), value = 'bin', - !! as.name(id_str) ) %>%
      mutate( !! as.name(id_str) := as.character(!! as.name(id_str) )
              , !! as.name(key_str) := as.character(!! as.name(key_str) )) %>%
      left_join( select(data_input, one_of( c(key_str, value_str, id_str) ) ) )  %>%
      rename( value = !! as.name(value_str) ) %>%
      mutate( bin = as.factor(bin)
              , bin = fct_relevel(bin, var_order) )
  })
  
  m = randomForest::randomForest(bin ~ value, df_match)
  
  df_filt = data_input %>%
    filter( !! as.name(key_str) == var )
  
  dens_var = density(df_filt[[value_str]])
  
  df_plot = tibble(x = dens_var$x
                   , y = dens_var$y ) %>%
    mutate( bin = predict(m, tibble( value = x) )
            , rwn = as.integer(bin) ) %>%
    left_join(df_col, by=c('rwn' = 'rwn') )
  
  p = ggplot(df_plot ) +
    geom_ribbon( aes(x, fill = fill_value, color = fill_value, ymin = 0, ymax = y) ) +
    geom_rug( data = df_filt, mapping = aes_string(value_str), sides = 'b')
  
  
}

plot_hist_model_response = function(var, p, data_input, pred_train = NULL, scale = 400){
  # get stratum color from p$data
  # get median values from dspace
  # detect pred original variable 
  
  var_str = var
  var_quo = as.name(var_str)
  
  is_pred = var == 'pred'
  
  if(is_pred){
    ori_name = names(data_input)[ ! names(data_input) %in% names(p$alluvial_params$dspace) ] %>% unlist()
    
    if( length(ori_name) > 1){
      stop( paste('data_input must only contain explanatory and predictive variables, found:', paste( ori_name, collapse = ',') ) )
    }
    
    is_num = is.numeric( data_input[[ori_name]] )
  }else{
    is_num = is.numeric( data_input[[var_str]] )
  }
  
  df_col = p$data %>%
    filter( x == var_str ) %>%
    arrange( desc(value) ) %>%
    select(fill_value, value) %>%
    distinct() %>%
    select( fill_value ) %>%
    mutate( fill_value = as_factor(fill_value)
            , rwn = row_number() )
  
  if( is_num & ! is_pred){
    
    df_median = p$alluvial_params$dspace %>%
      select( !! var_quo ) %>%
      distinct() %>%
      arrange(!! var_quo) %>%
      bind_cols(df_col)
    
    p = ggplot(data_input, aes_string( x = var_str ) ) +
      geom_density( aes( y = ..density..), size = 1 ) +
      geom_rug()
    
    for( i in seq(1, nrow(df_median) ) ){
      p = p +
        geom_vline(xintercept = df_median[[var_str]][i]
                   , color = df_median[['fill_value']][i]
                   , size = 2)
    }
  }
  
  if( ! is_num & ! is_pred){
    
    df_plot = data_input %>%
      select( !! var_quo ) %>%
      mutate( colors = !! var_quo
              , colors = as.integer(colors) ) %>%
      left_join( df_col, by = c( 'colors' = 'rwn') )
    
    p = ggplot(df_plot, aes_string(var_str, fill = 'fill_value') ) +
      geom_bar( width = 1/2 ) 
    
  }
  
  if( is_num & is_pred ){
    
    var_str = ori_name
    
    df_ori = data_input %>%
      select( !! as.name(var_str) ) %>%
      mutate( variable = var_str ) %>%
      rename( value = !! as.name(var_str) )
    
    
    df_pred = tibble( variable = 'pred'
                      , value = p$alluvial_params$pred )
    

    dens_pred = density( df_pred$value )
    dens_ori = density( df_ori$value)
    

    # pred_train -------------------------------------
    
    pred_train_from_plot = ! is_null(p$alluvial_params$pred_train)
    pred_train_as_arg = ! is_null(pred_train)
    
    if(pred_train_from_plot){
      pred_train = p$alluvial_params$pred_train
    }
    
    if(pred_train_from_plot | pred_train_as_arg){
      df_pred_train = tibble( variable = 'pred_train'
                              , x = density(pred_train)$x
                              , y = density(pred_train)$y
                              , colors = 'lightgrey')
    }
    
    # assemple df_plot ----------------------------
    df_plot = tibble( variable = 'pred'
                      , x = dens_pred$x
                      , y = dens_pred$y )
    
    if( pred_train_from_plot){
      
      df_plot = df_plot %>%
        bind_rows( select(df_pred_train, - colors) )
      
    }
    
    # apply cuts and select colors -----------------
    df_plot = df_plot%>%
      mutate( rwn = apply_cuts(x, p$alluvial_params$new_cuts) 
              , rwn = as.integer(rwn) )
    
    min_rwn_pred = p$alluvial_params$pred %>%
      apply_cuts( p$alluvial_params$new_cuts) %>%
      as.integer() %>%
      min()
    
    max_color = max(df_plot$rwn)
    
    df_col = df_col %>%
      mutate( rwn = rwn + min_rwn_pred -1 ) 
    
    unused_colours = p$alluvial_params$col_vector_flow[ ! p$alluvial_params$col_vector_flow %in% df_col$fill_value]
    unused_rwn = seq(1,max_color)
    unused_rwn = unused_rwn[! unused_rwn %in% df_col$rwn]
    
    n_missing_colors = length(unused_rwn) - length(unused_colours)
    n_missing_colors = ifelse(n_missing_colors < 0, 0, n_missing_colors)
    
    unused_colours = c( unused_colours, rep('grey', n_missing_colors) )
    
    df_col = df_col %>%
      mutate( fill_value = as.character(fill_value) ) %>%
      bind_rows( tibble(rwn = unused_rwn, fill_value = unused_colours[0:length( unused_rwn) ]) )
      
    df_plot = df_plot %>%
      left_join( df_col, by = c( 'rwn' = 'rwn') ) %>%
      rename( colors = fill_value ) %>%
      mutate( colors = as.character(colors) ) %>%
      bind_rows( tibble(variable = var_str
                        , x = dens_ori$x
                        , y = dens_ori$y
                        , colors = 'lightgrey') )
    
      
    if( pred_train_as_arg & (! pred_train_from_plot) ){
      df_plot = df_plot %>%
        bind_rows( df_pred_train )
    }
    
    # adjust order of variables on y-axis ----------------------------

    if( is_null(pred_train) ){
      df_plot = mutate(df_plot, variable = fct_relevel(variable, 'pred') )
    }else{
      df_plot = mutate(df_plot, variable = fct_relevel(variable, 'pred', 'pred_train') )
    }

  # plot ---------------------------------------------------------------
    
    p = ggplot(df_plot) +
      ggridges::geom_ridgeline_gradient(aes(x = x
                                            , y = variable
                                            , height = y
                                            , fill = colors
                                            , group = variable )
                              , scale = scale) +
      geom_rug( aes(x = value), data = df_pred ) +
      geom_rug( aes(x = value), data = df_ori, sides = 't' )
    
  }
  
  return(p)
}

plot_hist_wide = function( var, p, data_input){
  # use ID to join data_key with data_input to
  # have sratum name combined with original numeric value
  # get stratum color from p$data
  # use density() to get x,y density coordinate
  # train model on stratum_name (key) and original numeric value
  # apply model to x density coordinates to get segment borders
  
  var_str = var
  var_quo = as.name(var_str)
  
  is_num = is.numeric( data_input[[var_str]] )
  
  data_input = data_input %>%
    rename( var_num = !! var_quo )

  p$data_key = p$data_key %>%
    rename( var_key = !! var_quo )
  
  if( is_null(p$alluvial_params$id) ){
    p$alluvial_params$id = 'ID'
    
    data_input = data_input %>%
      mutate( ID = as.character(row_number() ) )
  }
  
  df = left_join(data_input, p$data_key, by = p$alluvial_params$id) %>%
    select( starts_with('var') ) 
  
  df_col = p$data %>%
    filter( x == var_str) %>%
    select( value, fill_value) %>%
    distinct() %>%
  mutate_if( is.factor, as.character )
  
  df_rug = data_input %>% 
    select(var_num) %>% 
    rename( x = var_num) %>%
    mutate( y = 0)
  
  
  if( is_num ){
  
    d = density(df$var_num)
    m = randomForest::randomForest(var_key ~ var_num, df)
    pred = predict(m, newdata = tibble(var_num = d$x), type = 'class' )
    
    df_plot = tibble( x = d$x
                      , y = d$y
                      , fill = as.character(pred) ) %>%
      left_join( df_col, by = c('fill' = 'value') )
    

    p = ggplot(df_plot ) +
      geom_ribbon( aes(x, fill = fill_value, color = fill_value, ymin = 0, ymax = y) ) +
      geom_rug( data = df_rug, mapping = aes(x), sides = 'b')
    

  } else{
    df = df %>%
      mutate_if( is.factor, as.character ) %>%
      left_join( df_col, by = c('var_key' = 'value') ) 
    
    p = ggplot(df, aes(var_num, fill = fill_value) ) +
      geom_bar( width = 1/2 ) 
  }
    
  return(p)
}


add_marginal_histograms = function(p, data_input, top = TRUE, keep_labels = FALSE, ...){
  
  vars = levels( p$data$x )

  hists = map( vars, plot_hist, p = p, data_input = data_input, ...)
  
  p_margin = do.call( gridExtra::grid.arrange, c( hists, nrow = 1) )
  
  if(keep_labels){
    params = list( top = paste( p$labels$title, '-', p$labels$subtitle ), bottom = p$labels$caption  )
  }else{
    params = list( top = NULL, bottom = NULL)
  }
  
  p = p +
    labs( y = '', caption = '', title = '', subtitle = '') +
    coord_cartesian( xlim = c(1.25, length(vars) - 0.25 ) ) +
    theme( axis.text.y = element_blank(), axis.ticks.y = element_blank()
           )
    
  if(! top){
    layout = as.matrix( data.frame( x = c(1,1,1,1,1,1,1,2) ) )
    
    p_full = do.call( gridExtra::grid.arrange, c( list(p, p_margin) , ncol = 1, params
                                                  , layout_matrix = list(layout) ) )
    
  }else{

    layout = as.matrix( data.frame( x = c(1,2,2,2,2,2,2,2) ) )
    
    p_full = do.call( gridExtra::grid.arrange, c( list(p_margin, p) , ncol = 1, params
                                                  , layout_matrix = list(layout) ) )
    
  }
  
}



