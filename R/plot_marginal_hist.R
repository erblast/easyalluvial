
require(tidyverse)

p = alluvial_wide( data = mtcars2, id = id
                   , max_variables = 5                 
                   , fill_by = 'first_variable' )

p$data_input = mtcars2
p$data_key = p$data_key %>%
  mutate_if( is.factor, fct_drop)

plot_hist = function( var, p){
  
  var_quo = enquo(var)
  var_str = quo_name(var_quo)
  
  is_num = is.numeric( p$data_input[[var_str]] )
  
  p$data_input = p$data_input %>%
    rename( var_num = !! var_quo )
  
  p$data_key = p$data_key %>%
    rename( var_key = !! var_quo )
  
  df = left_join(p$data_input, p$data_key, by = 'id') %>%
    select( starts_with('var') ) 
  
  df_col = p$data %>%
    filter( x == var_str) %>%
    select( value, fill_value) %>%
    distinct()
  
  if( is_num ){
  
    d = density(df$var_num)
    m = randomForest::randomForest(var_key ~ var_num, df)
    pred = predict(m, newdata = tibble(var_num = d$x), type = 'class' )
    
    df_plot = tibble( x = d$x
                      , y = d$y
                      , fill = pred ) %>%
      left_join( df_col, by = c('fill' = 'value') )
    
    p = ggplot(df_plot, aes(x, y, fill = fill_value, color = fill_value) ) +
      geom_ribbon( aes(ymin = 0, ymax = y) ) 
    
  } else{
    df = df %>%
      left_join( df_col, by = c('var_key' = 'value') ) 
    
    p = ggplot(df, aes(var_num, fill = fill_value) ) +
      geom_bar( width = 1/2 ) 
    
  }
  
  p = p +
    scale_fill_identity() +
    scale_color_identity() +
    theme( line = element_blank()
           , axis.text.y = element_blank()
           , axis.title = element_blank()
           , panel.background = element_blank() ) 
  
  return(p)
}

# plot_hist( cyl, p)

add_marginal_histograms = function(p){
  
  vars = levels( p$data$x )
  
  vars = map( vars, as.name )
  
  hists = map( vars, plot_hist, p = p)
  
  p_margin = do.call( gridExtra::grid.arrange, c( hists, nrow = 1) )
  
  p = p +
    labs( y = '', caption = '') +
    coord_cartesian( xlim = c(1.25, length(vars) - 0.25 ) ) +
    theme( axis.text.y = element_blank(), axis.ticks.y = element_blank()
           )
    
  
  p_full = gridExtra::grid.arrange(p, p_margin, ncol = 1
                                   , layout_matrix = as.matrix( data.frame( x = c(1,1,1,1,1,1,1,2) ) ))
  
  p_full = gridExtra::grid.arrange(p_margin, p, ncol = 1
                                   , layout_matrix = as.matrix( data.frame( x = c(1,2,2,2,2,2,2,2) ) ))
  
}


add_marginal_histograms(p)
  
