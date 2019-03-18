
plot_imp = function(p, data_input){
  
  imp = p$alluvial_params$imp
  
  all_vars = p$data$x %>%
    levels() %>%
    .[ ! . == 'pred']
  
  imp_df = check_imp(imp, data_input) %>%
    mutate( perc = imp/ sum(imp)
            , plotted = ifelse( vars %in% all_vars, 'y', 'n') )
  
  perc_total_plotted = imp_df %>%
    filter(plotted == 'y') %>%
    .$perc %>%
    sum()
  
  imp_df = imp_df %>%
    bind_rows( tibble( vars = 'total\nalluvial'
                       , perc = perc_total_plotted
                       , plotted = 'y') ) %>%
    mutate( vars = as_factor(vars)
            , vars = fct_relevel(vars, 'total\nalluvial')
            , vars = fct_rev(vars) )
  
  
  
  ggplot(imp_df, aes_string('vars', 'perc', fill = 'plotted')) +
    geom_col( color = 'grey'
              , show.legend = F
              , size = 1) +
    scale_fill_manual( values = c('white', 'grey') ) +
    coord_flip() +
    theme_minimal() +
    labs( x = '', y = 'Percent Importance') +
    scale_y_continuous( position = 'right', limits = c(0,1) ) +
    geom_text( aes( label = round(perc,2))
               , nudge_y = 0.025)
  
}