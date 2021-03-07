# code as numeric, remove missing category
corr_plot <- function(data, title = NULL, filename = NULL) {
  data_name <- deparse(substitute(data))
  
  kendalls_corr <- function(xvar_string, yvar_string) {
    
    tmp <- data %>%
      dplyr::select(xvar := !! xvar_string,
             yvar := !! yvar_string) %>%
      filter_all(all_vars(!(. %in% c('skip', 'skip_all', 'NA')))) %>%
      mutate_all(as.numeric)
    res <- cor(x = tmp$xvar, 
               y = tmp$yvar,
               method = 'kendall')
    out <- list(corr = res,
                vars = c(xvar_string, yvar_string),
                n = nrow(tmp))
  }
  
  
  feature_names <- names(data)[-which(names(data) %in% 'RecordNo')]
  corr_res <- tibble(xvar = character(),
                     yvar = character(),
                     corr = numeric(),
                     n = numeric())
  for (i in seq_along(feature_names)) {
    len <- length(feature_names)
    for (j in i:len) {
      res <- kendalls_corr(xvar_string = feature_names[i],
                           yvar_string = feature_names[j])
      corr_res <- corr_res %>% 
        add_row(xvar = res[[2]][1],
                yvar = res[[2]][2],
                corr = res[[1]],
                n = res[[3]])
    }
  }
  
  if (is.null(title)) title <- str_replace(str_c('Section ', str_remove(data_name, 'data_')), '_', ' ')
  corr_plot_colours <- c("blue", "white", "red")
  corr_res %>% 
    left_join(distinct(corr_res, xvar) %>%
                mutate(orderx = row_number()),
              by = 'xvar') %>%
    left_join(distinct(corr_res, yvar) %>%
                mutate(ordery = -row_number()),
              by = 'yvar') %>%
    mutate_at(vars(c('xvar', 'yvar')), list(~ str_remove(., '_cat'))) %>%
    ggplot(aes(x = reorder(xvar, orderx), y = reorder(yvar, ordery), 
               fill = corr)) +
    geom_tile() +
    scale_fill_gradient2(low = corr_plot_colours[1], 
                         high = corr_plot_colours[3], 
                         mid = corr_plot_colours[2], 
                         midpoint = 0,
                         limit = c(-1, 1),
                         name = 'correlation') +
    coord_fixed() +
    labs(x = NULL, y = NULL,
         title = title) +
    theme_cowplot(8) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust=0.95,
                                     vjust=0.5)
    ) 
  if (is.null(filename)) filename <- str_c(data_name, '_corrplot')
  ggsave(filename = str_c(filename, '.png'),
         path = file.path(results_path, 'corr_plots'),
         width = 14.5, height = 18, units = 'cm')
}

