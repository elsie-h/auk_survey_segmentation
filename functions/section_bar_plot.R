section_bar_plot <- function(data, 
                             var_section,
                             cluster_section = NULL,
                             adherence_group,
                             folder,
                             path = results_path) {
  
  if (length(adherence_group) > 1 && !is.null(cluster_section))
    stop('Cannot plot more than one adherence groups and a cluster_section.')
  
  if (adherence_group %in% 'no_preventer' && var_section %in% c('D1', 'D3', 'D4')) {
    print(str_c('No data for no_preventer group in section ', var_section))
  } else {
    if (length(adherence_group) == 3) {
      filename <- str_c('allvars_bar_all.png')
    } else {
      filename <- str_c('allvars_bar_',adherence_group ,'.png')
    }
    
    # define adherence levs
    adherence_levs <- c(good = 'Good preventer-type adherence',
                        poor = 'Poor preventer-type adherence',
                        no_preventer = 'No preventer-type inhaler')
    adherence_levs <- adherence_levs[adherence_group]
    
    data <- data %>%
      filter(adherence %in% adherence_levs) 
    
    if (length(adherence_levs) == 1) {
      title <- adherence_levs
      subtitle <- str_c('Section ', cluster_section, ' clusters')
      facet_var <- 'cluster'
    } else {
      title <- NULL
      subtitle <- NULL
      facet_var <- 'adherence'
    }
    
    tmp_adherence <- adherence_group
    adherence_group <- levels(data$adherence)
    adherence_levs <- str_replace(adherence_levs, ' adherence', '\nadherence')
    adherence_levs <- str_replace(adherence_levs, ' inhaler', '\ninhaler')
    
    # folder for results
    res_path <- file.path(path, folder, var_section)
    if (!dir.exists(res_path)) dir.create(res_path)
    
    # survey variables
    name_levs <- survey_key$variable_label[survey_key$section == var_section]
    name_levs <- rev(name_levs)
    
    if (var_section=='G') name_levs <- name_levs[!(str_detect(name_levs, 'reason'))]
    
    # check that there is data
    nrow_data <- data %>%
      filter_at(vars(all_of(name_levs)), any_vars(!(. == 'NA'))) %>%
      nrow()
    if (nrow_data == 0) {
      print(str_c('No data to plot for ', tmp_adherence,' in section ', var_section))
    } else {
      # check whether levels are the same across varaibles
      value_levs <- levels(data[,name_levs[1]][[1]])[1:5]
      same <- all(sapply(name_levs, function(x) all(levels(data[,x][[1]])[1:5] == value_levs)))
      if (same) value_levs <- c(value_levs, 'skip', 'NA') else value_levs <- c(as.character(1:5), 'skip', 'NA')
      
      plot_data <- data %>%
        select(RecordNo, adherence, cluster, all_of(name_levs)) %>%
        mutate_at('adherence', factor, levels = adherence_group,  labels = adherence_levs) %>%
        mutate_at(vars(4:ncol(.)),
                  list(~ factor(as.numeric(.),
                                levels = 1:7,
                                labels = value_levs))) %>%
        pivot_longer(cols = 4:ncol(.)) %>%
        group_by(adherence, cluster, name, value) %>%
        count() %>%
        ungroup(value) %>% 
        mutate(total = sum(n),
               percent = 100*n/sum(n)) %>%
        ungroup() %>%
        filter(!(value %in% 'NA')) 
      
      cluster_labs <- plot_data %>%
        distinct(cluster, total) %>%
        mutate(cluster_lab = str_c(cluster, '\n(n = ', total, ')'))
      
      plot_data <- plot_data %>%  
        mutate_at('name', factor, levels = name_levs) %>%
        mutate_at('cluster', list(~ factor(.,
                                           levels = cluster_labs$cluster,
                                           labels = cluster_labs$cluster_lab)))
      
      p <- plot_data %>%
        ggplot(aes(x = percent, y = name, fill = reorder(value, -as.numeric(value)))) +
        geom_bar(stat = 'identity') +
        facet_grid(reformulate(facet_var), scales = 'free_x') +
        labs(y = str_c('Section ', var_section),
             caption = 'NA categories removed.') +
        scale_fill_manual(name = 'response',
                          values = c(viridis(5), '#808080', '#DCDCDC'),
                          breaks = levels(plot_data$value)) +
        theme(legend.position = 'bottom',
              panel.background = element_blank(),
              plot.background = element_blank()) +
        guides(fill=guide_legend(ncol=5,
                                 byrow=TRUE,
                                 title.position="top",
                                 title.hjust = 0.5)) 
      
      ggsave(filename = filename,
             plot = p,
             path = res_path,
             width = 16, height = 16, units = 'cm')
    }
  }
}






