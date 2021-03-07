my_line_plot <- function(var,
                         data,
                         folder,
                         section,
                         section_cluster,
                         adherence_group,
                         survey_key_data = survey_key,
                         type='line',
                         remove_NA = 'no',
                         ylim = c(0,100),
                         path = results_path) {
  
  if (!any(remove_NA %in% c('no', 'before_percent', 'after_percent'))) 
    stop('remove_NA not valid')
  
  adherence_group <- c('good', 'poor', 'no_preventer')
  adherence_levs <- c(good = 'Good preventer-type adherence',
                      poor = 'Poor preventer-type adherence',
                      no_preventer = 'No preventer-type inhaler')
  
  colour_var <- data %>%
    distinct(cluster, colour_var)
  
  data <- data %>%
    mutate_at('adherence', factor, levels = adherence_levs)
  
  # get survey label, question and section
  key <- survey_key_data %>% 
    filter(variable_label %in% var) %>%
    unlist() %>% 
    unname()
  
  # split long questions over several lines for plotting
  if (nchar(key[2]) > 80) {
    n <- ceiling(nchar(key[2])/80)
    split <- key[2]
    for (i in 2:n) {
      j <- i-1
      split <- str_split_fixed(split, "(?<=.{75})", n = 2)
      split[j,2] <- str_replace(split[j,2], '\\s', '\n')
    }
    question <- str_c(split, collapse = '')
  } else {
    question <- key[2]
  }
  
  # define caption
  caption_1 <- case_when(folder=='good' ~ 'Good preventer-type adherence',
                         folder=='poor' ~ 'Poor preventer-type adherence',
                         folder=='no_preventer' ~ 'No preventer-type inhaler',
                         TRUE ~ NA_character_)
  if (is.na(caption_1)) {
    caption <- str_c('Section ', key[3], '; ', key[1])
  } else {
    caption <- str_c(caption_1, '; Section ', key[3], '; ', key[1])
  }
  
  caption_NA <- switch(remove_NA,
                       no = NULL,
                       before_percent = 'NA category removed before percentage calculations',
                       after_percent = 'NA category removed after percentage calculations')
  
  if (!is.null(caption_NA)) {
    caption <- str_c(caption, ';\n', caption_NA)
  }
  
  # tuncate long factor labels for plotting
  levs <- levels(data[,var][[1]])
  levs <- str_trunc(levs, side = 'right', width = 35)
  plot_data <- data %>%
    select(RecordNo, adherence, cluster, all_of(var)) %>%
    pivot_longer(cols = all_of(var)) 
  
  if (remove_NA == 'before_percent') {
    plot_data <- plot_data %>%
      filter(!(value %in% 'NA'))
  }
  
  plot_data <- plot_data %>%
    mutate_at('value', list(~ factor(str_trunc(as.character(.), 
                                               side = 'right', 
                                               width = 35),
                                     levels = levs))) %>%
    group_by(adherence, cluster, name, value) %>%
    count() %>%
    ungroup(value) %>%
    mutate(percent = round(100*n/sum(n),2)) %>%
    ungroup() 
  
  if (remove_NA == 'after_percent') {
    plot_data <- plot_data %>%
      filter(!(value %in% 'NA'))
  }
  
  # define xlabels
  xlabs <- levels(plot_data$value)
  # split long labels onto 2 lines
  xlabs_new <- character()
  max_length <- 8
  for (x in xlabs) {
    len_x <- nchar(x)
    if (len_x > 28) {
      x <- str_trunc(x, width = 28, side = 'right')
    }
    if (len_x > max_length) {
      n <- ceiling(nchar(x)/max_length)
      split <- x
      for (i in 2:n) {
        j <- i-1
        split <- str_split_fixed(split, str_c('(?<=.{', max_length, '})'), n = 2)
        split[j,2] <- str_replace(split[j,2], '\\s', '\n')
      }
      x <- str_c(split, collapse = '')
    } 
    xlabs_new <- c(xlabs_new, x)
  }
  
  if (n_distinct(colour_var$colour_var) == 1) {
    # expand data so all possible levels plotted
    keep_clusters <- unique(plot_data$cluster)
    expanded_data <- plot_data %>%
      select(-adherence) %>%
      expand(cluster, name, value) %>%
      filter(cluster %in% keep_clusters)
    
    plot_data <- plot_data %>%
      right_join(expanded_data, by = c('cluster', 'name', 'value')) %>%
      mutate_at(vars(c('n', 'percent')),
                list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
      mutate_at('adherence', 
                list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                        str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                        str_detect(cluster, '^No') ~ adherence_levs[3],
                                        TRUE ~ as.character(.)),
                              levels = adherence_levs))) %>%
      left_join(colour_var, by = 'cluster')
  } else {
    # expand data so all possible levels plotted
    keep_clusters <- unique(plot_data$cluster)
    expanded_data <- plot_data %>%
      expand(adherence, cluster, name, value) %>%
      filter(cluster %in% keep_clusters)
    
    plot_data <- plot_data %>%
      right_join(expanded_data, by = c('adherence', 'cluster', 'name', 'value')) %>%
      mutate_at(vars(c('n', 'percent')),
                list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
      mutate_at('adherence', 
                list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                        str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                        str_detect(cluster, '^No') ~ adherence_levs[3],
                                        TRUE ~ as.character(.)),
                              levels = adherence_levs))) %>%
      left_join(colour_var, by = 'cluster')
  }
  
  
  
  if (remove_NA != 'no') {
    plot_data <- plot_data %>%
      filter(!(value %in% 'NA'))
  }
  
  if (nrow(plot_data) == 0) {
    print(str_c('No non-NA data for ', var))
  } else {
    # folder for results
    res_path <- file.path(path, folder, section)
    if (!dir.exists(res_path)) dir.create(res_path)
    
    plot_data <- plot_data %>%
      mutate_at('value', as.numeric)
    
    if (type == 'line') {
      
      if (n_distinct(colour_var$colour_var) == 1) {
        p <- plot_data %>%
          ggplot(aes(x = value, y = percent, 
                     colour = colour_var,
                     linetype = adherence)) +
          geom_line(size=1, alpha=0.8, colour = 'black') +
          geom_point(size=2, alpha=0.8, colour = 'black') +
          guides(colour = 'none',
                 linetype = guide_legend(nrow=3,
                                         title.position="top"))
      } else {
        p <- plot_data %>%
          ggplot(aes(x = value, y = percent, 
                     colour = colour_var,
                     linetype = adherence)) +
          geom_line(size=1, alpha=0.8) +
          geom_point(size=2, alpha=0.8)  +
          scale_colour_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
          guides(colour= guide_legend(nrow=3,
                                      title.position="top"),
                 linetype = guide_legend(nrow=3,
                                         title.position="top"))
      }
      
      p <- p +
        scale_linetype_discrete(name = 'Adherence') +
        scale_x_continuous(breaks = seq(1,length(xlabs),1),
                           labels = xlabs_new) 
      
      xlab <- NULL
    } else if (type == 'bar') {
      if (n_distinct(colour_var$colour_var) == 1) {
        p <- plot_data %>%
          ggplot(aes(x = value, y = percent, fill = adherence)) +
          geom_bar(stat='identity', position = 'dodge') +
          scale_fill_grey(name =  'Adherence') +
          guides(fill=guide_legend(nrow=3,
                                   title.position="top")) +
          scale_x_continuous(breaks = seq(1,length(xlabs),1),
                             labels = xlabs_new) 
        plot_height <- 16
      } else {
        p <- plot_data %>%
          ggplot(aes(x = value, y = percent, fill = colour_var)) +
          geom_bar(stat='identity', position = 'dodge') +
          facet_wrap(~adherence, ncol = 1) +
          scale_fill_viridis_d(name =  str_c('Section ', section_cluster, ' cluster')) +
          guides(fill=guide_legend(nrow=1,
                                   title.position="top")) +
          scale_x_continuous(breaks = seq(1,length(xlabs),1),
                             labels = xlabs_new)
        plot_height <- 22
      }
      xlab <- NULL
    }
    p <- p  +
      theme(legend.position = 'bottom',
            panel.background = element_blank(),
            text = element_text(size=16),
            plot.title = element_text(size = 18, face = 'bold')) +
      lims(y = ylim) +
      labs(x=xlab,
           subtitle = question,
           caption = caption) 
    
    if (type == 'line') {
      p <- p + theme(legend.key.width = unit(1.8,'cm'))
      plot_height <- 16
    }
    ggsave(filename = str_c(var, '.png'),
           plot = p,
           path = res_path,
           width = 25, height = plot_height, units = 'cm')
  }
}