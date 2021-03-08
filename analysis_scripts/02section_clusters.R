# Identify section-specific clusters (see report for details)
# Plot a summary of the section variables each of the section clusters
# Store the section clusters in a dataset for later use

# load data
likert_list <- readRDS(file = file.path(data_path, 'likert_list.RDS'))
data_clean <- readRDS(file = file.path(data_path, 'data_clean.RDS'))
survey_key <- readRDS(file = file.path(data_path, 'survey_key.RDS'))

source(file.path(functions_path, 'combine_likert.R'))
source(file.path(functions_path, 'my_line_plot.R'))

sections <- sort(unique(survey_key$section))

likert_sections <- c('B1', 'D1', 'D2', 'E2', 'F2')

# prepare dataset for storing all clusters
data_all_clusters <- data_clean %>% select(RecordNo, adherence)

  # loop over all sections
  for (sec in likert_sections) {
    
    vars_section <- survey_key %>% 
      filter(section %in% sec) %>%
      select(variable_label) %>%
      unlist() %>% 
      unname()
    
    data_a <-  data_clean %>%
      filter_at(vars( all_of(vars_section)),
                any_vars(!(. %in% c('NA', 'skip')))) %>%
      select(RecordNo, all_of(vars_section))
    
    cluster_name <- str_c(sec, '_cluster')
    
    # if sample size < 100, don't cluster
    if (nrow(data_a) < 100) {
      data_all_clusters <- data_all_clusters %>%
        left_join(data_clusters %>%
                    select(RecordNo) %>%
                    mutate(!! cluster_name := 'sample size too small'),
                  by = 'RecordNo')
      next
    }
    
    
    # MCA to reduce to 2 dimensions
    # the choice of 2 dimensions is based on exploratory analysis not included in this repository
    mca_data <- combine_likert(data = data_a %>%
                                 mutate_at(vars(all_of(vars_section)), as.character),
                               vars = vars_section)
    
    mca_res <- MCA(mca_data %>%
                     dplyr::select(-RecordNo), 
                   ncp = 2, 
                   graph = FALSE)
    
    # calculate MCA scores
    scores <- as_tibble(mca_res$ind$coord)
    # calculate squared Euclidean distances
    d <- dist(x = scores)^2
    
    # clusterboot
    cluster_res_list <- list()
    cluster_res_list[[1]] <- NA_real_
    sil_res <- numeric()
    sil_res[1] <- NA_real_
    for (x in 3) {
      clusterboot_res <- clusterboot(data = scores,
                                     B=100,
                                     bootmethod = 'boot',
                                     clustermethod = kmeansCBI,
                                     k = x,
                                     nstart = 25,
                                     iter.max = 25,
                                     dissolution = 0.5,
                                     recover = 0.75,
                                     seed = seed)
      # calculate silhouette widths if all three clusters have average Jaccard coefficient > 0.85
      if (all(clusterboot_res$bootmean > 0.85)) {
        cluster_res_list[[x]] <- clusterboot_res$result$result$cluster
        sil_res[x]  <- summary(silhouette(x = clusterboot_res$result$result$cluster, dist = d))$avg.width
      } else {
        sil_res[x] <- NA_real_
        cluster_res_list[[x]] <- NA_real_
      }
    }
    # if average silhouette widths >0.5 store section clusters
    if (max(sil_res, na.rm=TRUE) > 0.5) {
      k <- which(sil_res ==  max(sil_res, na.rm=TRUE))
      data_clusters <- data_a %>%
        mutate(cluster = factor(cluster_res_list[[k]])) %>%
        left_join(data_clean %>%
                    select(RecordNo, adherence), 
                  by = 'RecordNo')
      
      # save the clusters
      data_all_clusters <- data_all_clusters %>%
        left_join(data_clusters %>%
                    select(RecordNo, !! cluster_name := cluster),
                  by = 'RecordNo')
      
      plot_data <- data_clusters %>%
        select(RecordNo, cluster, all_of(vars_section)) %>%
        pivot_longer(cols = 3:ncol(.)) %>%
        group_by(cluster, name, value) %>%
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
        mutate_at('cluster', list(~ factor(.,
                                           levels = cluster_labs$cluster,
                                           labels = cluster_labs$cluster_lab)))
      
      # relabel clusters so that all are ordered in the following order:
      cluster_labels <- c('least positive', 'middle positive', 'most positive')
      # this relabelling is based on exploratory analysis that is not inlcluded in this repository
      cluster_levels <- switch(sec,
                               B1 = c(2,3,1),
                               D1 = c(2,1,3),
                               D2 = c(3,2,1),
                               E2 = c(3,2,1),
                               F2 = c(1,2,3))
      
      data_all_clusters <- data_all_clusters %>%
        mutate_at(all_of(cluster_name),
                  list(~ factor(as.numeric(.), 
                                labels = cluster_labels,
                                levels = cluster_levels)))
      
      # plot a summary of all questions in section across the three clusters
      plot_data %>%
        mutate_at('cluster',
                  list(~ factor(as.numeric(.),
                                labels = cluster_labels,
                                levels = cluster_levels))) %>%
        ggplot(aes(x = percent, y = name, fill = reorder(value, -as.numeric(value)))) +
        geom_bar(stat = 'identity') +
        facet_grid(~cluster, scales = 'free_x') +
        labs(y = str_c('Section ', sec),
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
      ggsave(filename = str_c(sec, '_clusters.png'),
             path = file.path(results_path, 'section_clusters'),
             width = 16, height = 16, units = 'cm')
    
    }
  }

# save all clusters
saveRDS(data_all_clusters,
        file = file.path(results_path, 'section_clusters', 'data_section_clusters.RDS'),
        compress = FALSE)
