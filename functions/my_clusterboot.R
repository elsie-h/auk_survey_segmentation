# define clusterboot function
my_clusterboot <- function(scores,
                           dims, 
                           k_in = 2:6,
                           option = NULL) {
  
  # keep_dims from data
  data_boot <- scores[,1:dims]
  # data_boot <- select(bind_rows(scores), all_of(1:dims))
  # clusterboot
  clusterboot_res <- lapply(k_in, function(x)
    clusterboot(data = data_boot,
                B=100,
                bootmethod = 'boot',
                clustermethod = kmeansCBI,
                k = x,
                nstart = 25,
                iter.max = 25,
                dissolution = 0.5,
                recover = 0.75,
                seed = seed))
  
  # filename for results
  filename <- str_c('clusterboot_res_', dims, '.RDS') 
  
  # save results
  saveRDS(clusterboot_res,
          file = file.path(results_path, filename),
          compress = FALSE)
  
  # create plot
  bind_rows(lapply(seq_along(k_in), function(x)
    tibble(cluster = 1:k_in[x],
           jaccard = clusterboot_res[[x]]$bootmean) %>%
      mutate(k = max(cluster)))) %>%
    pivot_longer(cols = jaccard) %>%
    ggplot(aes(x = cluster, y = value, fill = name)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_hline(yintercept = 0.85, linetype = 'dashed', colour = viridis(3)[2]) +
    scale_x_continuous(breaks = seq(1,max(k_in),2)) +
    scale_fill_viridis_d(guide=FALSE) +
    labs(y = 'mean Jaccard coefficient',
         title = maintitle,
         subtitle = str_c('Cluster stability across k for ', dims, ' MCA dimensions'),
         caption = str_c('Method: kmeans with 25 random starts and a maximum of 25 iterations;
                         Stability assessment: 100 bootstrap samples;
                         Dashed line correspondes to mean Jaccard coefficiant = 0.85')) +
    facet_wrap(~ k) +
    theme_cowplot(12)
  ggsave(filename = str_c('clusterboot_', dims, '.png'),
         path = image_path,
         width = 14, height = 14, units = 'cm')
}