directory <- '/Users/elsiehorne/Docs/asthma_behaviour_segmentation'

source(file.path(directory, 'setup.R'))

data_all_clusters <- readRDS(file = file.path(results_path, 'data_all_clusters_new.RDS'))

for (s in c('B1', 'D2', 'E2', 'F2')) {
  cluster_name <- str_c(s, '_cluster')
  data_all_clusters %>%
    rename(cluster = cluster_name) %>%
    group_by(adherence, cluster) %>%
    count() %>%
    ungroup(cluster) %>%
    filter(!is.na(cluster)) %>%
    mutate(percent = round(100*n/sum(n),0)) %>%
    print(n=12)
}
