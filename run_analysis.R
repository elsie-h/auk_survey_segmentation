# run analysis

# set project directory
directory <- ''

source(file.path(directory, 'setup.R'))

update_data <- TRUE
update_section_clusters <- TRUE
update_summaries <- TRUE

if (update_data) {
  # script for cleaning raw survey data
  source(file = file.path(scripts_path, '01data_clean.R'))
  rm(list = ls()[!(ls() %in% keep_objects)])
}

if (update_section_clusters) {
  # script for cleaning raw survey data
  source(file = file.path(scripts_path, '02section_clusters.R'))
  rm(list = ls()[!(ls() %in% keep_objects)])
} 

if (update_summaries) {
  for (section_cluster in c('B1', 'D1', 'D2', 'E2', 'F2')) {
    source(file = file.path(scripts_path, '03survey_summary_by_cluster.R'))
    rm(list = ls()[!(ls() %in% keep_objects)])
  }
}