# run analysis

# set project directory
directory <- ''

source(file.path(directory, 'setup.R'))

# script for cleaning raw survey data
source(file = file.path(scripts_path, '01data_clean.R'))
rm(list = ls()[!(ls() %in% keep_objects)])

# knit word doc of summay tables and correlation plots
knitr::knit(file = file.path(directory, 'markdown', 'survey_summary.Rmd'))

# script for creating section clusters for sections B1, D1, D2, E2, F2
source(file = file.path(scripts_path, '02section_clusters.R'))
rm(list = ls()[!(ls() %in% keep_objects)])

# script for summarising the whole survey by each of the section clusters
# if section cluster is NULL, summary plots are by adherence groups only
for (section_cluster in c(NULL, 'B1', 'D1', 'D2', 'E2', 'F2')) {
  source(file = file.path(scripts_path, '03survey_summary_by_cluster.R'))
  rm(list = ls()[!(ls() %in% keep_objects)])
}