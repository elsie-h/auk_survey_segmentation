# setup

# load libraries
source(file.path(directory, 'libraries.R'))

# define paths
# image_path <- file.path(directory, 'figures')
data_path <- file.path(directory, 'data')
results_path <- file.path(directory, 'results')
scripts_path <- file.path(directory, 'analysis_scripts')
functions_path <- file.path(directory, 'functions')

# create data and results folders if they don't already exist
# (as they are empty so not included in public github repo)
for (x_path in c(data_path, results_path)) {
  res_path <- file.path(results_path, x_path)
  if (!dir.exists(res_path)) dir.create(res_path)
}

seed <- 546

keep_objects <- c('directory', ls()[str_detect(ls(), 'path$')], 'keep_objects', 'seed')

cat(str_c('Check that the following files are in ', data_path, ':\n',
            'Asthma condition management survey - SPSS v2.0.sav\n',
            'Asthma condition management survey - MAP.xlsx'))
