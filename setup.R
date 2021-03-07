# setup

# load libraries
source(file.path(directory, 'libraries.R'))

# define paths
# image_path <- file.path(directory, 'figures')
data_path <- file.path(directory, 'data')
results_path <- file.path(directory, 'results')
scripts_path <- file.path(directory, 'analysis_scripts')
functions_path <- file.path(directory, 'functions')

seed <- 546

keep_objects <- c(ls()[str_detect(ls(), 'path$')], 'keep_objects', 'seed')