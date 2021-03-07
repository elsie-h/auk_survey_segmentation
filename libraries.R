# Package names
packages <- c(
  # loading data
  "readr", "readxl",
  # data manipulation
  "dplyr", "tidyr", "stringr",
  # plots
  "ggplot2", "cowplot", 
  # colour schemes on plots
  "viridis", "viridisLite", 
  # MCA
  "FactoMineR",
  # clusterboot
  "fpc",
  # silhouette stats
  "cluster")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  print('Uncomment line 22 to allow installation of packages.')
  # install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
