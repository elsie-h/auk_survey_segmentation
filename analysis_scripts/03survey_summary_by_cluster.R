# summary plots for all variables by adherence groups and section clusters (optional)

# For each of the adherence subgroups, identify section-specific clusters
# Plot a summary of the section variables each of the section clusters
# Store the section clusters in a dataset potentially for later use

# load data
likert_list <- readRDS(file = file.path(data_path, 'likert_list.RDS'))
data_clean <- readRDS(file = file.path(data_path, 'data_clean.RDS'))
survey_key <- readRDS(file = file.path(data_path, 'survey_key.RDS'))
data_section_clusters <- readRDS(file = file.path(results_path, 'section_clusters', 'data_section_clusters.RDS'))

source(file.path(functions_path, 'combine_likert.R'))
source(file.path(functions_path, 'my_line_plot.R'))
source(file.path(functions_path, 'section_bar_plot.R'))

if (is.null(section_cluster)) {
  section_cluster_levs <- NULL
  plot_height <- 16
} else {
  section_cluster_levs <- c('least positive', 'middle positive', 'most positive')
  plot_height <- 22
}

# adherence levels for plotting
adherence_group <- c('good', 'poor', 'no_preventer')
adherence_levs <- c(good = 'Good preventer-type adherence',
                    poor = 'Poor preventer-type adherence',
                    no_preventer = 'No preventer-type inhaler')

likert_sections <- c('B1', 'D1', 'D2', 'D3', 'D4', 'E2', 'F2', 'G', 'H')

# define folder
folder <- str_c('all', section_cluster, sep='_')

# check if folder exists, if not create it
res_path <- file.path(results_path, folder)
if (!dir.exists(res_path)) dir.create(res_path)
# also create all subfolders
for (section in c(likert_sections, 'A', 'B2', 'C', 'E1', 'F1', 'Demographics')) {
  res_path <- file.path(results_path, folder, section)
  if (!dir.exists(res_path)) dir.create(res_path)
}

if (is.null(section_cluster)) {
  # counts in each adherence group
  data_section_clusters %>%
    group_by(adherence) %>%
    count() %>%
    ungroup() %>%
    mutate(`n (%)` = str_c(n, ' (', round(100*n/sum(n),0), '%)')) %>%
    select(-n) %>%
    print(n=Inf)
  
  data_clusters <- data_section_clusters %>%
    mutate(cluster = factor(adherence,
                            levels = adherence_group,
                            labels = adherence_levs)) %>%
    mutate_at('adherence', factor, levels = adherence_group,
              labels = adherence_levs) %>%
    select(RecordNo, adherence, cluster) %>%
    left_join(data_clean %>% select(-adherence), by = 'RecordNo') %>%
    mutate(colour_var = 'most positive')
  
} else if ((length(section_cluster) == 1) && (section_cluster %in% likert_sections)) {
  
  # counts in each adherence group x section cluster
  cluster_name <- str_c(section_cluster, '_cluster')
  data_section_clusters %>%
    rename(cluster = cluster_name) %>%
    group_by(adherence, cluster) %>%
    count() %>%
    ungroup(cluster) %>%
    filter(!is.na(cluster)) %>%
    mutate(`n (%)` = str_c(n, ' (', round(100*n/sum(n),0), '%)')) %>%
    select(-n) %>%
    print(n=Inf)
  
  cluster_var <- str_c(section_cluster, 'cluster', sep = '_')
  data_clusters <- data_section_clusters %>%
    select(RecordNo, adherence, all_of(cluster_var)) %>%
    pivot_longer(cols = 3:ncol(.)) %>%
    filter(!is.na(value)) %>%
    mutate_at('adherence', factor, levels = adherence_group,
              labels = adherence_levs) %>%
    mutate(cluster = value) %>%
    select(RecordNo, adherence, cluster) %>%
    left_join(data_clean %>% select(-adherence), by = 'RecordNo') %>%
    mutate(colour_var = cluster)
  
} else {
  stop('Either set adherence_group to all three, or specify a likert section.')
}

# specify labels for all non-likert plots
fill_labs <- data_clusters %>%
  group_by(cluster) %>%
  count() %>%
  ungroup() %>% 
  mutate(label = str_c(cluster, '; n = ', n)) %>%
  select(label) %>%
  unlist() %>% unname()

# plot_pallette <- viridis(length(fill_labs))
colour_var <- data_clusters %>%
  distinct(cluster, colour_var)

################################################################################
# summary plots for all likert sections
################################################################################
for (s in likert_sections) {
  
  if (n_distinct(colour_var$colour_var) == 1) {
    section_bar_plot(data = data_clusters, 
                     var_section = s,
                     cluster_section = NULL,
                     adherence_group = adherence_group,
                     folder = folder)
  } else {
    for (a in adherence_group) {
      section_bar_plot(data = data_clusters, 
                       var_section = s,
                       cluster_section = section_cluster,
                       adherence_group = a,
                       folder = folder)
    }
  }
  
  for (variable in survey_key$variable_label[survey_key$section == s]) {
    my_line_plot(var = variable,
                 data = data_clusters,
                 folder = folder,
                 section = s,
                 ylim = c(0,NA),
                 section_cluster = section_cluster)
  }
}

################################################################################
# section A
################################################################################
my_line_plot(var = 'asthma_diagnosis2',
             data = data_clusters,
             folder = folder,
             section = 'A',
             type = 'bar',
             section_cluster = section_cluster)

################################################################################
# section B2
################################################################################
#  create summary plots
my_line_plot(var = 'asthma_control1',
             data = data_clusters,
             folder = folder,
             section = 'B2',
             type = 'bar',
             section_cluster = section_cluster)

my_line_plot(var = 'asthma_control2',
             data = data_clusters,
             folder = folder,
             section = 'B2',
             type = 'bar',
             section_cluster = section_cluster)

#### asthma_control3.png
plot_data <- data_clusters %>% 
  select(RecordNo, adherence, cluster,
         all_of(survey_key$variable_label[str_detect(survey_key$variable_label, 'asthma_control3_')])) %>%
  select(-asthma_control3_5) %>%
  pivot_longer((cols = 4:ncol(.))) %>%
  mutate(survey_question = factor(case_when(name %in% 'asthma_control3_1' ~ 1,
                                     name %in% 'asthma_control3_2' ~ 2,
                                     name %in% 'asthma_control3_3' ~ 3,
                                     name %in% 'asthma_control3_4' ~ 4,
                                     TRUE ~ NA_real_),
                                  labels = c('to A&E',
                                             'admitted to\nhospital',
                                             'in ICU',
                                             'on a\nventilator'))) %>%
  group_by(adherence, cluster, survey_question, value) %>% 
  count() %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup()

if (n_distinct(colour_var$colour_var) == 1) {
  p <- plot_data %>%
    right_join(expand(plot_data, cluster, value),
               by = c('cluster', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = adherence)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_grey(name = 'Adherence') +
    guides(fill=guide_legend(nrow=3,
                             title.position = 'top')) 
} else {
  p <- plot_data %>%
    right_join(expand(plot_data, adherence, cluster, survey_question, value), 
               by = c('adherence', 'cluster', 'survey_question', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = colour_var)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ adherence, ncol=1) +
    scale_fill_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
    guides(fill=guide_legend(nrow=1,
                             byrow=TRUE,
                             title.position = 'top')) 
}
p +
  labs(x=NULL,
       subtitle = 'Because of my asthma, in the past year I\'ve been... ',
       caption = 'Section B2; asthma_control3')  +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        text = element_text(size=16),
        plot.title = element_text(size = 18, face = 'bold'))
ggsave(filename = 'asthma_control3.png',
       path = file.path(results_path, folder, 'B2'),
       width = 25, height = plot_height, units = 'cm')

################################################################################
# section C
################################################################################
# inhalers
plot_data <- data_clusters %>% 
  select(RecordNo, adherence, cluster,
         asthma_manage_1, asthma_manage_2, asthma_manage_3, asthma_manage_mart) %>%
  pivot_longer((cols = 4:ncol(.))) %>%
  mutate(survey_question = factor(case_when(name %in% 'asthma_manage_1' ~ 1,
                                            name %in% 'asthma_manage_2' ~ 2,
                                            name %in% 'asthma_manage_3' ~ 3,
                                            name %in% 'asthma_manage_mart' ~ 4,
                                            TRUE ~ NA_real_),
                                  labels = c('reliever',
                                             'preventer',
                                             'combination',
                                             'MART'))) %>%
  group_by(adherence, cluster, survey_question, value) %>% 
  count() %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup()

if (n_distinct(colour_var$colour_var) == 1) {
  
  p <- plot_data %>%
    # expand data so all possible levels plotted
    right_join(expand(plot_data, cluster, value), by = c('cluster','value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = adherence)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_grey(name = 'Adherence') +
    guides(fill=guide_legend(nrow=3,
                             title.position = 'top')) 
} else {
  p <- plot_data %>%
    right_join(expand(plot_data, adherence, cluster, survey_question, value), 
               by = c('adherence', 'cluster', 'survey_question', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = colour_var)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ adherence, ncol=1) +
    scale_fill_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
    guides(fill=guide_legend(nrow=1,
                             byrow=TRUE,
                             title.position = 'top')) 
}
p +
  labs(x=NULL,
       subtitle = 'Which of the following inhalers have you used to manage your asthma\nin the past year?',
       caption = 'Section C; asthma_manage_inhalers') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        text = element_text(size=16),
        plot.title = element_text(size = 18, face = 'bold'))
ggsave(filename = 'asthma_manage_inhalers.png',
       path = file.path(results_path, folder, 'C'),
       width = 25, height = plot_height, units = 'cm')

# other
plot_data <- data_clusters %>% 
  select(RecordNo, adherence,  cluster,
         asthma_manage_4, asthma_manage_5, asthma_manage_6, asthma_manage_7,
         asthma_manage_8, asthma_manage_9) %>%
  pivot_longer((cols = 4:ncol(.))) %>%
  mutate(survey_question = factor(case_when(name %in% 'asthma_manage_4' ~ 1,
                                            name %in% 'asthma_manage_5' ~ 2,
                                            name %in% 'asthma_manage_6' ~ 3,
                                            name %in% 'asthma_manage_7' ~ 4,
                                            name %in% 'asthma_manage_8' ~ 5,
                                            name %in% 'asthma_manage_9' ~ 6,
                                            TRUE ~ NA_real_),
                                  labels = c('monoclonal\nantibody\ninjection',
                                             'LTRAs',
                                             'theophylline',
                                             'antihistamines',
                                             'nasal\nsprays',
                                             'alternative\ntreatments'))) %>%
  group_by(adherence, cluster, survey_question, value) %>% 
  count() %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup()

if (n_distinct(colour_var$colour_var) == 1) {
  p <- plot_data %>%
    right_join(expand(plot_data, cluster, value),
               by = c('cluster', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = adherence)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_grey(name = 'Adherence') +
    guides(fill=guide_legend(nrow=3,
                             title.position = 'top')) 
} else {
  p <- plot_data %>%
    right_join(expand(plot_data, adherence, cluster, survey_question, value), 
               by = c('adherence', 'cluster', 'survey_question', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = colour_var)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ adherence, ncol=1) +
    scale_fill_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
    guides(fill=guide_legend(nrow=1,
                             byrow=TRUE,
                             title.position = 'top')) 
}

p  +
  labs(x=NULL,
       subtitle = 'Which of the following have you used to manage your asthma\nin the past year?',
       caption = 'Section C; asthma_manage') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        text = element_text(size=16),
        plot.title = element_text(size = 18, face = 'bold'))
ggsave(filename = 'asthma_manage.png',
       path = file.path(results_path, folder, 'C'),
       width = 25, height = plot_height, units = 'cm')

# inhaler freq
for (inhaler_freq in c('reliever_freq1', 'reliever_freq2',
                       'preventer_freq', 'combi_freq', 
                       'mart_freq1', 'mart_freq2', 'mart_freq3')) {
  my_line_plot(var = inhaler_freq,
               data = data_clusters,
               folder = folder,
               section_cluster = section_cluster,
               section = 'C',
               remove_NA = 'before_percent',
               ylim = c(0, NA),
               type = 'bar')
}

################################################################################
# E1
################################################################################
for (var in c('asthma_review_freq', 'asthma_review_reasons',
              'support_seeking_flareup', 'info_seeking_freq')) {
  my_line_plot(var = var,
               data = data_clusters,
               folder = folder,
               section = 'E1',
               type = 'line',
               section_cluster = section_cluster)
}

# info_seeking_sources
plot_data <- data_clusters %>% 
  select(RecordNo, adherence,  cluster,
         info_seeking_sources_1:info_seeking_sources_10) %>%
  pivot_longer((cols = 4:ncol(.))) %>%
  mutate(survey_question = factor(case_when(name %in% 'info_seeking_sources_1' ~ 1,
                                            name %in% 'info_seeking_sources_2' ~ 2,
                                            name %in% 'info_seeking_sources_3' ~ 3,
                                            name %in% 'info_seeking_sources_4' ~ 4,
                                            name %in% 'info_seeking_sources_5' ~ 5,
                                            name %in% 'info_seeking_sources_6' ~ 6,
                                            name %in% 'info_seeking_sources_7' ~ 7,
                                            name %in% 'info_seeking_sources_8' ~ 8,
                                            name %in% 'info_seeking_sources_9' ~ 9,
                                            name %in% 'info_seeking_sources_10' ~ 10,
                                            TRUE ~ NA_real_),
                                  labels = c('asthma\naction plan',
                                             'NHS 111',
                                             'local\npharmacy',
                                             'GP or\nnurse',
                                             'NHS\nwebsite',
                                             'AUK\nwebsite',
                                             'other\nonline',
                                             'social\nmedia',
                                             'family or\nfriends',
                                             'leaflets,\nbooks,\nmagazines'))) %>%
  group_by(adherence, cluster, survey_question, value) %>% 
  count() %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup()

if (n_distinct(colour_var$colour_var) == 1) {
  p <- plot_data %>%
    right_join(expand(plot_data, cluster, value), 
               by = c('cluster', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = adherence)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_grey(name = 'Adherence') +
    guides(fill=guide_legend(nrow=3,
                             title.position = 'top')) 
} else {
  p <- plot_data %>%
    right_join(expand(plot_data, adherence, cluster, survey_question, value), 
               by = c('adherence', 'cluster', 'survey_question', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = colour_var)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ adherence, ncol=1) +
    scale_fill_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
    guides(fill=guide_legend(nrow=1,
                             byrow=TRUE,
                             title.position = 'top')) 
}
p +
  labs(x=NULL,
       subtitle = 'Where do you seek information about your asthma?',
       caption = 'Section E1; info_seeking_sources') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        text = element_text(size=16),
        plot.title = element_text(size = 18, face = 'bold'))
ggsave(filename = 'info_seeking_sources.png',
       path = file.path(results_path, folder, 'E1'),
       width = 25, height = plot_height, units = 'cm')

################################################################################
# F1
################################################################################
# monitoring
plot_data <- data_clusters %>% 
  select(RecordNo, adherence, cluster,
         monitor_how_1, monitor_how_2, monitor_how_3, 
         monitor_how_4, monitor_how_5, monitor_how_6,
         monitor_how_8) %>%
  pivot_longer((cols = 4:ncol(.))) %>%
  mutate(survey_question = factor(case_when(name %in% 'monitor_how_1' ~ 1,
                                            name %in% 'monitor_how_2' ~ 2,
                                            name %in% 'monitor_how_3' ~ 3,
                                            name %in% 'monitor_how_4' ~ 4,
                                            name %in% 'monitor_how_5' ~ 5,
                                            name %in% 'monitor_how_6' ~ 6,
                                            name %in% 'monitor_how_8' ~ 7,
                                            TRUE ~ NA_real_),
                                  labels = c('peak flow\nmeter',
                                             'spirometer',
                                             'pulse\noximeter',
                                             'diary',
                                             'app',
                                             'mental\nnote',
                                             'Nothing'))) %>%
  group_by(cluster, adherence, survey_question, value) %>% 
  count() %>%
  ungroup(value) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup()

if (n_distinct(colour_var$colour_var) == 1) {
  p <- plot_data %>%
    right_join(expand(plot_data, cluster, value), 
               by = c('cluster', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = adherence)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_grey(name = 'Adherence') +
    guides(fill=guide_legend(nrow=3,
                             title.position = 'top'))
} else {
  p <- plot_data %>%
    right_join(expand(plot_data, adherence, cluster, survey_question, value),
               by = c('adherence', 'cluster', 'survey_question', 'value')) %>%
    filter(value %in% 'yes') %>%
    mutate_at('value', factor, levels = 'yes') %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at('adherence', 
              list(~ factor(case_when(str_detect(cluster, '^Good') ~ adherence_levs[1],
                                      str_detect(cluster, '^Poor') ~ adherence_levs[2],
                                      str_detect(cluster, '^No') ~ adherence_levs[3],
                                      TRUE ~ as.character(.)),
                            levels = adherence_levs))) %>%
    left_join(colour_var, by = 'cluster') %>%
    ggplot(aes(x = survey_question, y = percent, fill = colour_var)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ adherence, ncol=1) +
    scale_fill_viridis_d(name = str_c('Section ', section_cluster, ' cluster')) +
    guides(fill=guide_legend(nrow=1,
                             byrow=TRUE,
                             title.position = 'top'))
}

p  +
  labs(x=NULL,
       subtitle = 'What do you use to monitor your asthma yourself?',
       caption = 'Section F1; monitor_how') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        text = element_text(size=16),
        plot.title = element_text(size = 18, face = 'bold'))
ggsave(filename = 'monitor_how.png',
       path = file.path(results_path, folder, 'F1'),
       width = 25, height = plot_height, units = 'cm')

# monitor_freq
my_line_plot(var = 'monitor_freq',
             data = data_clusters,
             folder = folder,
             section = 'F1',
             section_cluster = section_cluster,
             remove_NA = 'before_percent',
             ylim = c(0, NA),
             type = 'bar',
             adherence_group = adherence_group)

# monitor_when
my_line_plot(var = 'monitor_when',
             data = data_clusters,
             folder = folder,
             section = 'F1',
             section_cluster = section_cluster,
             remove_NA = 'before_percent',
             ylim = c(0, NA),
             type = 'bar')

################################################################################
# G
################################################################################
# device_freq
for (device_freq in survey_key$variable_label[str_detect(survey_key$variable_label, 'device_reasons')]) {
  my_line_plot(var = device_freq,
               data = data_clusters,
               folder = folder,
               section = 'G',
               # remove_NA = TRUE,
               section_cluster = section_cluster,
               ylim = c(0, NA),
               type = 'bar')
}

################################################################################
# Demographics
################################################################################
my_line_plot(var = 'age_rb',
             data = data_clusters,
             folder = folder,
             section = 'Demographics',
             section_cluster = section_cluster,
             ylim = c(0, NA),
             type = 'line')

my_line_plot(var = 'profile_gender',
             data = data_clusters,
             folder = folder,
             section = 'Demographics',
             section_cluster = section_cluster,
             ylim = c(0, NA),
             type = 'bar')
 

