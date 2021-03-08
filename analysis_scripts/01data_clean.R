# clean survey data

# load functions
source(file.path(functions_path,'combine_likert.R'))
source(file.path(functions_path,'corr_plot.R'))

# read the data into R
data <- foreign::read.spss(file.path(data_path,'Asthma condition management survey - SPSS v2.0.sav')) %>%
  as_tibble() %>%
  mutate_if(is.factor, as.character) %>%
  # remove first row as questions
  slice(-1) %>%
  # all as lower case
  mutate_if(is.character, str_to_lower) %>%
  # fix typo
  rename(prev_attitudes2 = rev_attitudes2)

survey_key <- readxl::read_excel(path = file.path('data', 'Asthma condition management survey - MAP.xlsx'), 
                                 range = 'B1:C242') %>%
  rename(variable_label = Variable, survey_question = Label) %>%
  # correct typo
  mutate_at('variable_label', list(~ if_else(. %in% 'rev_attitudes2', 'prev_attitudes2', .))) %>%
  # create column to indicate survey section
  mutate(section = case_when(str_detect(variable_label, '^asthma_diagnosis')
                             ~ 'A',
                             str_detect(variable_label, '^health|^realise')
                             ~ 'B1',
                             str_detect(variable_label, '^man_')
                             ~ 'B1',
                             str_detect(variable_label, '^asthma_control')
                             ~ 'B2',
                             str_detect(variable_label, '^asthma_manage')
                             ~ 'C',
                             str_detect(variable_label, '^reliever|^preventer')
                             ~ 'C',
                             str_detect(variable_label, '^combi_freq|^mart_freq')
                             ~ 'C',
                             str_detect(variable_label, '^prev_|^rel_')
                             ~ 'D',
                             str_detect(variable_label, '^combi_|^mart_')
                             ~ 'D',
                             str_detect(variable_label, '^asthma_review|seeking')
                             ~ 'E1',
                             str_detect(variable_label, '^support_|^knowledge_')
                             ~ 'E2',
                             str_detect(variable_label, '^monitor')
                             ~ 'F1',
                             str_detect(variable_label, '^self')
                             ~ 'F2',
                             str_detect(variable_label, '^internet_|^device_')
                             ~ 'G',
                             str_detect(variable_label, '^covid_')
                             ~ 'H',
                             TRUE ~ 'Demographics'))  %>%
  mutate_at('section', 
            list(~ case_when(. %in% 'D' & str_detect(variable_label, 'prev_') ~ 'D1',
                             . %in% 'D' & str_detect(variable_label, 'rel_') ~ 'D2',
                             . %in% 'D' & str_detect(variable_label, 'combi_') ~ 'D3',
                             . %in% 'D' & str_detect(variable_label, 'mart_') ~ 'D4',
                             TRUE ~ .))) %>%
  mutate_at('survey_question',
            list(~ case_when(str_detect(variable_label, 'internet_freq') ~ str_c('How often do you use the internet for: ', tolower(.)),
                             str_detect(variable_label, 'device_freq') ~ str_c('How often do you use a: ', tolower(.)),
                             TRUE ~ .)))

saveRDS(survey_key,
        file = file.path(data_path, 'survey_key.RDS'),
        compress = FALSE)

# define likert levels
likert_levs <- c('strongly disagree', 'disagree', 'neutral', 'agree', 'strongly agree')
freq_levs <- c('never', 'rarely', 'sometimes', 'often', 'always')

# identify the variables that are measured on the five point scale
levs_check <- sapply(names(data), 
                     function(x)
                       all(unique(data[,x][[1]]) %in% c(likert_levs, NA_character_))|
                       all(unique(data[,x][[1]]) %in% c(freq_levs, NA_character_)))

likert_vars <- names(levs_check)[levs_check]
inhaler_vars <- likert_vars[str_detect(likert_vars, '^prev_')|
                              str_detect(likert_vars, '^rel_')|
                              str_detect(likert_vars, '^combi_')|
                              str_detect(likert_vars, '^mart_')]
logic_vars <- c('self_mon_breg1', 'self_mon_skill')

combine_vars <- likert_vars[!(likert_vars %in% inhaler_vars)]

data <- data %>%
  mutate_at(all_of(combine_vars),
            list(~ if_else(is.na(.),
                           'skip',
                           .)))

# store in list for later use
likert_list <- list(likert_levs = likert_levs,
                    freq_levs = freq_levs,
                    likert_vars = likert_vars, 
                    inhaler_vars = inhaler_vars, 
                    logic_vars = logic_vars, 
                    combine_vars = combine_vars)
saveRDS(likert_list,
        file = file.path(data_path, 'likert_list.RDS'),
        compress = FALSE)

# Demographics
demographics <- data %>%
  select(RecordNo, age_rb:ethnicity_new) %>% 
  mutate_at(vars(starts_with('age')), factor) %>%
  mutate_at(vars(c('profile_gender',
                   'profile_GOR',
                   'profile_education_level')), 
            factor) %>%
  mutate_at('profile_gross_household',
            list(~ factor(case_when(is.na(.) ~ 'prefer not to answer',
                                    TRUE ~ str_replace_all(., '\xa3', '£')),
                          levels = c("£5,000 to £9,999 per year",
                                     "£10,000 to £14,999 per year",
                                     "£15,000 to £19,999 per year" ,
                                     "£20,000 to £24,999 per year",
                                     "£25,000 to £29,999 per year",
                                     "£30,000 to £34,999 per year",
                                     "£35,000 to £39,999 per year",
                                     "£40,000 to £44,999 per year",
                                     "£45,000 to £49,999 per year" ,
                                     "£50,000 to £59,999 per year" ,
                                     "£60,000 to £69,999 per year" ,
                                     "£70,000 to £99,999 per year",
                                     "£100,000 to £149,999 per year",
                                     "£150,000 and over",
                                     "don't know",
                                     "prefer not to answer")))) 

# Section A
data_A <- data %>%
  select(RecordNo, starts_with('asthma_diagnosis')) %>%
  mutate_at('asthma_diagnosis1', factor) %>%
  mutate_at('asthma_diagnosis2',
            list(~ factor(.,
                          levels = c('1 year or less',
                                     '2-5 years',
                                     '6-10 years',
                                     '11 years or more'))))

# Section B1
data_B1 <- data %>%
  select(RecordNo, health_con1:man_envir) %>%
  mutate_at(vars(health_con1:man_envir), 
            list(~ factor(., 
                          levels = c(likert_levs, 'skip', 'NA'))))

# Section B2
data_B2 <- data %>%
  select(RecordNo, asthma_control1:asthma_control3_5) %>%
  # replace NA with skip and order levels
  mutate_at(2:ncol(.), 
            list(~ if_else(is.na(.), 'skip', .))) %>%
  mutate_at('asthma_control1', 
            list(~ factor(.,
                          levels = c('not at all',
                                     'for a few days per year',
                                     'for a few months per year',
                                     'for a few weeks per year',
                                     'for more than 6 months per year',
                                     'skip')))) %>%
  mutate_at('asthma_control2', list(~ factor(.,
                                             levels = c('none',
                                                        '1',
                                                        '2 or more',
                                                        'i take oral steroids every day',
                                                        'skip')))) %>%
  mutate_at(vars(starts_with('asthma_control3')),
            list(~ factor(.,
                          levels = c('no',
                                     'yes',
                                     'skip'))))


# Section C
data_C <- data %>%
  select(RecordNo, asthma_manage_1:mart_freq3) %>%
  mutate_at(vars(asthma_manage_1:asthma_manage_10),
            list(~ factor(if_else(is.na(.), 'skip', .),
                          levels = c('no',
                                     'yes',
                                     'skip')))) %>%
  mutate_at('asthma_manage_mart', 
            list(~ factor(case_when(asthma_manage_3 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c('no',
                                     'yes',
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('reliever_freq')),
            list(~ factor(case_when(asthma_manage_1 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c('not at all',
                                     '1-2 times a week',
                                     '3-4 times a week',
                                     '5-6 times a week',
                                     'every day',
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(c('mart_freq1', 'mart_freq2')),
            list(~ factor(case_when(asthma_manage_mart %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c('not at all',
                                     '1-2 times a week',
                                     '3-4 times a week',
                                     '5-6 times a week',
                                     'every day',
                                     'skip',
                                     'NA')))) %>%
  mutate_at('mart_freq3',
            list(~ factor(case_when(asthma_manage_mart %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ str_remove(., 'i take it every day ')),
                          levels = c('no matter what',
                                     'when i have symptoms',
                                     'only when my symptoms are very severe',
                                     'during certain seasons or times of the year',
                                     'when i\'m reminded to do so',
                                     'i take it only occasionally',
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('preventer_freq')),
            list(~ factor(case_when(asthma_manage_2 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ str_remove(., 'i take it every day ')),
                          levels = c('no matter what',
                                     'when i have symptoms',
                                     'only when my symptoms are very severe',
                                     'during certain seasons or times of the year',
                                     'when i\'m reminded to do so',
                                     'i take it only occasionally',
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('combi_freq')),
            list(~ factor(case_when(asthma_manage_3 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ str_remove(., 'i take it every day ')),
                          levels = c('no matter what',
                                     'when i have symptoms',
                                     'only when my symptoms are very severe',
                                     'during certain seasons or times of the year',
                                     'when i\'m reminded to do so',
                                     'i take it only occasionally',
                                     'skip',
                                     'NA')))) 

data_D <- data %>%
  select(RecordNo, 
         asthma_manage_1, asthma_manage_2, asthma_manage_3, asthma_manage_mart,
         prev_knowledge1:mart_emot2) %>%
  mutate_at(vars(starts_with('prev')),
            list(~ factor(case_when(asthma_manage_2 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c(likert_levs,
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('rel')),
            list(~ factor(case_when(asthma_manage_1 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c(likert_levs,
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('combi')),
            list(~ factor(case_when(asthma_manage_3 %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c(likert_levs,
                                     'skip',
                                     'NA')))) %>%
  mutate_at(vars(starts_with('mart')),
            list(~ factor(case_when(asthma_manage_mart %in% 'yes' & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c(likert_levs,
                                     'skip',
                                     'NA'))))

# Section D: preventer
data_D_preventer <- data_D %>%
  select(RecordNo, starts_with('prev'))

# Section D: reliever
data_D_reliever <- data_D %>%
  select(RecordNo, starts_with('rel'))

# Section D: combi
data_D_combi <- data_D %>%
  select(RecordNo, starts_with('combi'))

# Section D: MART
data_D_mart <- data_D %>%
  select(RecordNo, starts_with('mart'))

## Section E1
data_E1 <- data %>%
  select(RecordNo, asthma_control1, 
         asthma_review_freq:asthma_review_reasons_other,
         support_seeking_flareup:info_seeking_sources_11) %>%
  select(-asthma_review_reasons_other) %>%
  mutate_at('asthma_review_freq',
            list(~ factor(case_when(is.na(.) ~ 'skip', 
                                    str_detect(., 'not applicable') ~ 'not applicable',
                                    TRUE ~ .),
                          levels = c('less than once a year',
                                     'once a year',
                                     'more than once a year',
                                     'not applicable',
                                     'skip')))) %>%
  mutate_at('asthma_review_reasons',
            list(~ factor(case_when(asthma_review_freq %in% c('less than once a year', 'once a year','more than once a year') & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ str_trunc(., width = 30, side = 'right')),
                          levels = c('an invitation from my docto...',
                                     'an increase in my symptoms',
                                     'i book one regularly',
                                     'i was speaking to a doctor ...',
                                     'other',
                                     'skip',
                                     'NA'
                          )))) %>%
  mutate_at('support_seeking_flareup',
            list(~ factor(case_when(asthma_control1 %in% c('for a few days per year',
                                                           'for a few months per year',
                                                           'for a few weeks per year',
                                                           'for more than 6 months per year') & is.na(.) ~ 'skip',
                                    is.na(.) ~ 'NA',
                                    TRUE ~ .),
                          levels = c('immediately',
                                     'after a few days',
                                     'after a few weeks',
                                     'after a longer time',
                                     'i do not usually seek help',
                                     'skip',
                                     'NA')))) %>%
  select(-asthma_control1) %>%
  mutate_at('info_seeking_freq',
            list(~ factor(.,
                          levels = c('never',
                                     'almost never (every few years)',
                                     'once a year',
                                     'a few times per year',
                                     'once a month',
                                     '2-3 times per month',
                                     '2-3 times per week',
                                     'every day'
                          )))) %>%
  mutate_at(vars(info_seeking_sources_1:info_seeking_sources_11),
            list(~ factor(case_when(info_seeking_freq %in% 'never' ~ 'NA',
                                    is.na(.) ~ 'skip',
                                    TRUE ~ .),
                          levels = c('no', 'yes', 'skip', 'NA'))))


# Section E2
data_E2 <- data %>%
  select(RecordNo, support_cap1:knowledge_check4) %>%
  mutate_at(vars(support_cap1:knowledge_check4), 
            list(~ factor(., 
                          levels = c(likert_levs, 'skip', 'NA'))))

# Section F1
data_F1 <- data %>%
  select(RecordNo, monitor_how_1:monitor_how_8,
         monitor_freq, monitor_when) %>%
  mutate_at(vars(starts_with('monitor_how')),
            list(~ factor(if_else(is.na(.), 'skip', .),
                          levels = c('no', 'yes', 'skip')))) %>%
  mutate_at('monitor_freq', 
            list(~ factor(case_when(monitor_how_8 %in% 'yes' & is.na(.) ~ 'NA',
                                    is.na(.) ~ 'skip',
                                    TRUE ~ .),
                          levels = c('at least once a day',
                                     'a few times per week',
                                     'once a week',
                                     '2 to 3 times per month',
                                     'once a month',
                                     'less often',
                                     'skip',
                                     'NA')))) %>%
  mutate_at('monitor_when', 
            list(~ factor(case_when(monitor_how_8 %in% 'yes' & is.na(.) ~ 'NA',
                                    is.na(.) ~ 'skip',
                                    TRUE ~ .),
                          levels = c('only when i have symptoms',
                                     'mostly when i have symptoms',
                                     'mostly as part of a regular routine',
                                     'always as part of a regular routine',
                                     'other',
                                     'skip',
                                     'NA'))))

# Section F2
data_F2 <- data %>%
  select(RecordNo, monitor_how_8, self_mon_knowledge:selfmon_emot2) %>%
  mutate_at(vars(c('self_mon_skill', 'self_mon_breg1')),
            list(~ factor(case_when(monitor_how_8 %in% 'yes' ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c(likert_levs, 'skip', 'NA')))) %>%
  mutate_at(vars(self_mon_knowledge:selfmon_emot2),
            list(~ factor(., levels = c(likert_levs, 'skip', 'NA')))) %>%
  select(-monitor_how_8)

# Section G
data_G <- data %>%
  select(RecordNo, internet_freq1:device_reasons_grid6_99) %>%
  mutate_at(vars(starts_with('device_reasons_grid1')),
            list(~ factor(case_when(device_freq1 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA'))))  %>%
  mutate_at(vars(starts_with('device_reasons_grid2')),
            list(~ factor(case_when(device_freq2 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA')))) %>%
  mutate_at(vars(starts_with('device_reasons_grid3')),
            list(~ factor(case_when(device_freq3 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA')))) %>%
  mutate_at(vars(starts_with('device_reasons_grid4')),
            list(~ factor(case_when(device_freq4 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA')))) %>%
  mutate_at(vars(starts_with('device_reasons_grid5')),
            list(~ factor(case_when(device_freq5 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA')))) %>%
  mutate_at(vars(starts_with('device_reasons_grid6')),
            list(~ factor(case_when(device_freq6 %in% c('sometimes', 'often', 'always') & is.na(.) ~ 'NA',
                                    is.na(as.character(.)) ~ 'skip',
                                    TRUE ~ as.character(.)),
                          levels = c('no', 'yes', 'skip', 'NA')))) 

# Section G: frequencies
data_G_freq <- data_G %>% 
  select(RecordNo, contains('freq')) %>%
  mutate_at(vars(contains('freq')),
            list(~ factor(.,
                          levels = c('never',
                                     'rarely',
                                     'sometimes',
                                     'often',
                                     'always',
                                     'skip',
                                     'NA'))))

# Section G: smartphone
data_G_smartphone <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid1'))

# Section G: smartwatch
data_G_smartwatch <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid2'))

# Section G: smartmeter
data_G_smartmeter <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid3'))

# Section G: smart other
data_G_smartother <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid4'))

# Section G: voice assistant
data_G_voiceassistant <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid5'))

# Section G: virtual reality
data_G_virtualreality <- data_G %>% 
  select(RecordNo, starts_with('device_reasons_grid6'))

# Section H: covid
covid_vars <- names(data %>% select(starts_with('covid')))
covid_likert <- c('covid_cap1', 'covid_cap3')
covid_easier <- 'covid_cap2'
covid_other <- covid_vars[!(covid_vars %in% c(covid_likert, covid_easier))]
data_H <- data %>%
  select(RecordNo, starts_with('covid')) %>%
  mutate_at(vars(all_of(covid_likert)), 
            list(~ factor(.,
                          levels = c(likert_levs, 'skip', 'NA')))) %>%
  mutate_at(vars(all_of(covid_easier)),
            list(~ factor(case_when(is.na(as.character(.)) ~ 'NA',
                                    TRUE ~ as.character(.)),
                          levels = c('much easier than before',
                                     'a little easier than before',
                                     'the same',
                                     'a little harder than before',
                                     'much harder than before',
                                     'NA')))) 

for (var in covid_other) {
  levs <- unique(data_H[,var][[1]])
  levs <- levs[!is.na(levs)]
  levs <- c(levs[str_detect(levs, 'much') & str_detect(levs, 'less')],
            levs[str_detect(levs, 'little') & str_detect(levs, 'less')],
            levs[str_detect(levs, 'no change|the same')],
            levs[str_detect(levs, 'little') & str_detect(levs, 'more')],
            levs[str_detect(levs, 'much') & str_detect(levs, 'more')]
            )
  data_H <- data_H %>%
    mutate_at(vars(all_of(var)), 
              list(~ factor(case_when(is.na(as.character(.)) ~ 'skip',
                                      TRUE ~ as.character(.)),
                            levels = c(levs, 'skip'))))
}
  

data_adherence <- data_C %>%
  select(RecordNo, preventer_freq, combi_freq, mart_freq3) %>%
  mutate_at(vars(c('preventer_freq', 'combi_freq', 'mart_freq3')),
            list(~ case_when(. %in% 'NA' ~ as.character(.),
                             . %in% 'no matter what' ~ 'good',
                             TRUE ~ 'poor'))) %>%
  mutate(adherence = case_when(preventer_freq %in% 'good' |
                                    combi_freq %in% 'good' |
                                    mart_freq3 %in% 'good' ~ 'good',
                                  preventer_freq %in% 'poor' |
                                    combi_freq %in% 'poor' |
                                    mart_freq3 %in% 'poor' ~ 'poor',
                                  preventer_freq %in% 'NA' &
                                    combi_freq %in% 'NA' &
                                    mart_freq3 %in% 'NA' ~ 'no_preventer',
                                  TRUE ~ NA_character_)) %>%
  select(RecordNo, adherence)

data_clean <- data_A %>%
  left_join(data_B1, by = 'RecordNo') %>%
  left_join(data_B2, by = 'RecordNo') %>%
  left_join(data_C, by = 'RecordNo') %>%
  left_join(data_D_reliever, by = 'RecordNo') %>%
  left_join(data_D_preventer, by = 'RecordNo') %>%
  left_join(data_D_combi, by = 'RecordNo') %>%
  left_join(data_D_mart, by = 'RecordNo') %>%
  left_join(data_E1, by = 'RecordNo') %>%
  left_join(data_E2, by = 'RecordNo') %>%
  left_join(data_F1, by = 'RecordNo') %>%
  left_join(data_F2, by = 'RecordNo') %>%
  left_join(data_G_freq, by = 'RecordNo') %>%
  left_join(data_G_smartphone, by = 'RecordNo') %>%
  left_join(data_G_smartwatch, by = 'RecordNo') %>%
  left_join(data_G_smartother, by = 'RecordNo') %>%
  left_join(data_G_smartmeter, by = 'RecordNo') %>%
  left_join(data_G_voiceassistant, by = 'RecordNo') %>%
  left_join(data_G_virtualreality, by = 'RecordNo') %>%
  left_join(data_H, by = 'RecordNo') %>%
  left_join(demographics, by = 'RecordNo') %>%
  left_join(data_adherence, by = 'RecordNo')

saveRDS(data_clean,
        file = file.path(data_path, 'data_clean.RDS'),
        compress = FALSE)

