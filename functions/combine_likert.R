combine_likert <- function(data,
                           vars,
                           threshold = 5) {
  
  likert_levs <- c('strongly disagree', 'disagree', 'neutral', 'agree', 'strongly agree')
  tmp <- data %>%
    # select all prev variables on the likert scale
    dplyr::select(all_of(vars)) %>%
    pivot_longer(cols = everything()) %>%
    group_by(name, value) %>%
    count() %>% 
    ungroup(value) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ungroup() %>%
    dplyr::select(-n) %>%
    pivot_wider(names_from = value, values_from = percent) %>%
    mutate_at(2:ncol(.), list(~ if_else(is.na(.), 0, .)))
  
  for (l in c(likert_levs, 'skip', 'NA')) {
    if (!(l %in% names(tmp))) {
      tmp <- tmp %>% mutate(!! l := 0)
    } else {
      next
    }
  }
  
  # combine skip and neutral
  combNA3 <- tmp %>%
    filter(skip < threshold) %>%
    dplyr::select(name) %>% unlist() %>% unname()
  # combine disagree and strongly disagree
  comb12 <- tmp %>%
    filter((`strongly disagree` < threshold) & (`strongly disagree` + disagree > threshold)) %>%
    dplyr::select(name) %>% unlist() %>% unname()
  # combine neutral, disagree and strongly disagree
  comb123 <- tmp %>%
    filter((`strongly disagree` < threshold) & (`strongly disagree` + disagree < threshold)) %>%
    dplyr::select(name) %>% unlist() %>% unname()
  # combine agree and strongly agree
  comb45 <- tmp %>%
    filter((`strongly agree` < threshold) & (`strongly agree` + agree > threshold)) %>%
    dplyr::select(name) %>% unlist() %>% unname()
  # combine neutral, agree and strongly agree
  comb345 <- tmp %>%
    filter((`strongly agree` < threshold) & (`strongly agree` + agree < threshold)) %>%
    dplyr::select(name) %>% unlist() %>% unname()
  
  # combine
  data <- data %>%
    mutate_at(vars(all_of(combNA3)),
              list(~ case_when(. %in% 'skip'
                               ~ 'neutral',
                               TRUE ~ .))) %>%
    mutate_at(vars(all_of(comb12)),
              list(~ case_when(str_detect(., 'disagree') 
                               ~ 'disagree',
                               TRUE ~ .))) %>%
    mutate_at(vars(all_of(comb123)),
              list(~ case_when(str_detect(., 'disagree') 
                               ~ 'neutral',
                               TRUE ~ .))) %>%
    mutate_at(vars(all_of(comb45)),
              list(~ case_when(. %in% 'strongly agree' 
                               ~ 'agree',
                               TRUE ~ .))) %>%
    mutate_at(vars(all_of(comb345)),
              list(~ case_when(. %in% 'agree' | . %in% 'strongly agree' 
                               ~ 'neutral',
                               TRUE ~ .))) %>%
    mutate_at(all_of(vars), 
              list(~ factor(., levels = c(likert_levs, 'skip', 'NA'))))
  
  return(data)
}
