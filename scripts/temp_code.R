library(gtsummary)
library(tidyverse)
library(srvyr)
library(gt)
library(survey)
library(targets)
tar_load(diab_list)
`%!in%` <- Negate(`%in%`)

ageStandardSurvey <- function(data) {
  data %>%
    as_survey_design(
      ids = VARPSU_all,
      weights = DIABW,
      strata = VARSTR_all,
      nest = TRUE
    ) %>%
    svystandardize(.,
                   ~ AGE_all,
                   ~ 1,
                   population = c(0.530535, 0.299194, 0.088967, 0.081304))
}

countsAndProps <- function(data, variable, year) {
  variable_sym <- as.symbol(variable)
  counts <- data %>%
    group_by( {{variable_sym}} ) %>%
    survey_count()

  props <- data %>%
    group_by( {{variable_sym}}) %>%
    summarise(survey_prop()) %>%
    rename(prop = coef)

  out <- left_join(counts, props) %>%
    ungroup() %>%
    mutate(Insurance_Type = case_when({{variable_sym}} == 'YES' ~ paste0(variable, '_YES'),
                                        {{variable_sym}} == 'NO' ~ paste0(variable, '_NO')))
  out <- out[, c('Insurance_Type', 'prop', 'n')]
  names(out)[c(2, 3)] <- paste(names(out)[c(2, 3)], year, sep = "_")
  out

}

unweightedCounts <- function(data) {
  data %>%
    tbl_svysummary(include = 'MEDICARE',
                  statistic = list(all_categorical() ~ "{n_unweighted}"))
}

diab_age_srvy <- map(diab_list, ~ageStandardSurvey(.x))
dat_grid <- expand_grid(
  year = 2008:2020,
  variable = c(
    'MEDICARE',
    'MEDICAID',
    'OPAEV_all',
    'OPBEV_all',
    'PRVEV_all',
    'TRIEV_all',
    'GVAEV_all',
    'GVBEV_all',
    'GVCEV_all'
  )
) %>%
  arrange(variable, year) %>%
  mutate(data = rep(diab_age_srvy, 9)) %>%
  filter(
    variable %in% c(
      'MEDICARE',
      'MEDICAID',
      'OPAEV_all',
      'OPBEV_all',
      'PRVEV_all',
      'TRIEV_all'
    ) |
      (
        variable %in% c('GVAEV_all',
                        'GVBEV_all',
                        'GVCEV_all') & year >= 2018
      )
  ) %>%
  group_split(variable)
all_counts <- map(dat_grid,
    ~ pmap(.x, ~ countsAndProps(..3, ..2, ..1)) %>%
      reduce(left_join, by = 'Insurance_Type'))


all_counts_df <- bind_rows(all_counts) %>%
  #filter(!is.na(Insurance_Type)) %>%
  select(Insurance_Type, num_range('prop_', 2008:2020), num_range('n_', 2008:2020)) %>%
  mutate(across(starts_with('prop'), ~round(.x * 100, 2)),
         across(starts_with('n'), ~round(.x))) %>%
  mutate(Insurance_Type = case_when(str_detect(Insurance_Type, '_all') == TRUE ~ str_replace(Insurance_Type, '_all', ''),
                                    TRUE ~ Insurance_Type)) %>%
  mutate(Insurance_Type = case_when(str_detect(Insurance_Type, 'GVAEV') == TRUE ~ str_replace(Insurance_Type, 'GVAEV', 'Other_Public_Ins'),
                                    str_detect(Insurance_Type, 'GVBEV') == TRUE ~ str_replace(Insurance_Type, 'GVBEV', 'Other_Public_HMO'),
                                    str_detect(Insurance_Type, 'GVCEV') == TRUE ~ str_replace(Insurance_Type, 'GVCEV', 'Other_Public_Premium_Paid'),
                                    str_detect(Insurance_Type, 'OPAEV') == TRUE ~ str_replace(Insurance_Type, 'OPAEV', 'Other_Public_A'),
                                    str_detect(Insurance_Type, 'OPBEV') == TRUE ~ str_replace(Insurance_Type, 'OPBEV', 'Other_Public_B'),
                                    str_detect(Insurance_Type, 'PRVEV') == TRUE ~ str_replace(Insurance_Type, 'PRVEV', 'Private'),
                                    str_detect(Insurance_Type, 'TRIEV') == TRUE ~ str_replace(Insurance_Type, 'TRIEV', 'Tricare'),
                                    TRUE ~ Insurance_Type)) %>%
  arrange(Insurance_Type) %>%
  filter(str_detect(Insurance_Type, 'YES'))


all_counts_df %>%
  select(Insurance_Type, starts_with('prop')) %>%
  gt() %>%
  gtsave('insurance_table_prop.docx')

all_counts_df %>%
  select(Insurance_Type, starts_with('n')) %>%
  gt() %>%
  gtsave('insurance_table_n.docx')

# Unweighted counts
unweighted_counts <- diab_list %>%
  bind_rows() %>%
  filter(DIABW > 0) %>%
  group_by(year) %>%
  mutate(across(c('MEDICARE',
                  'MEDICAID',
                  'OPAEV_all',
                  'OPBEV_all',
                  'PRVEV_all',
                  'TRIEV_all',
                  'GVAEV_all',
                  'GVBEV_all',
                  'GVCEV_all'),
                ~case_when(.x == 'YES' ~ 1,
                           .x == 'NO' ~ 0))) %>%
  summarise(across(c('MEDICARE',
                     'MEDICAID',
                     'OPAEV_all',
                     'OPBEV_all',
                     'PRVEV_all',
                     'TRIEV_all',
                     'GVAEV_all',
                     'GVBEV_all',
                     'GVCEV_all'),
                   ~sum(.x)))
unweighted_counts_t <- as.data.frame(t(unweighted_counts[, -1]))
colnames(unweighted_counts_t) <- t(unweighted_counts[, 1])
unweighted_counts_t <- unweighted_counts_t %>%
  mutate(Insurance_Type = rownames(.)) %>%
  select(Insurance_Type, everything()) %>%
  mutate(Insurance_Type = case_when(str_detect(Insurance_Type, 'GVAEV') == TRUE ~ str_replace(Insurance_Type, 'GVAEV', 'Other_Public_Ins'),
                                    str_detect(Insurance_Type, 'GVBEV') == TRUE ~ str_replace(Insurance_Type, 'GVBEV', 'Other_Public_HMO'),
                                    str_detect(Insurance_Type, 'GVCEV') == TRUE ~ str_replace(Insurance_Type, 'GVCEV', 'Other_Public_Premium_Paid'),
                                    str_detect(Insurance_Type, 'OPAEV') == TRUE ~ str_replace(Insurance_Type, 'OPAEV', 'Other_Public_A'),
                                    str_detect(Insurance_Type, 'OPBEV') == TRUE ~ str_replace(Insurance_Type, 'OPBEV', 'Other_Public_B'),
                                    str_detect(Insurance_Type, 'PRVEV') == TRUE ~ str_replace(Insurance_Type, 'PRVEV', 'Private'),
                                    str_detect(Insurance_Type, 'TRIEV') == TRUE ~ str_replace(Insurance_Type, 'TRIEV', 'Tricare'),
                                    TRUE ~ Insurance_Type))
rownames(unweighted_counts_t) <- NULL

unweighted_counts_t %>%
  gt() %>%
  gtsave('insurance_table_unweighted_n.docx')
