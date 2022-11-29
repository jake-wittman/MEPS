# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(future)
library(future.callr)
library(tarchetypes)
# Set up future
#plan(callr)
options(clustermq.scheduler = 'multiprocess')

# Set target options:
tar_option_set(
  packages = c("tidyverse", "haven", 'sitrep', 'gtsummary', 'srvyr'),
  # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('scripts/functions.R')
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(

  # Get MEPS files
  tar_target(
    name = dat_files,
    command = list.files(
      path = 'data',
      pattern = 'meps',
      full.names = TRUE
    )
  ),
  tar_target(dat_names,
             map_chr(
               dat_files, ~ str_extract(.x, 'meps[0-9]{4}')
             )),
  tar_target(
    dat_list,
    map2(dat_files, dat_names, ~ readAndSelect(.x, .y)) %>%
      setNames(dat_names)
  ),
  tar_target(dat,
             bind_rows(dat_list, .id = 'year')),
  # Clean data
  tar_target(cleaned_dat,
             cleanMepsData(dat)),
  tar_target(combined_dat,
             coalesceData(cleaned_dat)),
  tar_target(releveled_dat,
             relevelFactors(combined_dat)),
  # Get down to just the columns we want
  tar_target(
    diab_dat,
    select(
      releveled_dat,
      year,
      DUPERSID,
      SEX,
      DIABW,
      ends_with('_all'),
      CHOLCK53,
      FLUSHT53,
      CHECK53,
      DENTCK53,
      DSA1C53,
      DSCKFT53
    ) %>%
      mutate(year = str_extract(year, '[0-9]{4}'))
  ),
  tar_target(diab_list,
             diab_dat %>% group_split(year),
             iteration = 'list'),
  tar_target(years, as.list(2001:2020)),
  tar_target(
    overall,
    calcProp(diab_list, years, NULL),
    pattern = map(diab_list, years)
  ),
  tar_target(
    sex,
    calcProp(diab_list, years, 'SEX'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    race,
    calcProp(diab_list, years, 'RACE_all'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    edu,
    calcProp(diab_list, years, 'HIDEG_all'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    age,
    safelyCalcProp(diab_list, years, "AGE_all"),
    pattern = map(diab_list, years)
  ),
  tar_target(dash_files,
             list.files(
               here::here('data', 'dashboard'), full.names = TRUE
             )),
  tar_target(dash_file_names, str_extract(list.files(
    here::here('data', 'dashboard')
  ), '^([^.])+')),
  tar_target(
    dash_list,
    map(
      dash_files,
      ~ read_csv(
        .x,
        skip = 2,
        col_names = TRUE,
        n_max = 21,
        col_types = c('dcddd'),
        na = c('Suppressed', 'No Data')
      )
    ) %>%
      setNames(dash_file_names)
  ),
  tar_target(
    dash_dat,
    bind_rows(dash_list, .id = 'file_name') %>%
      separate(
        file_name,
        into = c('indicator', 'stratifier', 'strata'),
        sep = "_"
      ) %>%
      janitor::clean_names() %>%
      mutate(across(percentage:upper_limit, ~ .x / 100))
  ),
  tar_target(overall_indicator,
             combineOverall(overall, dash_dat)),
  tar_target(sex_indicator,
             combineSex(sex, dash_dat)),
  tar_target(race_indicator,
             combineRace(race, dash_dat)),
  tar_target(edu_indicator,
             combineEdu(edu, dash_dat)),
  tar_target(overall_trend,
             plotTrends(overall_indicator, NULL)),
  tar_target(sex_trend,
             plotTrends(sex_indicator, strata = 'SEX')),
  tar_target(race_trend,
             plotTrends(race_indicator, strata = 'RACE_all')),
  tar_target(edu_trend,
             plotTrends(edu_indicator, strata = 'HIDEG_all')),
  tar_target(
    diab_all_srvy,
    diab_dat %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      )
  ),
  tar_target(giant_table,
             createTable(diab_all_srvy, by = 'year')),
  tar_target(
    yearly_tables,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTable(., by = NULL),
    pattern = map(diab_list),
    iteration = 'list'
  ),
  tar_target(
    sex_table,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTable(., by = "SEX"),
    pattern = map(diab_list)
  ),
  tar_target(
    race_table,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTable(., by = "RACE_all"),
    pattern = map(diab_list)
  ),
  tar_target(
    edu_table,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTable(., by = "HIDEG_all"),
    pattern = map(diab_list)
  ),

  # Make tables to get additional values from, like sample size and standard error
  tar_target(
    sex_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "SEX", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    yearly_table_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = NULL, diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    race_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "RACE_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    edu_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "HIDEG_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    overall_joined,
    joinTables(overall_indicator, yearly_table_all_stats)
  ),
  tar_target(
    sex_joined,
    joinTables(sex_indicator, sex_all_stats, variable = 'sex')
  ),
  tar_target(
    race_joined,
    joinTables(race_indicator, race_all_stats, variable = 'race')
  ),
  tar_target(
    edu_joined,
    joinTables(edu_indicator, edu_all_stats, variable = 'edu')
  ),
  tar_target(
    overall_suppressed,
    suppressData(overall_joined)
  ),
  tar_target(
    sex_suppressed,
    suppressData(sex_joined)
  ),
  tar_target(
    race_suppressed,
    suppressData(race_joined)
  ),
  tar_target(
    edu_suppressed,
    suppressData(edu_joined)
  ),
  tar_target(overall_trend_suppressed,
             plotTrends(overall_suppressed, NULL)),
  tar_target(sex_trend_suppressed,
             plotTrends(sex_suppressed, strata = 'SEX')),
  tar_target(race_trend_suppressed,
             plotTrends(race_suppressed, strata = 'RACE_all')),
  tar_target(edu_trend_suppressed,
             plotTrends(edu_suppressed, strata = 'HIDEG_all')),
  tar_target(overall_focus,
             overall_suppressed%>%
               dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                             value %in% c(
                               'Within last year',
                               '2 or more A1C tests in a year',
                               '1 or more foot examinations in a year',
                               'Once a year',
                               'YES'
                             ))),
  tar_target(sex_focus,
             sex_suppressed%>%
               dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                             value %in% c(
                               'Within last year',
                               '2 or more A1C tests in a year',
                               '1 or more foot examinations in a year',
                               'Once a year',
                               'YES'
                             ))),
  tar_target(race_focus,
             race_suppressed%>%
               dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                             value %in% c(
                               'Within last year',
                               '2 or more A1C tests in a year',
                               '1 or more foot examinations in a year',
                               'Once a year',
                               'YES'
                             ))),
  tar_target(edu_focus,
             edu_suppressed%>%
               dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                             value %in% c(
                               'Within last year',
                               '2 or more A1C tests in a year',
                               '1 or more foot examinations in a year',
                               'Once a year',
                               'YES'
                             )))
)
