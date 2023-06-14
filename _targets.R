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
  packages = c("tidyverse", "haven", 'sitrep', 'gtsummary', 'srvyr', 'dtplyr',
               'nih.joinpoint', 'survey'),
  debug = 'jp_regressions_proportions',
  # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)
# tar_make_future_config:
library(future)
library(future.callr)
plan(callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('scripts/functions.R')
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(

# Set up data -------------------------------------------------------------


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
             bind_rows(dat_list, .id = 'year'),
             format = 'parquet'),
  # Clean data
  tar_target(cleaned_dat,
             cleanMepsData(dat),
             format = 'parquet'),
  tar_target(combined_dat,
             coalesceData(cleaned_dat),
             format = 'parquet'),
  tar_target(releveled_dat,
             relevelFactors(combined_dat),
             format = 'parquet'),
  # Get down to just the columns we want
  tar_target(
    diab_dat_sub,
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
      DSA1C53,
      DSCKFT53,
      PERWT,
      MEDICAID,
      MEDICARE
    ) %>%
      mutate(year = as.numeric(str_extract(year, '[0-9]{4}'))) %>%
      filter(year >= 2008),
    format = 'parquet'
  ),
  tar_target(
    diab_dat,
    cumulativeServices(diab_dat_sub)
  ),
  tar_target(diab_list,
             diab_dat %>% group_split(year),
             iteration = 'list'),
  tar_target(years, as.list(2008:2020)),

# Calculate proportions ---------------------------------------------------


  # tar_target(
  #   overall,
  #   calcProp(diab_list, years, NULL),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   sex,
  #   calcProp(diab_list, years, 'SEX'),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   race,
  #   calcProp(diab_list, years, 'RACE_all'),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   edu,
  #   calcProp(diab_list, years, 'HIDEG_all'),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   ages,
  #   calcProp(diab_list, years, "AGE_all"),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   pov,
  #   calcProp(diab_list, years, 'POVCAT_all'),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   region,
  #   calcProp(diab_list, years, 'REGION_all'),
  #   pattern = map(diab_list, years)
  # ),
  # tar_target(
  #   insurance,
  #   calcProp(diab_list, years, 'INSCOV_all'),
  #   pattern = map(diab_list, years)
  #   ),


# Get Dashboard data ------------------------------------------------------

# tar_target(dash_files,
#            list.files(
#              here::here('data', 'dashboard'), full.names = TRUE
#            )),
  # tar_target(dash_file_names, str_extract(list.files(
  #   here::here('data', 'dashboard')
  # ), '^([^.])+')),
  # tar_target(
  #   dash_list,
  #   map(
  #     dash_files,
  #     ~ read_csv(
  #       .x,
  #       skip = 2,
  #       col_names = TRUE,
  #       n_max = 21,
  #       col_types = c('dcddd'),
  #       na = c('Suppressed', 'No Data')
  #     )
  #   ) %>%
  #     setNames(dash_file_names)
  # ),
  # tar_target(
  #   dash_dat,
  #   bind_rows(dash_list, .id = 'file_name') %>%
  #     separate(
  #       file_name,
  #       into = c('indicator', 'stratifier', 'strata'),
  #       sep = "_"
  #     ) %>%
  #     janitor::clean_names() %>%
  #     mutate(across(percentage:upper_limit, ~ .x / 100))
  # ),
  # tar_target(overall_indicator,
  #            combineOverall(overall, dash_dat)),
  # tar_target(sex_indicator,
  #            combineSex(sex, dash_dat)),
  # tar_target(race_indicator,
  #            combineRace(race, dash_dat)),
  # tar_target(edu_indicator,
  #            combineEdu(edu, dash_dat)),
  # tar_target(age_indicator,
  #            combineEdu(age, dash_dat)),
  # tar_target(overall_trend,
  #            plotTrends(overall_indicator, NULL)),
  # tar_target(sex_trend,
  #            plotTrends(sex_indicator, strata = 'SEX')),
  # tar_target(race_trend,
  #            plotTrends(race_indicator, strata = 'RACE_all')),
  # tar_target(edu_trend,
  #            plotTrends(edu_indicator, strata = 'HIDEG_all')),
  # tar_target(age_trend,
  #            plotTrends(edu_indicator, strata = 'AGE_all')),

# Yearly tables by stratifier ---------------------------------------------


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
  # tar_target(
  #   giant_table,
  #   createTable(diab_all_srvy, by = 'year')
  #   ),
  # tar_target(
  #   yearly_tables,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = NULL),
  #   pattern = map(diab_list),
  #   iteration = 'list'
  # ),
  # tar_target(
  #   sex_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "SEX"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   race_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "RACE_all"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   age_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "AGE_all"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   edu_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "HIDEG_all"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   pov_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "POVCAT_all"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   region_table,
  #   diab_list %>%
  #     filter(!is.na(DIABW)) %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "REGION_all"),
  #   pattern = map(diab_list)
  # ),
  # tar_target(
  #   ins_table,
  #   diab_list %>%
  #     as_survey_design(
  #       ids = VARPSU_all,
  #       weights = DIABW,
  #       strata = VARSTR_all,
  #       nest = TRUE
  #     ) %>%
  #     createTable(., by = "INSCOV_all"),
  #   pattern = map(diab_list)
  # ),
  # Make tables to get proportions and other relevant stats, then age adjust the proportions

# Age adjusted stats ------------------------------------------------------
  tar_target(
    yearly_table_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = NULL, diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    overall_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = NULL, diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    overall_stats_joined,
    left_join(select(yearly_table_all_stats, -strata),
              overall_age_adjusted, by = c('variable', 'label', 'year'))
  ),
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
    sex_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = "SEX", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
    ),
  tar_target(sex_stats_joined,
             left_join(sex_all_stats, sex_age_adjusted, by = c('variable', 'label', 'strata', 'year'))),

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
    race_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = "RACE_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(race_stats_joined,
             left_join(race_all_stats, race_age_adjusted, by = c('variable', 'label', 'strata', 'year'))),
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
    edu_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = "HIDEG_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(edu_stats_joined,
             left_join(edu_all_stats, edu_age_adjusted, by = c('variable', 'label', 'strata', 'year'))),
  tar_target(
    age_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "AGE_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    pov_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "POVCAT_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    pov_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = "POVCAT_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(pov_stats_joined,
             left_join(pov_all_stats, pov_age_adjusted, by = c('variable', 'label', 'strata', 'year'))),
  tar_target(
    ins_all_stats,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      createTableAllStats(., by = "INSCOV_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    ins_age_adjusted,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      createTableAllStats(., by = "INSCOV_all", diab_list) %>%
      makeTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(ins_stats_joined,
             left_join(ins_all_stats, ins_age_adjusted, by = c('variable', 'label', 'strata', 'year'))),
  tar_target(
    stats_tables_combined,
    bind_rows(
      list(
        overall = yearly_table_all_stats,
        age = age_all_stats,
        edu = edu_all_stats,
        race = race_all_stats,
        sex = sex_all_stats,
        insurance = ins_all_stats,
        poverty = pov_all_stats
      ),
      .id = 'stratifier'
    )
  ),
# This is the final table of age adjusted statistics
  tar_target(
    age_adjusted_stats,
    bind_rows(
      list(
        overall = overall_age_adjusted,
        age = age_all_stats,
        edu = edu_age_adjusted,
        race = race_age_adjusted,
        sex = sex_age_adjusted,
        insurance = ins_age_adjusted,
        poverty = pov_age_adjusted
      ),
      .id = 'stratifier'
    ) %>%
      filter(label %in% c(
        'Within last year',
        '2 or more A1C tests in a year',
        '1 or more foot examinations in a year',
        'Once a year',
        'YES',
        'Once a year or more'
      )
      ) %>%
      filter(strata %!in% c('Not available', 'Other Race/not Hispanic'))
  ),
  tar_target(
    stats_table,
    stats_tables_combined %>%
      dplyr::filter(label %in% c(
                      'Within last year',
                      '2 or more A1C tests in a year',
                      '1 or more foot examinations in a year',
                      'Once a year',
                      'YES',
                      'Once a year or more'
                      )
                    )
  ),

# Some other tables? ------------------------------------------------------


  # tar_target(
  #   overall_joined,
  #   joinTables(overall_indicator, yearly_table_all_stats)
  # ),
  # tar_target(
  #   sex_joined,
  #   joinTables(sex_indicator, sex_all_stats, variable = 'sex')
  # ),
  # tar_target(
  #   race_joined,
  #   joinTables(race_indicator, race_all_stats, variable = 'race')
  # ),
  # tar_target(
  #   edu_joined,
  #   joinTables(edu_indicator, edu_all_stats, variable = 'edu')
  # ),
  # tar_target(
  #   age_joined,
  #   joinTables(age_indicator, age_all_stats, variable = 'age')
  # ),


# Check suppression -------------------------------------------------------

  # tar_target(
  #   overall_suppressed,
  #   suppressData(overall_joined)
  # ),
  # tar_target(
  #   sex_suppressed,
  #   suppressData(sex_joined)
  # ),
  # tar_target(
  #   race_suppressed,
  #   suppressData(race_joined)
  # ),
  # tar_target(
  #   edu_suppressed,
  #   suppressData(edu_joined)
  # ),
  # tar_target(overall_trend_suppressed,
  #            plotTrends(overall_suppressed, NULL)),
  # tar_target(sex_trend_suppressed,
  #            plotTrends(sex_suppressed, strata = 'SEX')),
  # tar_target(race_trend_suppressed,
  #            plotTrends(race_suppressed, strata = 'RACE_all')),
  # tar_target(edu_trend_suppressed,
  #            plotTrends(edu_suppressed, strata = 'HIDEG_all')),
  # tar_target(overall_focus,
  #            overall_suppressed %>%
  #              dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
  #                            value %in% c(
  #                              'Within last year',
  #                              '2 or more A1C tests in a year',
  #                              '1 or more foot examinations in a year',
  #                              'Once a year',
  #                              'YES'
  #                            ))),
  # tar_target(sex_focus,
  #            sex_suppressed%>%
  #              dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
  #                            value %in% c(
  #                              'Within last year',
  #                              '2 or more A1C tests in a year',
  #                              '1 or more foot examinations in a year',
  #                              'Once a year',
  #                              'YES'
  #                            ))),
  # tar_target(race_focus,
  #            race_suppressed%>%
  #              dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
  #                            value %in% c(
  #                              'Within last year',
  #                              '2 or more A1C tests in a year',
  #                              '1 or more foot examinations in a year',
  #                              'Once a year',
  #                              'YES'
  #                            ))),
  # tar_target(edu_focus,
  #            edu_suppressed %>%
  #              dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
  #                            value %in% c(
  #                              'Within last year',
  #                              '2 or more A1C tests in a year',
  #                              '1 or more foot examinations in a year',
  #                              'Once a year',
  #                              'YES'
  #                            ))),

# Age adjust preventive services ------------------------------------
  tar_target(
    overall_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = NULL, diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    sex_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'SEX', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    race_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'RACE_all', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    edu_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'HIDEG_all', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    age_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'AGE_all', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),
  tar_target(
    pov_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'POVCAT_all', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),

  tar_target(
    ins_preventive,
    diab_list %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      ) %>%
      svystandardize(., ~ AGE_all, ~1, population = c(0.530535, 0.299194, 0.088967, 0.081304)) %>%
      preventiveService(., by = 'INSCOV_all', diab_list) %>%
      makePreventiveTablesLonger(),
    pattern = map(diab_list)
  ),

  # This is the final table of age adjusted # oif preventive care statistics
  tar_target(
    age_adjusted_preventive,
    bind_rows(
      list(
        overall = overall_preventive,
        age = age_preventive,
        edu = edu_preventive,
        race = race_preventive,
        sex = sex_preventive,
        insurance = ins_preventive,
        poverty = pov_preventive
      ),
      .id = 'stratifier'
    )
  ),

# Joinpoint analysis ------------------------------------------------------

  tar_target(
    jp_regressions_proportions,
    age_adjusted_stats %>%
      filter(strata != 'Total') %>%
      filter(strata != 'Not available') %>%
      filter(strata != 'Other Race/not Hispanic') %>%
      mutate(variable_text = case_when(variable == 'a1c' ~ '2 or more A1C tests',
                                       variable == 'CHOL_all' ~ 'Cholesterol tested',
                                       variable == 'DENT_all' ~ '1 or more dentist visits',
                                       variable == 'eye-exam' ~ 'Eye exam with dilation',
                                       variable == 'flu' ~ 'Received flu vaccine',
                                       variable == 'foot' ~ 'Foot examination'),
             strata = case_when(strata == 'stat_0' ~ 'Overall',
                                TRUE ~ strata)) %>%
      mutate(strata = as.factor(strata),
             year_adj = year - 2007) %>%
      split(., f = ~.$stratifier + .$variable) %>%
      map(., ~joinPointRegression(.x))
  ),
  tar_target(
    jp_regressions_preventive,
    age_adjusted_preventive %>%
      filter(strata != 'Total') %>%
      filter(strata != 'Not available') %>%
      filter(strata != 'Other Race/not Hispanic') %>%
      mutate(variable_text = case_when(variable == 'a1c' ~ '2 or more A1C tests',
                                       variable == 'CHOL_all' ~ 'Cholesterol tested',
                                       variable == 'DENT_all' ~ '1 or more dentist visits',
                                       variable == 'eye-exam' ~ 'Eye exam with dilation',
                                       variable == 'flu' ~ 'Received flu vaccine',
                                       variable == 'foot' ~ 'Foot examination'),
             strata = case_when(strata == 'stat_0' ~ 'Overall',
                                TRUE ~ strata)) %>%
      mutate(strata = as.factor(strata),
             year_adj = year - 2007) %>%
      split(., f = ~.$stratifier + .$variable) %>%
      map(., ~joinPointRegression(.x))
  ),
  tar_target(
    jp_proportion_lines,
    map2(jp_regressions_proportions, names(jp_regressions_proportions), function(.x, .y) {
      .x$report %>%
        mutate(group.service = .y)
    }) %>%
      bind_rows() %>%
      separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
      mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                                TRUE ~ strata)) %>%
      mutate(stratifier = factor(stratifier,
                                 c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
             strata = case_when(strata == 'stat_0' ~ '-',
                                strata == 'Greater than HS' ~ 'Greater than high school',
                                TRUE ~ strata)) %>%
      mutate(strata = as.factor(strata)) %>%
      mutate(stratifier = recode_factor(
        stratifier,
        overall = 'Overall',
        age = 'Age',
        sex = 'Sex',
        race = 'Race/Ethnicity',
        edu = 'Highest degree earned',
        insurance = 'Insurance coverage',
        poverty = 'Poverty level'
      ))
  ),
  tar_target(
    jp_prevention_aapc,
    map(jp_regressions_preventive, function(.x, .y) {
      .x$aapc
    }) %>%
      bind_rows(.id = 'group.service') %>%
      filter(str_detect(group.service, 'atleast_three')) %>%
      separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
      mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                                TRUE ~ strata)) %>%
      mutate(stratifier = factor(stratifier,
                                 c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
             strata = case_when(strata == 'stat_0' ~ '-',
                                strata == 'Greater than HS' ~ 'Greater than high school',
                                TRUE ~ strata)) %>%
      mutate(strata = as.factor(strata)) %>%
      mutate(stratifier = recode_factor(
        stratifier,
        overall = 'Overall',
        age = 'Age',
        sex = 'Sex',
        race = 'Race/Ethnicity',
        edu = 'Highest degree earned',
        insurance = 'Insurance coverage',
        poverty = 'Poverty level'
      ))
  ),
tar_target(
  jp_prevention_apc,
  map(jp_regressions_preventive, function(.x, .y) {
    .x$apc
  }) %>%
    bind_rows(.id = 'group.service') %>%
    filter(str_detect(group.service, 'atleast_three')) %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
  ),
tar_target(
  jp_proportion_plot_data,
  map(jp_regressions_proportions, ~.x$data_export %>%
        mutate(apc = as.character(apc),
               model = as.character(model),
               joinpoints = as.character(joinpoints),
               final_selected_model = as.character(final_selected_model))) %>%
    map2(., names(.), ~.x %>%
            mutate(stratifier.service = .y)) %>%
    bind_rows() %>%
    separate(stratifier.service, c('stratifier', 'variable'), sep = '[.]') %>%
    ungroup()
),

# Joinpoint without 2020 --------------------------------------------------
tar_target(
  jp_regressions_proportions_sans_2020,
  age_adjusted_stats %>%
    filter(strata != 'Total') %>%
    filter(strata != 'Not available') %>%
    filter(strata != 'Other Race/not Hispanic') %>%
    filter(year <= 2019) %>%
    mutate(variable_text = case_when(variable == 'a1c' ~ '2 or more A1C tests',
                                     variable == 'CHOL_all' ~ 'Cholesterol tested',
                                     variable == 'DENT_all' ~ '1 or more dentist visits',
                                     variable == 'eye-exam' ~ 'Eye exam with dilation',
                                     variable == 'flu' ~ 'Received flu vaccine',
                                     variable == 'foot' ~ 'Foot examination'),
           strata = case_when(strata == 'stat_0' ~ 'Overall',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata),
           year_adj = year - 2007) %>%
    split(., f = ~.$stratifier + .$variable) %>%
    map(., ~joinPointRegression(.x))
),
tar_target(
  jp_regressions_preventive_sans_2020,
  age_adjusted_preventive %>%
    filter(strata != 'Total') %>%
    filter(strata != 'Not available') %>%
    filter(strata != 'Other Race/not Hispanic') %>%
    filter(year <= 2019) %>%
    mutate(variable_text = case_when(variable == 'a1c' ~ '2 or more A1C tests',
                                     variable == 'CHOL_all' ~ 'Cholesterol tested',
                                     variable == 'DENT_all' ~ '1 or more dentist visits',
                                     variable == 'eye-exam' ~ 'Eye exam with dilation',
                                     variable == 'flu' ~ 'Received flu vaccine',
                                     variable == 'foot' ~ 'Foot examination'),
           strata = case_when(strata == 'stat_0' ~ 'Overall',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata),
           year_adj = year - 2007) %>%
    split(., f = ~.$stratifier + .$variable) %>%
    map(., ~joinPointRegression(.x))
),
tar_target(
  jp_proportion_lines_sans_2020,
  map2(jp_regressions_proportions_sans_2020, names(jp_regressions_proportions_sans_2020), function(.x, .y) {
    .x$report %>%
      mutate(group.service = .y)
  }) %>%
    bind_rows() %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
),
tar_target(
  jp_prevention_aapc_sans_2020,
  map(jp_regressions_preventive_sans_2020, function(.x, .y) {
    .x$aapc
  }) %>%
    bind_rows(.id = 'group.service') %>%
    filter(str_detect(group.service, 'atleast_three')) %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
),
tar_target(
  jp_prevention_apc_sans_2020,
  map(jp_regressions_preventive_sans_2020, function(.x, .y) {
    .x$apc
  }) %>%
    bind_rows(.id = 'group.service') %>%
    filter(str_detect(group.service, 'atleast_three')) %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
),
tar_target(
  jp_proportion_plot_data_sans_2020,
  map(jp_regressions_proportions_sans_2020, ~.x$data_export %>%
        mutate(apc = as.character(apc),
               model = as.character(model),
               joinpoints = as.character(joinpoints),
               final_selected_model = as.character(final_selected_model))) %>%
    map2(., names(.), ~.x %>%
           mutate(stratifier.service = .y)) %>%
    bind_rows() %>%
    separate(stratifier.service, c('stratifier', 'variable'), sep = '[.]') %>%
    ungroup()
),
tar_target(
  jp_proportion_aapc_sans_2020,
  map(jp_regressions_proportions_sans_2020, function(.x, .y) {
    .x$aapc
  }) %>%
    bind_rows(.id = 'group.service') %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
),
tar_target(
  jp_proportion_apc_sans_2020,
  map(jp_regressions_proportions_sans_2020, function(.x, .y) {
    .x$apc
  }) %>%
    bind_rows(.id = 'group.service') %>%
    separate(group.service, c('stratifier', 'service'), sep = '[.]') %>%
    mutate(strata = case_when(stratifier == 'overall' ~ 'stat_0',
                              TRUE ~ strata)) %>%
    mutate(stratifier = factor(stratifier,
                               c('overall', 'age', 'sex', 'race', 'edu', 'insurance', 'poverty')),
           strata = case_when(strata == 'stat_0' ~ '-',
                              strata == 'Greater than HS' ~ 'Greater than high school',
                              TRUE ~ strata)) %>%
    mutate(strata = as.factor(strata)) %>%
    mutate(stratifier = recode_factor(
      stratifier,
      overall = 'Overall',
      age = 'Age',
      sex = 'Sex',
      race = 'Race/Ethnicity',
      edu = 'Highest degree earned',
      insurance = 'Insurance coverage',
      poverty = 'Poverty level'
    ))
)


)


