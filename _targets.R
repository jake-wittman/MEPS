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
plan(callr)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "haven", 'sitrep', 'gtsummary', 'srvyr'), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source('scripts/functions.R')
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # tar_target(
  #   name = grasp_form_data_input,
  #   command = "//cdc.gov/locker/CCHP_NCCD_DDT_Data1/epistat/Surveillance/Surveillance and Trends Reporting System/GRASP/NEW_Jan2018/Datasets/consol_dat_2022_1014.sas7bdat",
  #   format = 'file'
  # ),
  # tar_target(
  #   name = grasp_form_data,
  #   command = readGraspData(grasp_form_data_input),
  #   format = 'feather'
  # ),
  # Get MEPS files
  tar_target(
    name = dat_files,
    command = list.files(path = 'data', pattern = 'meps', full.names = TRUE)
  ),
  tar_target(
    dat_names,
    map_chr(dat_files, ~str_extract(.x, 'meps[0-9]{4}'))
  ),
  tar_target(
    dat_list,
    map2(dat_files, dat_names, ~readAndSelect(.x, .y)) %>%
      setNames(dat_names)
  ),
  tar_target(
    dat,
    bind_rows(dat_list, .id = 'year')
  ),
  # Clean data
  tar_target(
    cleaned_dat,
    cleanMepsData(dat)
  ),
  tar_target(
    combined_dat,
    coalesceData(cleaned_dat)
  ),
  # Get down to just the columns we want
  tar_target(
    diab_dat,
    select(combined_dat, year, DUPERSID, SEX, DIABW, ends_with('_all'),
           CHOLCK53, FLUSHT53, CHECK53, DENTCK53, DSA1C53, DSCKFT53) %>%
      mutate(year = str_extract(year, '[0-9]{4}'))
  ),
  tar_target(
    diab_list,
    diab_dat %>% group_split(year),
    iteration = 'list'
  ),
  tar_target(years, as.list(2001:2020)),
  tar_target(
    overall,
    safelyCalcProp(diab_list, years, NULL),
    pattern = map(diab_list, years)
  ),
  tar_target(
    sex,
    safelyCalcProp(diab_list, years, 'SEX'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    race,
    safelyCalcProp(diab_list, years, 'RACE_all'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    edu,
    safelyCalcProp(diab_list, years, 'HIDEG_all'),
    pattern = map(diab_list, years)
  ),
  tar_target(
    age,
    safelyCalcProp(diab_list, years, "AGE_all"),
    pattern = map(diab_list, years)
  )

)
