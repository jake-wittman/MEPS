
library(tidyverse)
library(haven)
`%!in%` <- Negate(`%in%`)

# Read in data
readAndSelect <- function(data_path, data_year) {
  yr <- substr(data_year, 7, 8)
  eye_var <- paste0('DSEY', yr, '53')
  chol_var <- paste0('DSCH', yr, '53')
  feet_var <- paste0('DSFT', yr, '53')
  flu_var <- paste0('DSFL', yr, '53')
  if (str_ends(data_path, '[sSpP]{3}') == TRUE) {
    temp <- read_xpt(data_path)

  } else {
    temp <- read_sas(data_path)

  }
  if (yr == '01') {
    temp <- temp %>%
      mutate(RACEX = recode_factor(
        RACEX,
        `1` = 'American Indian',
        `2` = 'Aleut, Eskimo',
        `3` = 'Asian or Pacific Islander',
        `4` = 'Black',
        `5` = 'White'
      ))
  }

  temp %>%
    lazy_dt() %>%
    mutate(across(starts_with('RACE'), ~as.factor(.x))) %>%
    select(starts_with('DOB'),
           starts_with('DUPERSID'),
           starts_with('AGE'),
           starts_with('RACE'),
           starts_with('SEX'),
           starts_with('VARPSU'), # PSU
           starts_with('VARSTR'), # Strata
           starts_with('INSCOV'), # Insurance coverage variable
           starts_with('HIDEG'), # Highest degree - want only HIDEGYR and HIDEG
           starts_with('EDUYRDG'), #
           starts_with('TTLP'), # Total income
           starts_with('POVCAT'), # Categorical poverty status
           #starts_with('POVLEV'), # Continuous poverty status
           starts_with('REGION'),
           starts_with('DENTCK53'), # Dental checkup, only 2000 - 2016,
           matches('DVTOT[0-9]{2}'), # # of dental care visits in YYYY
           starts_with('CHOLCK'), # Cholesterol check, only 2000 - 2016,
           starts_with('CHECK53'), # Routine check up, only 2000 - 2016,
           starts_with('FLUSHT'), # Flu shot. only 2000 - 2016
           starts_with('DSFL'),
           starts_with('DIAB'), # 3 different variable names for diabetes, all start with DIABDX. this should also grab the DAIBW weight variables
           starts_with(!!eye_var), # Get only the eye variable for each year
           starts_with(!!flu_var),
           starts_with(!!chol_var),
           starts_with(!!feet_var),
           starts_with('DSEYE53'), # Have to get the 2001 eye check variable too
           starts_with('DSA1'), # A1C test,
           starts_with('DSCKFT'), # Feet check, only 2001 - 2007
           starts_with('DSDIA'), # Diagnosed with diabetes by a physician
           starts_with('DIABDX'), # Diabetes diagnosis
           -starts_with('DSEB'), # Remove the eye test in year before columns
           -contains('AGE31X'),
           -contains('AGE42X'),
           -contains('AGE53x'), # Remove some of the extra age variables
           -contains('DSFLNV'), # Remove the never had flu shot
           -contains('REGION31'), # Remove excess region variables
           -contains('REGION42'),
           -contains('REGION53'),
           starts_with('PERWT'), # Person weight level. Not sure I need this, since just working with DCS variables
           starts_with('INSCOP') # Inscope, again not sure if needed but putting here anyway for now
    ) %>%
    as_tibble()
}

# Clean the data
cleanMepsData <- function(dat) {
  dat <- dat %>%
    mutate(
      DSEY0153 = case_when(
        DSEYE53 == 2 ~ 1,
        DSEYE53 %in% c(1, 3, 4, 5) ~ 2,
        DSEYE53 == -9 ~ -9,
        DSEYE53 == -8 ~ -8,
        DSEYE53 == -1 ~ -1,
        TRUE ~ as.numeric(DSEYE53)
      ),
      DSA1C53 = as.factor(
        case_when(
          DSA1C53 == -1 ~ 'Inapplicable',
          DSA1C53 == -7 ~ 'Refused',
          DSA1C53 == -8 ~ 'Do not know',
          DSA1C53 == -9 ~ 'Not ascertained',
          DSA1C53 == -15 ~ 'Cannot be computed',
          DSA1C53 %in% c(0, 1, 96) ~ 'Less than 2 or more A1C tests in a year',
          DSA1C53 >= 2 ~ '2 or more A1C tests in a year',
          TRUE ~ as.character(DSA1C53)
        )
      ),

      DSCKFT53 = as.factor(
        case_when(
          DSCKFT53 >= 1 ~ 'YES',
          DSCKFT53 == 0 ~ 'NO',
          DSCKFT53 == -1 ~ 'Inapplicable',
          DSCKFT53 == -7 ~ 'Refused',
          DSCKFT53 == -8 ~ 'Do not know',
          DSCKFT53 == -9 ~ 'Not ascertained',
          DSCKFT53 == -15 ~ 'Cannot be computed',
        )
      )
    ) %>%
    select(-DSEYE53) %>% # Remove the 2001 eye variable that we recoded
    mutate(
      across(
        c(
          starts_with('DS'),
          CHOLCK53,
          CHECK53,
          FLUSHT53,
          HIDEGYR,
          HIDEG,
          EDUYRDG,
          RACEX,
          RACETHNX,
        ),
        ~ as.factor(.x)
      )
    ) %>%
    # NEed to coalesce DVTOT variables before cleaning
    mutate(all_DVTOT = coalesce(!!!select(., num_range('DVTOT', 17:20)))) %>%
    select(-starts_with('DVTOT'))



  # Recode factors to facilitate working with them
  dat <- dat %>%
    mutate(
      across(
        intersect(starts_with('DSEY'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'YES',
          `2` = 'NO'
        )
      ),
      across(
        starts_with('POVCAT'),
        ~ recode_factor(
          .x,
          `1` = 'Poor/Negative',
          `2` = 'Near poor',
          `3` = 'Low income',
          `4` = 'Middle income',
          `5` = 'High income'
        ) %>%
          fct_collapse(
            `Low income` = c('Low income', 'Near poor')
          )
      ),
      across(
        starts_with('REGION'),
        ~ recode_factor(
          .x,
          `-1` = 'Inapplicable',
          `1` = 'Northeast',
          `2` = 'Midwest',
          `3` = 'South',
          `4` = 'West'
        )
      ),
      across(
        intersect(starts_with('DSCH'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'Within last year',
          `2` = 'NO'
        )
      ),
      across(
        starts_with('INSCOV'),
        ~ recode_factor(
          .x,
          `1` = 'Any private',
          `2` = 'Public only',
          `3` = 'Uninsured'
        )
      ),
      across(
        intersect(starts_with('DSFL'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'Within last year',
          `2` = 'NO'
        )
      ),
      across(
        intersect(starts_with('DSFT'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'YES',
          `2` = 'NO'
        )
      ),
      across(
        starts_with('DIABDX'),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'YES',
          `2` = 'NO'
        )
      ),
      across(
        ends_with('DVTOT'),
        ~ as.factor(case_when(.x >= 1 ~ 1,
                              .x == 0 ~ 0,
                              TRUE ~ -9)) %>%
          recode_factor(`-9` = 'Not ascertained',
                        `1` = 'Once a year or more',
                        `0` = 'Less than once a year')
      ),
      across(
        starts_with('HIDE'),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'No Degree',
          `2` = 'GED',
          `3` = 'HS',
          `4` = 'Bachelor',
          `5` = 'Masters',
          `6` = 'Doctorate',
          `7` = 'Other',
          `8` = 'Under 16, inapplicable'
        ) %>%
          fct_collapse(
            `Less than high school` = c('No Degree', 'GED', 'Under 16, inapplicable'),
            `High school` = c('HS'),
            `Greater than HS` = c('Bachelor', 'Masters', 'Doctorate', 'Other'),
            `Not available` = c('Not ascertained', 'Do not know', 'Refused', 'Inapplicable', 'Cannot be computed')
          )
      ),
      EDUYRDG = recode_factor(
        EDUYRDG,
        `-15` = 'Cannot be computed',
        `-9` = 'Not ascertained',
        `-8` = 'Do not know',
        `-7` = 'Refused',
        `-1` = 'Inapplicable',
        `1` = '<= 8th grade',
        `2` = 'No High School',
        `3` = 'GED',
        `4` = 'High School',
        `5` = 'Some college',
        `6` = 'Associate Occupational/Technical',
        `7` =  'Associate Academic',
        `8` = 'Bachelor',
        `9` = 'Master, Professional, or Doctorate',
        `10` = 'Child under 5 years old'
      ) %>%
        fct_collapse(
          `Less than high school` = c('<= 8th grade', 'No High School', 'GED'),
          `High school` = c('High School', 'Some college'),
          `Greater than HS` = c('Associate Occupational/Technical', 'Associate Academic', 'Bachelor', 'Master, Professional, or Doctorate'),
          `Not available` = c('Not ascertained', 'Do not know', 'Refused', 'Inapplicable', 'Cannot be computed', 'Child under 5 years old')
        ),
      across(
        c(RACEX, RACEV1X),
        ~ recode_factor(
          .x,
          `1` = 'White',
          `2` = 'Black',
          `3` = 'Amer Indian/Alaska Native',
          `4` = 'Asian',
          `5` = 'Native Hawiian/Pacific Islander',
          `6` = 'Multiple races reported',
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
        )
      ),
      RACETHNX = recode_factor(
        RACETHNX,
        `1` = 'Hispanic',
        `2` = 'Black/Not Hispanic',
        `3` = 'Asian/Not Hispanic',
        `4` = 'Other Race/Not Hispanic',
      ) %>% fct_expand('White/Not Hispanic'),
      RACETHX = recode_factor(
        RACETHX,
        `1` = 'Hispanic',
        `2` = 'White/Not Hispanic',
        `3` = 'Black/Not Hispanic',
        `4` = 'Asian/Not Hispanic',
        `5` = 'Other Race/Not Hispanic',
      ),
      # USBORN42 = recode_factor(
      #   USBORN42,
      #   `-15` = 'Cannot be computed',
      #   `-9` = 'Not ascertained',
      #   `-8` = 'Do not know',
      #   `-7` = 'Refused',
      #   `-1` = 'Inapplicable',
      #   `1` = 'Yes',
      #   `2` = 'No'
      # ),
      # BORNUSA = recode_factor(
      #   BORNUSA,
      #   `-15` = 'Cannot be computed',
      #   `-9` = 'Not ascertained',
      #   `-8` = 'Do not know',
      #   `-7` = 'Refused',
      #   `-1` = 'Inapplicable',
      #   `1` = 'Yes',
      #   `2` = 'No'
      # ),
      across(
        c(CHOLCK53, CHECK53, FLUSHT53),
        ~ recode_factor(
          .x,
          `-15` = 'Cannot be computed',
          `-9` = 'Not ascertained',
          `-8` = 'Do not know',
          `-7` = 'Refused',
          `-1` = 'Inapplicable',
          `1` = 'Within last year',
          `2` = 'Within past 2 years',
          `3` = 'Within past 3 years',
          `4` = 'Within past 5 years',
          `5` = 'More than 5 years',
          `6` = 'Never'
        ) %>%
          fct_collapse(
            `Within last 2 or more years` = c(
              'Within past 2 years',
              'Within past 3 years',
              'Within past 5 years',
              'More than 5 years',
              'Never'
            ),
            `Not available` = c(
              'Not ascertained',
              'Do not know',
              'Refused',
              'Inapplicable'
            )
          )
      ),
      DENTCK53 = recode_factor(
        DENTCK53,
        `-15` = 'Cannot be computed',
        `-9` = 'Not ascertained',
        `-8` = 'Do not know',
        `-7` = 'Refused',
        `-1` = 'Inapplicable',
        `1` = 'Twice a year or more',
        `2` = 'Once a year',
        `3` = 'Less than once a year',
        `4` = 'Never go to dentist'
      ) %>%
        fct_collapse(
          `Once a year or more` = c(
            'Once a year',
            'Twice a year or more'
          ),
          `Less than once a year` = c(
            'Less than once a year',
            'Never go to dentist'
          )
        ),
      SEX = recode_factor(SEX,
                          `1` = 'Male',
                          `2` = 'Female')


    )

  dat$RACETHNX[dat$RACEX == 'White'] <- 'White/Not Hispanic'
  return(dat)
}

# Need to coalesce some of the columns to fill out values
coalesceData <- function(dat) {
  dat$DIABDX_all <- coalesce(dat$DIABDX53, dat$DIABDX_M18)
  # Variable to coallesce
  # HIDEGYR/HIDEG
  # POVCAT
  # DIABW variables
  dat <- dat %>%
    mutate(
      DSEY_all = coalesce(!!!select(., starts_with('DSEY'))),
      HIDEG_all = coalesce(!!!select(., starts_with('HIDEG'), starts_with('EDUY'))),
      DIABDX_all = coalesce(!!!select(., starts_with('DIABDX'))),
      DIABW = coalesce(!!!select(., starts_with('DIABW'))),
      PERWT = coalesce(!!!select(., starts_with('PERWT'))),
      POVCAT_all = coalesce(!!!select(., starts_with('POVCAT'))),
      INSCOV_all = coalesce(!!!select(., starts_with('INSCOV'))),
      REGION_all = coalesce(!!!select(., starts_with('REGION'))),
      VARPSU_all = coalesce(!!!select(., starts_with('VARPSU'))),
      VARSTR_all = coalesce(!!!select(., starts_with('VARSTR'))),
      AGE_all = coalesce(!!!select(., starts_with('AGE'))),
      RACE_all = coalesce(!!!select(., RACETHNX, RACETHX)),
      CHOL_all = coalesce(!!!select(., CHOLCK53, starts_with('DSCH'))),
      FLU_all = coalesce(!!!select(., FLUSHT53, starts_with('DSFL'))),
      DENT_all = coalesce(!!!select(., DENTCK53, ends_with('DVTOT'))),
      FEET_all = coalesce(!!!select(., DSCKFT53, starts_with('DSFT'))),

    ) %>%
    mutate(
      AGE_all = as.factor(case_when(
        AGE_all == -1 ~ NA_character_,
        AGE_all < 18 & AGE_all > -1 ~ 'Less than 18',
        AGE_all >= 18 & AGE_all <= 44 ~ '18 to 44',
        AGE_all >= 45 & AGE_all <= 64 ~ '45 to 64',
        AGE_all >= 65 & AGE_all <= 74 ~ '65 to 74',
        AGE_all >= 75 ~ '75+',
        TRUE ~ as.character(AGE_all)
      ))
    ) %>%
    filter(AGE_all != 'Less than 18') %>% # Need to drop this factor level, throwing errors
    filter(REGION_all != 'Inapplicable') %>%
    mutate(AGE_all = forcats::fct_drop(AGE_all),
           REGION_all = forcats::fct_drop(REGION_all))
}

relevelFactors <- function(dat) {
  dat %>%
    mutate(DSEY_all = fct_relevel(DSEY_all, 'YES', 'NO', 'Not ascertained', 'Do not know', 'Cannot be computed'),
           FEET_all = fct_relevel(FEET_all, 'YES', 'NO', 'Not ascertained', 'Do not know', 'Inapplicable', 'Cannot be computed'),
           CHOL_all = fct_relevel(CHOL_all, 'Within last year', 'Within last 2 or more years', 'NO', 'Not ascertained', 'Do not know', 'Inapplicable', 'Cannot be computed'),
           FLU_all = fct_relevel(FLU_all, 'Within last year', 'Within last 2 or more years', 'NO', 'Not ascertained', 'Do not know', 'Refused', 'Inapplicable', 'Cannot be computed'),
           DENT_all = fct_relevel(DENT_all, 'Once a year or more', 'Less than once a year', 'Not ascertained', 'Do not know', 'Refused', 'Inapplicable'),
           DSA1C53 = fct_relevel(DSA1C53, '2 or more A1C tests in a year', 'Less than 2 or more A1C tests in a year', 'Not ascertained', 'Do not know', 'Inapplicable', 'Cannot be computed'),
           POVCAT_all = fct_relevel(POVCAT_all, 'High income', 'Middle income', 'Low income', 'Poor/Negative')
           ) %>%
    as_tibble()
}

# This function calculates proportion of individuals following prventive
# care practices in each year and for the different strata.
calcProp <- function(data, year, variable = NULL) {
  if (is.character(variable)) {
    vari <- enquo(variable)
    data[[variable]] <- forcats::fct_drop(data[[variable]])
    diab_svy <- data %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      )
    npsu <- nrow(unique(diab_svy$cluster))
    nstrata <- nrow(unique(diab_svy$strata))
    df <- npsu - nstrata
    diab_svy %>%
      tab_survey(
         FLU_all, DSA1C53, DSEY_all, FEET_all, CHOL_all, DENT_all,
        pretty = FALSE,
        strata = !!vari,
        wide = FALSE,
        method = 'beta',
        deff = TRUE,
        row_total = TRUE
        #drop = c('Not ascertained', 'Do not know', 'Refused', 'Inapplicable'),
        # keep = c(
        #   'Within last year',
        #   '2 or more A1C tests in a year',
        #   '1 or more foot examinations in a year',
        #   'Once a year',
        #   #Dental checkups
        #   'YES' # eye exam
        # )
      ) %>%
      mutate(year = as.numeric(year),
             df = df)
  } else {
    vari <- variable
    diab_svy <- data %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      )
    npsu <- nrow(unique(diab_svy$cluster))
    nstrata <- nrow(unique(diab_svy$strata))
    df <- npsu - nstrata
    diab_svy %>%
      tab_survey(
        FLU_all, DSA1C53, DSEY_all, FEET_all, CHOL_all, DENT_all,
        pretty = FALSE,
        strata = vari,
        wide = FALSE,
        method = 'beta',
        deff = TRUE,
        row_total = TRUE
        #drop = c('Not ascertained', 'Do not know', 'Refused', 'Inapplicable'),
        # keep = c(
        #   'Within last year',
        #   '2 or more A1C tests in a year',
        #   '1 or more foot examinations in a year',
        #   'Once a year',
        #   #Dental checkups
        #   'YES' # eye exam
        # )
      ) %>%
      mutate(year = as.numeric(year),
             df = df)
  }

}
safelyCalcProp <- safely(calcProp)
possiblyCalcProp <- possibly(calcProp, otherwise = NULL)

# Combine dashboard with MEPS data
combineOverall <- function(meps_data, dash_data) {
  overall_indicator <- meps_data %>%
    mutate(
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      )
    ) %>%
    left_join(.,
              dplyr::filter(dash_data, stratifier == 'total'),
              by = c('year', 'variable' = 'indicator'))
}
combineRace <- function(meps_data, dash_data) {
  race_indicator <- meps_data %>%
    mutate(
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      ),
      RACE_all = case_when(
        RACE_all == 'Hispanic' ~ 'hispanic',
        RACE_all == 'Black/Not Hispanic' ~ 'black',
        RACE_all == 'White/Not Hispanic' ~ 'white',
        RACE_all == 'Asian/Not Hispanic' ~ 'asian',
        TRUE ~ as.character(RACE_all)
      )
    ) %>%
    left_join(
      .,
      dplyr::filter(dash_data, stratifier == 'race'),
      by = c('year', 'variable' = 'indicator', 'RACE_all' = 'strata')
    )

}
combineSex <- function(meps_data, dash_data) {
  sex_indicator <- meps_data %>%
    mutate(
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      ),
      SEX = case_when(SEX == 'Male' ~ 'M',
                      SEX == 'Female' ~ 'F')
    ) %>%
    left_join(
      .,
      dplyr::filter(dash_data, stratifier == 'gender'),
      by = c('year', 'variable' = 'indicator', 'SEX' = 'strata')
    )
}
combineEdu <- function(meps_data, dash_data) {
  edu_indicator <- meps_data %>%
    mutate(
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      ),
      HIDEG_all = case_when(
        HIDEG_all == 'Greater than HS' ~ 'greater-than-HS',
        HIDEG_all == 'Less than high school' ~ 'less-than-HS',
        HIDEG_all == 'High school' ~ 'HS',
        TRUE ~ as.character(HIDEG_all)
      )
    ) %>%
    left_join(
      .,
      dplyr::filter(dash_data, stratifier == 'edu'),
      by = c('year', 'variable' = 'indicator', 'HIDEG_all' = 'strata')
    )
}

combineAge <- function(meps_data, dash_data) {
  age_indicator <- meps_data %>%
    mutate(
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      ),
      HIDEG_all = case_when(
        HIDEG_all == 'Greater than HS' ~ 'greater-than-HS',
        HIDEG_all == 'Less than high school' ~ 'less-than-HS',
        HIDEG_all == 'High school' ~ 'HS',
        TRUE ~ as.character(HIDEG_all)
      )
    ) %>%
    left_join(
      .,
      dplyr::filter(dash_data, stratifier == 'edu'),
      by = c('year', 'variable' = 'indicator', 'HIDEG_all' = 'strata')
    )
}

plotTrends <- function(data, strata) {
  if (is.null(strata) == TRUE) {
    data %>%
      dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                    value %in% c(
                      'Within last year',
                      '2 or more A1C tests in a year',
                      '1 or more foot examinations in a year',
                      'Once a year',
                      'YES'
                    )) %>%
      ggplot() +
      geom_line(aes(
        x = year,
        y = proportion,
        group = variable,
        linetype = 'MEPS'
      )) +
      geom_line(aes(
        x = year,
        y = percentage,
        group = variable,
        linetype = 'Dashboard'
      )) +
      facet_wrap(~ variable) +
      theme_bw()

  } else {

  data %>%
  dplyr::filter(variable %in% c('a1c', 'eye-exam', 'flu', 'foot'),
                value %in% c(
                  'Within last year',
                  '2 or more A1C tests in a year',
                  '1 or more foot examinations in a year',
                  'Once a year',
                  #Dental checkups
                  'YES' # eye exam
                )) %>%
    ggplot(.) +
    geom_line(aes(
      x = year,
      y = proportion,
      group = .data[[strata]],
      color = .data[[strata]],
      linetype = 'MEPS'
    )) +
    geom_line(aes(
      x = year,
      y = percentage,
      group = .data[[strata]],
      color = .data[[strata]],
      linetype = 'Dashboard'
    )) +
    facet_wrap(~ variable) +
    scale_linetype_manual(labels = c('Dashboard', 'MEPS'),
                          values = c('solid', 'dashed')) +
    theme_bw()
  }
}

createTable <- function(survey.object, by = NULL) {
   out <- tbl_svysummary(
      survey.object,
      by = by,
      include = c(CHOL_all,
                  FLU_all,
                  DENT_all,
                  DSA1C53,
                  DSEY_all,
                  FEET_all),
      label = list(DSEY_all ~ 'Dilated eye exam in the last year',
                   CHOL_all ~ 'Cholesterol tested',
                   FLU_all ~ 'Flu shot',
                   DENT_all ~ '2 or more dentist visits in the last year',
                   DSA1C53 ~ '2 or more A1C tests in the last year',
                   FEET_all ~ 'Foot checked')
    )
  out$table_body
}

createTableAllStats <- function(survey.object, by = NULL, data) {
  out_table <- tbl_svysummary(
    survey.object,
    by = by,
    digits = list(everything() ~ 12),
    include = c(CHOL_all,
                FLU_all,
                DENT_all,
                DSA1C53,
                DSEY_all,
                FEET_all),
    label = list(DSEY_all ~ 'Dilated eye exam in the last year',
                 CHOL_all ~ 'Cholesterol tested',
                 FLU_all ~ 'Flu shot',
                 DENT_all ~ '2 or more dentist visits in the last year',
                 DSA1C53 ~ '2 or more A1C tests in the last year',
                 FEET_all ~ 'Foot checked'),
    statistic = list(all_categorical() ~ "{n}-{N}-{p}-{p.std.error}-{n_unweighted}-{N_unweighted}-{p_unweighted}")
  )
  out_table$table_body <- out_table$table_body %>%
    mutate(
      year = data$year[1],
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      )
    )
  if (is.null(by) == FALSE) {
    names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
      levels(data[[by]])
  }
  return(out_table$table_body)

}

createTableAllStatsbyAge <- function(survey.object, by = NULL, data) {
  out_table <- tbl_strata(
    survey.object,
    strata = AGE_all,
    .tbl_fun =
      ~.x %>%
      tbl_svysummary(
        by = by,
        digits = list(everything() ~ 12),
        include = c(CHOL_all,
                    FLU_all,
                    DENT_all,
                    DSA1C53,
                    DSEY_all,
                    FEET_all),
        label = list(
          DSEY_all ~ 'Dilated eye exam in the last year',
          CHOL_all ~ 'Cholesterol tested',
          FLU_all ~ 'Flu shot',
          DENT_all ~ '2 or more dentist visits in the last year',
          DSA1C53 ~ '2 or more A1C tests in the last year',
          FEET_all ~ 'Foot checked'
        ),
        statistic = list(
          all_categorical() ~ "{n}-{N}-{p}-{p.std.error}-{n_unweighted}-{N_unweighted}-{p_unweighted}"
        )
      )
  )

  out_table$table_body <- out_table$table_body %>%
    mutate(
      year = data$year[1],
      variable = case_when(
        variable == 'FLU_all' ~ 'flu',
        variable == 'DSEY_all' ~ 'eye-exam',
        variable == 'DSA1C53' ~ 'a1c',
        variable == 'FEET_all' ~ 'foot',
        TRUE ~ variable
      )
    )
  if (is.null(by) == FALSE) {
    names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
      paste(levels(data[[by]]), rep(c('18-44', '45-64', '65-74', '75+'), each = length(levels(data[[by]]))), sep = "_")
  } else {
    names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
      paste("stat_0", rep(c('18-44', '45-64', '65-74', '75+'), each = 1), sep = "_")
  }
  return(out_table$table_body)

}

makeTablesLonger <- function(table) {
  col_names <- c('n', 'N', 'p', 'p.std.error', 'n_unweighted', 'N_unweighted', 'p_unweighted')
  table %>%
    filter(row_type != 'label') %>%
    pivot_longer(cols = -c(variable, var_type, var_label, row_type, label, year),
                 names_to = "strata",
                 values_to = 'parameters') %>%
    separate(parameters, into = col_names, sep = "-") %>%
    mutate(across(n:p_unweighted, ~as.numeric(gsub(",", "", .x))))

}

makeTablesLongerbyAge <- function(table) {
  col_names <- c('n', 'N', 'p', 'p.std.error', 'n_unweighted', 'N_unweighted', 'p_unweighted')
  table %>%
    filter(row_type != 'label') %>%
    pivot_longer(cols = -c(variable, starts_with('var_type'), var_label, row_type, label, year),
                 names_to = c("strata", 'age'),
                 names_pattern = '(.*)_(.*)',
                 values_to = 'parameters') %>%
    separate(parameters, into = col_names, sep = "-") %>%
    mutate(across(n:p_unweighted, ~as.numeric(gsub(",", "", .x))))

}

ageAdjustTables <- function(data) {
  age_prop_table <- tibble(age = c('18-44', '45-64', '65-74', '75+'),
                           age_prop = c(0.530535, 0.299194, 0.088967, 0.081304))
  data %>%
    left_join(age_prop_table, by = 'age') %>%
    mutate(age_adjusted_prop = (p / 100) * age_prop) %>%
    group_by(variable, label, strata, year) %>%
    summarise(age_adjusted_prop = sum(age_adjusted_prop, na.rm = TRUE))
}

# Need to join the estimates from tbl_svysummary to the rest of the
# estimates for all the relevant parameters for determining suppression.
joinTables <- function(indicator_table, suppression_table, variable = NULL) {
  if (is.null(variable)) {
    left_join(
      indicator_table,
      mutate(
        suppression_table,
        year = as.numeric(year),
        variable = case_when(variable == 'FLU_all' ~ 'flu',
                             variable == 'DSA1C53' ~ 'a1c',
                             variable == 'DSEY_all' ~ 'eye-exam',
                             variable == 'FEET_all' ~ 'foot',
                             TRUE ~ variable)
      ),
      by = c('variable', 'value' = 'label', 'year')
    )
  }
  else if (variable == 'sex') {
    left_join(
      indicator_table,
      mutate(
        suppression_table,
        strata = case_when(strata == 'Male' ~ 'M', strata == 'Female' ~ 'F'),
        year = as.numeric(year),
        variable = case_when(variable == 'FLU_all' ~ 'flu',
                             variable == 'DSA1C53' ~ 'a1c',
                             variable == 'DSEY_all' ~ 'eye-exam',
                             variable == 'FEET_all' ~ 'foot',
                             TRUE ~ variable)
      ),
      by = c('variable', 'value' = 'label', 'year', 'SEX' = 'strata')
    )
  } else if (variable == 'race') {
    left_join(
      indicator_table,
      mutate(
        suppression_table,
        strata = case_when(strata == 'Hispanic' ~ 'hispanic',
                           strata == 'Black/Not Hispanic' ~ 'black',
                           strata == 'Asian/Not Hispanic' ~ 'asian',
                           strata == 'White/Not Hispanic' ~ 'white',
                           TRUE ~ strata),
        year = as.numeric(year),
        variable = case_when(variable == 'FLU_all' ~ 'flu',
                             variable == 'DSA1C53' ~ 'a1c',
                             variable == 'DSEY_all' ~ 'eye-exam',
                             variable == 'FEET_all' ~ 'foot',
                             TRUE ~ variable)
      ),
      by = c('variable', 'value' = 'label', 'year', 'RACE_all' = 'strata')
    )
  } else if (variable == 'edu') {
    left_join(
      indicator_table,
      mutate(
        suppression_table,
        strata = case_when(strata == 'Less than high school' ~ 'less-than-HS',
                           strata == 'High school' ~ 'HS',
                           strata == 'Greater than HS' ~ 'greater-than-HS',
                           TRUE ~ strata),
        year = as.numeric(year),
        variable = case_when(variable == 'FLU_all' ~ 'flu',
                             variable == 'DSA1C53' ~ 'a1c',
                             variable == 'DSEY_all' ~ 'eye-exam',
                             variable == 'FEET_all' ~ 'foot',
                             TRUE ~ variable)
      ),
      by = c('variable', 'value' = 'label', 'year', 'HIDEG_all' = 'strata')
    )
  }
}

suppressData <- function(data) {
  data %>%
    mutate(
      var = p.std.error ^ 2,
      num = p * (100 - p),
      ess = num / var,
      rse = (p.std.error / p) * 100,
      abs_ci_width = (upper_limit * 100) - (lower_limit * 100),
      rel_ci_width = (abs_ci_width / p) * 100
    ) %>%
  mutate(
    FLAG = case_when(
      N < 30 | ess < 30 ~ 'Nominal or Effective Sample Size < 30',
      abs_ci_width >= 30 ~ 'Absolute value of CI Width >= 30',
      abs_ci_width <= 5 ~ 'Absolute value of CI Width <= 5',
      rel_ci_width > 130 ~ 'Relative CI Width >130% of proportion',
      n.x == 0 ~ 'Number of events = 0',
      df < 8 ~ 'Degress of freedom < 8',
      TRUE ~ 'No flag'
    ),
    ACTION = case_when(
      FLAG %in% c('Nominal or Effective Sample Size < 30', 'Absolute value of CI Width >= 30') ~ 'Suppress',
      FLAG == 'Number of events = 0' | (FLAG != 'Number of events = 0' & FLAG == 'Degress of freedom < 8') ~ 'Review',
      FLAG == 'Relative CI Width >130% of proportion' ~ 'Suppress',
      TRUE ~ 'Okay'
    ),
    proportion = case_when(
      ACTION == 'Supress' ~ NA_real_,
      TRUE ~ proportion
    )
  )
}

cumulativePractices <- function(data) {
  data %>%
    # Create columns that binary indicate if each individual met the appropriate practice (1) or not (0)
    mutate(chol_bin = case_when(CHOL_all == 'Within last year' ~ 1, TRUE ~ 0),
           a1c_bin = case_when(DSA1C53 == '2 or more A1C tests in a year' ~ 1, TRUE ~ 0),
           eye_bin = case_when(DSEY_all == 'YES' ~ 1, TRUE ~ 0),
           feet_bin = case_when(FEET_all == 'YES' ~ 1, TRUE ~ 0),
           dent_bin = case_when(DENT_all == 'Once a year or more' ~ 1, TRUE ~ 0),
           flu_bin = case_when(FLU_all == 'Within last year' ~ 1, TRUE ~ 0)
           ) %>%
    rowwise() %>%
    mutate(sum_practices = sum(c_across(ends_with('bin')))) %>%
    ungroup() %>%
    mutate(zero_practices = case_when(sum_practices == 0 ~ 1, TRUE ~ 0),
           one_practices = case_when(sum_practices == 1 ~ 1, TRUE ~ 0),
           two_practices = case_when(sum_practices == 2 ~ 1, TRUE ~ 0),
           three_practices = case_when(sum_practices == 3 ~ 1, TRUE ~ 0),
           four_practices = case_when(sum_practices == 4 ~ 1, TRUE ~ 0),
           five_practices = case_when(sum_practices == 5 ~ 1, TRUE ~ 0),
           six_practices = case_when(sum_practices == 6 ~ 1, TRUE ~ 0),
           atleast_one_practices = case_when(sum_practices >= 1 ~ 1, TRUE ~ 0),
           atleast_two_practices = case_when(sum_practices >= 2 ~ 1, TRUE ~ 0),
           atleast_three_practices = case_when(sum_practices >= 3 ~ 1, TRUE ~ 0),
           atleast_four_practices = case_when(sum_practices >= 4 ~ 1, TRUE ~ 0),
           atleast_five_practices = case_when(sum_practices >= 5 ~ 1, TRUE ~ 0),
           atleast_six_practices = case_when(sum_practices >= 6 ~ 1, TRUE ~ 0)) %>%
    select(-ends_with('bin'))
}

preventivePracticebyAge <- function(survey.object, by = NULL, data) {
  if (is.null(by) || by != 'AGE_all'){
  out_table <- tbl_strata(
    survey.object,
    strata = AGE_all,
    .tbl_fun =
      ~.x %>%
      tbl_svysummary(
        by = by,
        digits = list(everything() ~ 12),
        include = c(
          #zero_practices,
          #one_practices,
          #two_practices,
          three_practices,
          #four_practices,
          #five_practices,
          #six_practices,
          #atleast_one_practices,
          #atleast_two_practices,
          atleast_three_practices
          #atleast_four_practices,
          #atleast_five_practices,
          #atleast_six_practices
        ),
        label = list(
          #zero_practices ~ "No preventive practices followed",
          #one_practices ~ "One preventive practice followed",
          #two_practices ~ "Two preventive practices followed",
          three_practices ~ "Three preventive practices followed",
          #four_practices ~ "Four preventive practices followed",
          #five_practices ~ "Five preventive practices followed",
          #six_practices ~ "Six preventive practices followed",
          #atleast_one_practices ~ "At least one preventive practice followed",
          #atleast_two_practices ~ "At least two preventive practices followed",
          atleast_three_practices ~ "At least three preventive practices followed"
          #atleast_four_practices ~ "At least four preventive practices followed",
          #atleast_five_practices ~ "At least five preventive practices followed",
          #atleast_six_practices ~ "At least six preventive practices followed"
        ),
        statistic = list(
          all_categorical() ~ "{n}-{N}-{p}-{p.std.error}-{n_unweighted}-{N_unweighted}-{p_unweighted}"
        )
      )
  )

  out_table$table_body <- out_table$table_body %>%
    mutate(
      year = data$year[1]
    )
  if (is.null(by) == FALSE) {
    names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
      paste(levels(data[[by]]), rep(c('18-44', '45-64', '65-74', '75+'), each = length(levels(data[[by]]))), sep = "_")
  } else {
    names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
      paste("stat_0", rep(c('18-44', '45-64', '65-74', '75+'), each = 1), sep = "_")
  }
  return(out_table$table_body)
  }
   else {
    out_table <- tbl_svysummary(
      survey.object,
      by = by,
      digits = list(everything() ~ 12),
      include = c(
        #zero_practices,
        #one_practices,
        #two_practices,
        three_practices,
        #four_practices,
        #five_practices,
        #six_practices,
        #atleast_one_practices,
        #atleast_two_practices,
        atleast_three_practices
        #atleast_four_practices,
        #atleast_five_practices,
        #atleast_six_practices
      ),
      label = list(
        #zero_practices ~ "No preventive practices followed",
        #one_practices ~ "One preventive practice followed",
        #two_practices ~ "Two preventive practices followed",
        three_practices ~ "Three preventive practices followed",
        #four_practices ~ "Four preventive practices followed",
        #five_practices ~ "Five preventive practices followed",
        #six_practices ~ "Six preventive practices followed",
        #atleast_one_practices ~ "At least one preventive practice followed",
        #atleast_two_practices ~ "At least two preventive practices followed",
        atleast_three_practices ~ "At least three preventive practices followed"
        #atleast_four_practices ~ "At least four preventive practices followed",
        #atleast_five_practices ~ "At least five preventive practices followed",
        #atleast_six_practices ~ "At least six preventive practices followed"
      ),
      statistic = list(all_categorical() ~ "{n}-{N}-{p}-{p.std.error}-{n_unweighted}-{N_unweighted}-{p_unweighted}")
    )
    out_table$table_body <- out_table$table_body %>%
      mutate(
        year = data$year[1]
      )
    if (is.null(by) == FALSE) {
      names(out_table$table_body)[str_detect(names(out_table$table_body), pattern = 'stat')] <-
        levels(data[[by]])
    }
    return(out_table$table_body)
  }
}
makePreventiveTablesLongerbyAge <- function(table) {
  col_names <- c('n', 'N', 'p', 'p.std.error', 'n_unweighted', 'N_unweighted', 'p_unweighted')
  table %>%
    pivot_longer(cols = -c(variable, starts_with('var_type'), var_label, row_type, label, year),
                 names_to = c("strata", 'age'),
                 names_pattern = '(.*)_(.*)',
                 values_to = 'parameters') %>%
    separate(parameters, into = col_names, sep = "-") %>%
    mutate(across(n:p_unweighted, ~as.numeric(gsub(",", "", .x))))

}
# For making the preventive table longer for age. Too lazy to give it a good name.
makePreventiveTablesLongerbyAge2 <- function(table) {
  col_names <- c('n', 'N', 'p', 'p.std.error', 'n_unweighted', 'N_unweighted', 'p_unweighted')
  table %>%
    pivot_longer(cols = -c(variable, var_type, var_label, row_type, label, year),
                 names_to = "strata",
                 values_to = 'parameters') %>%
    separate(parameters, into = col_names, sep = "-") %>%
    mutate(across(n:p_unweighted, ~as.numeric(gsub(",", "", .x))))

}
