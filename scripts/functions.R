
library(tidyverse)
library(haven)

readGraspData <- function(filepath) {
  if (file.exists(filepath)) {
  haven::read_sas(filepath)
  } else {
    print('File not found')
  }
}


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
    mutate(across(starts_with('RACE'), ~as.factor(.x))) %>%
    select(starts_with('DOB'),
           starts_with('DUPERSID'),
           starts_with('USBORN'),
           starts_with('BORN'), # Two different born in USA variables used in different years
           starts_with('AGE'),
           starts_with('RACE'),
           starts_with('SEX'),
           starts_with('VARPSU'), # PSU
           starts_with('VARSTR'), # Strata
           # starts_with('INSCOV'), # Insurance coverage variable
           starts_with('HIDEG'), # Highest degree - want only HIDEGYR and HIDEG
           starts_with('HIDEG'), # Highest degree when first entered MEPS
           starts_with('TTLP'), # Total income
           starts_with('POVCAT'), # Categorical poverty status
           #starts_with('POVLEV'), # Continuous poverty status
           #starts_with('REGION'),
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
           -starts_with('DSEB'), # Remove the eye test in year before columns
           -contains('AGE31X'),
           -contains('AGE42X'),
           -contains('AGE53x'), # Remove some of the extra age variables
           starts_with('PERWT'), # Person weight level. Not sure I need this, since just working with DCS variables
           starts_with('INSCOP') # Inscope, again not sure if needed but putting here anyway for now
    )
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
          DSA1C53 == -8 ~ 'DK',
          DSA1C53 == -9 ~ 'Not ascertained',
          DSA1C53 == -15 ~ 'Cannot be computed',
          DSA1C53 %in% c(0, 1, 96) ~ 'Less than 2 or more A1C tests in a year',
          DSA1C53 >= 2 ~ '2 or more A1C tests in a year',
          TRUE ~ as.character(DSA1C53)
        )
      ),

      DSCKFT53 = as.factor(
        case_when(
          DSCKFT53 >= 1 ~ 'Within last year',
          DSCKFT53 == 0 ~ 'No foot examinations in a year',
          DSCKFT53 == -1 ~ 'Inapplicable',
          DSCKFT53 == -7 ~ 'Refused',
          DSCKFT53 == -8 ~ 'DK',
          DSCKFT53 == -9 ~ 'Not ascertained'
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
          RACEX,
          RACETHNX,
          USBORN42
        ),
        ~ as.factor(.x)
      )
    )



  # Recode factors to facilitate working with them
  dat <- dat %>%
    mutate(
      across(
        intersect(starts_with('DSEY'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-9` = 'Not ascertained',
          `-8` = 'DK',
          `-1` = 'Inapplicable',
          `1` = 'YES',
          `2` = 'NO'
        )
      ),
      across(
        intersect(starts_with('DSCH'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-9` = 'Not ascertained',
          `-8` = 'DK',
          `-1` = 'Inapplicable',
          `1` = 'Within last year',
          `2` = 'NO'
        )
      ),
      across(
        intersect(starts_with('DSFL'),
                  ends_with('53')),
        ~ recode_factor(
          .x,
          `-9` = 'Not ascertained',
          `-8` = 'DK',
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
          `-9` = 'Not ascertained',
          `-8` = 'DK',
          `-1` = 'Inapplicable',
          `1` = 'Within last year',
          `2` = 'NO'
        )
      ),
      across(
        starts_with('DVTOT'),
        ~ as.factor(case_when(.x >= 1 ~ 1,
                              TRUE ~ 0)) %>%
          recode_factor(`1` = 'Once a year',
                        `0` = 'Less than once a year')
      ),
      across(
        starts_with('HIDE'),
        ~ recode_factor(
          .x,
          `-9` = 'Not ascertained',
          `-8` = 'DK',
          `-7` = 'Refused',
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
            `Not available` = c('Not ascertained', 'DK', 'Refused', '-1', '-15')
          )
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
          `-9` = 'Not ascertained',
          `-8` = 'DK',
          `-7` = 'Refused'
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
      USBORN42 = recode_factor(
        USBORN42,
        `-9` = 'Not ascertained',
        `-8` = 'DK',
        `-7` = 'Refused',
        `-1` = 'Inapplicable',
        `1` = 'Yes',
        `2` = 'No'
      ),
      BORNUSA = recode_factor(
        BORNUSA,
        `-15` = 'Cannot be computed',
        `-9` = 'Not ascertained',
        `-8` = 'DK',
        `-7` = 'Refused',
        `-1` = 'Inapplicable',
        `1` = 'Yes',
        `2` = 'No'
      ),
      across(
        c(CHOLCK53, CHECK53, FLUSHT53),
        ~ recode_factor(
          .x,
          `-9` = 'Not ascertained',
          `-8` = 'DK',
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
            `Within last 2 more more years` = c(
              'Within past 2 years',
              'Within past 3 years',
              'Within past 5 years',
              'More than 5 years',
              'Never'
            ),
            `Not available` = c(
              'Not ascertained',
              'DK',
              'Refused',
              'Inapplicable'
            )
          )
      ),
      DENTCK53 = recode_factor(
        DENTCK53,
        `-9` = 'Not ascertained',
        `-8` = 'DK',
        `-7` = 'Refused',
        `-1` = 'Inapplicable',
        `1` = 'Twice a year or more',
        `2` = 'Once a year',
        `3` = 'Less than once a year',
        `4` = 'Never go to dentist'
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
      HIDEG_all = coalesce(!!!select(., starts_with('HIDEG'))),
      DIABW = coalesce(!!!select(., starts_with('DIABW'))),
      POVCAT_all = coalesce(!!!select(., starts_with('POVCAT'))),
      USBORN_all = coalesce(!!!select(., contains('BORN'))),
      VARPSU_all = coalesce(!!!select(., starts_with('VARPSU'))),
      VARSTR_all = coalesce(!!!select(., starts_with('VARSTR'))),
      AGE_all = coalesce(!!!select(., starts_with('AGE'))),
      RACE_all = coalesce(!!!select(., RACETHNX, RACETHX)),
      CHOL_all = coalesce(!!!select(., CHOLCK53, starts_with('DSCH'))),
      FLU_all = coalesce(!!!select(., FLUSHT53, starts_with('DSFL'))),
      DENT_all = coalesce(!!!select(., DENTCK53, num_range('DVTOT', 17:20))),
      FEET_all = coalesce(!!!select(., DSCKFT53, starts_with('DSFT'))),

    ) %>%
    mutate(
      AGE_all = as.factor(case_when(
        AGE_all == -1 ~ NA_character_,
        AGE_all < 18 & AGE_all > -1 ~ NA_character_,
        AGE_all >= 18 & AGE_all <= 44 ~ '18 to 44',
        AGE_all >= 45 & AGE_all <= 64 ~ '45 to 64',
        AGE_all >= 65 & AGE_all <= 74 ~ '65 to 74',
        AGE_all >= 75 ~ '75+',
        TRUE ~ as.character(AGE_all)
      ))
    )
}

calcProp <- function(data, year, variable) {
  if (is.character(variable)) {
    vari <- enquo(variable)
    diab_svy <- data %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      )
    diab_svy %>%
      tab_survey(
        CHOL_all, FLU_all, DENT_all, DSA1C53, DSEY_all, FEET_all, #CHECK53 - not in all years
        pretty = FALSE,
        strata = !!vari,
        wide = FALSE,
        method = 'mean'
        #drop = c('Not ascertained', 'DK', 'Refused', 'Inapplicable'),
        # keep = c(
        #   'Within last year',
        #   '2 or more A1C tests in a year',
        #   '1 or more foot examinations in a year',
        #   'Once a year',
        #   #Dental checkups
        #   'YES' # eye exam
        # )
      ) %>%
      mutate(year = year)
  } else {
    vari <- variable
    diab_svy <- data %>%
      as_survey_design(
        ids = VARPSU_all,
        weights = DIABW,
        strata = VARSTR_all,
        nest = TRUE
      )
    diab_svy %>%
      tab_survey(
        CHOL_all, FLU_all, DENT_all, DSA1C53, DSEY_all, FEET_all, #CHECK53 - not in all years
        pretty = FALSE,
        strata = vari,
        wide = FALSE,
        method = 'mean'
        #drop = c('Not ascertained', 'DK', 'Refused', 'Inapplicable'),
        # keep = c(
        #   'Within last year',
        #   '2 or more A1C tests in a year',
        #   '1 or more foot examinations in a year',
        #   'Once a year',
        #   #Dental checkups
        #   'YES' # eye exam
        # )
      ) %>%
      mutate(year = as.numeric(year))
  }

}
safelyCalcProp <- safely(calcProp)
possiblyCalcProp <- possibly(calcProp, otherwise = NULL)
