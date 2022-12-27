library(tidyverse)
library(nih.joinpoint)
library(targets)

nih_sample_data %>% group_by(sex) %>% slice(1, 2, n()-1, n()) #first 2 and last 2 of each group


ggplot(nih_sample_data, aes(x=year, y=rate, color=sex)) + geom_point() + geom_line()

run_opt = run_options(model="ln", max_joinpoints=4, n_cores=3)
export_opt = export_options()

jp = joinpoint(nih_sample_data, x=year, y=rate, by=sex, se=se,
               run_opts=run_opt, export_opts=export_opt, dir = 'temp')
names(jp)

tar_load(age_adjusted_stats)


stats_table_plot_dat <- age_adjusted_stats %>%
  filter(strata != 'Total') %>%
  filter(strata != 'Not available') %>%
  filter(strata != 'Other Race/Not Hispanic') %>%
  mutate(variable_text = case_when(variable == 'a1c' ~ '2 or more A1C tests',
                                   variable == 'CHOL_all' ~ 'Cholesterol tested',
                                   variable == 'DENT_all' ~ '1 or more dentist visits',
                                   variable == 'eye-exam' ~ 'Eye exam with dilation',
                                   variable == 'flu' ~ 'Received flu vaccine',
                                   variable == 'foot' ~ 'Foot examination'),
         strata = case_when(strata == 'stat_0' ~ 'Overall',
                            TRUE ~ strata)) %>%
  mutate(strata = as.factor(strata),
         p_100 = p / 100,
         year_adj = year - 2007) %>%
  split(., f = ~.$stratifier + .$variable)

run_opt = run_options(
  model = "ln",
  min_joinpoints = 0,
  max_joinpoints = 3,
  n_cores = 3,
  # min_obs_end = 1,
  # min_obs_between = 1,
  het_error = 'constant variance',
  dependent_variable_type = 'proportion'
)
export_opt = export_options()
jp2 = joinpoint(
  stats_table_plot_dat$insurance.CHOL_all,
  x = year,
  y = p,
  by = strata,
  verbose = F,
  run_opts = run_opt,
  export_opts = export_opt,
  se = NULL,
  dir = 'temp'
)
jp2
jp2$apc
summary(jp2)
jp_plot(jp2)
