if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)
if (!require('mice')) install.packages('mice')
library(mice)
if (!require('furrr')) install.packages('furrr')
library(furrr)
if (!require('future')) install.packages('future')
library(future)
if (!require('progressr')) install.packages('progressr')
library(progressr)

data_dir <- './data/'
output_dir <- './output_0519/'
ver <- '250519'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))

cols_to_keep <- paste0('invasive_mbp_lpf_offset_minus', c(30, 25, 20, 15, 10, 5))

mbp_pre_wide <- complete_df %>%
  filter(offset >= -30 & offset <= 0) %>%
  select(icu_stay_id, offset, invasive_mbp_lpf) %>%
  pivot_wider(names_from = offset, values_from = invasive_mbp_lpf,
              names_prefix = 'invasive_mbp_lpf_offset_') %>% 
  rename_with(~ gsub('-', 'minus', .x)) %>% 
  select(-matches('^invasive_mbp_lpf_offset_minus\\d+$') |
           all_of(cols_to_keep)) %>% 
  left_join(df_base, by = 'icu_stay_id')

df_for_imputation <- mbp_pre_wide %>% 
  select(-c(icu_stay_id, past_vent, invasive_mbp_lpf_offset_0))

imp.meth <- map_chr(df_for_imputation, ~ ifelse(any(is.na(.x)), 'norm', ''))
print(imp.meth)

n_imputations <- 15
list_df_imp <- mice(df_for_imputation, m=n_imputations, maxit=50, meth=imp.meth, seed=813)

if (!dir.exists(paste0(data_dir, 'mi_data/'))) {
  dir.create(paste0(data_dir, 'mi_data/'), recursive = TRUE)
}

for (i in 1:n_imputations) {
  df_imp <- mice::complete(list_df_imp, i)
  
  df_imp <- bind_cols(df_imp, mbp_pre_wide %>% select(icu_stay_id, past_vent))
  
  df_post <- complete_df %>%
    filter(offset >= 1 & offset <= 120) %>%
    mutate(invasive_mbp_lpf_diff = invasive_mbp_lpf - invasive_mbp_time0_value) %>%
    select(icu_stay_id, offset, invasive_mbp_lpf_diff) %>%
    left_join(df_imp, by = 'icu_stay_id')
  
  assign(paste0('df_', i), df_post)
  rm(df_imp)
  rm(df_post)
  save(list = paste0('df_', i), file = paste0(data_dir, 'mi_data/imp', i, '.RData'))
}

predict_slope_max <- function(df, model) {
  df <- df %>% mutate(interv = -1)
  
  df_1 <- df %>% 
    mutate(
      interv = 1,
      gamma = 0.025
    )
  
  df_2 <- df %>% 
    mutate(
      interv = 2,
      gamma = 0.05
    )
  
  df_3 <- df %>% 
    mutate(
      interv = 3,
      gamma = 0.1
    )
  
  df_onesample <- bind_rows(df, df_1, df_2, df_3)
  
  df_onesample <- df_onesample %>%
    mutate(
      predicted_mbp = predict(model, df_onesample, type = 'response')
    )
  
  predicted_df_15 <- tibble(
    low_dose = df_onesample %>%
      filter(interv == 1 & offset == 15) %>%
      pull(predicted_mbp) %>%
      mean,
    intermediate_dose = df_onesample %>%
      filter(interv == 2 & offset == 15) %>%
      pull(predicted_mbp) %>%
      mean,
    high_dose = df_onesample %>%
      filter(interv == 3 & offset == 15) %>% 
      pull(predicted_mbp) %>%
      mean
  )
  
  predicted_df_15 <- predicted_df_15 %>% 
    mutate(
      low_dose = round(low_dose/15, 4),
      intermediate_dose = round(intermediate_dose/15, 4),
      high_dose = round(high_dose/15, 4),
      high_minus_low = high_dose - low_dose,
      high_minus_intermediate = high_dose - intermediate_dose
    )
  
  predicted_df_120_max <- tibble(
    low_dose = df_onesample %>%
      filter(interv == 1) %>%
      group_by(icu_stay_id) %>%
      summarise(max_predicted_mbp = max(predicted_mbp), .groups = 'drop') %>%
      pull(max_predicted_mbp) %>% 
      mean %>% 
      round(3),
    intermediate_dose = df_onesample %>% 
      filter(interv == 2) %>%
      group_by(icu_stay_id) %>%
      summarise(max_predicted_mbp = max(predicted_mbp), .groups = 'drop') %>%
      pull(max_predicted_mbp) %>% 
      mean %>% 
      round(3),
    high_dose = df_onesample %>%
      filter(interv == 3) %>%
      group_by(icu_stay_id) %>%
      summarise(max_predicted_mbp = max(predicted_mbp), .groups = 'drop') %>%
      pull(max_predicted_mbp) %>% 
      mean %>% 
      round(3)
  )
  
  predicted_df_120_max <- predicted_df_120_max %>% 
    mutate(
      high_minus_low = high_dose - low_dose,
      high_minus_intermediate = high_dose - intermediate_dose
    )
  return(list(t15 = predicted_df_15,
              t120 = predicted_df_120_max))
}

gam_slope_max_overall <- function(df) {
  gam_mbp_overall <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
                           sepsis + first_sofa + on_vent + past_vent +
                           ph + bicarbonate + base_excess + pco2 + lactate +
                           hr_pre + s(gamma) + s(invasive_mbp_time0_value) +
                           ti(gamma, offset) +
                           invasive_mbp_lpf_offset_minus30 +
                           invasive_mbp_lpf_offset_minus25 +
                           invasive_mbp_lpf_offset_minus20 +
                           invasive_mbp_lpf_offset_minus15 +
                           invasive_mbp_lpf_offset_minus10 +
                           invasive_mbp_lpf_offset_minus5,
                         data = df,
                         family = gaussian(link = 'identity'),
                         method = 'REML')
  
  results_overall <- predict_slope_max(df, gam_mbp_overall)
  
  return(results_overall)
}

gam_slope_subgroup <- function(df) {
  
  df_sepsis <- df %>% filter(sepsis == 1) %>% select(-sepsis)
  df_non_sepsis <- df %>% filter(sepsis == 0) %>% select(-sepsis)
  
  gam_mbp_sepsis <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
                          first_sofa + on_vent + past_vent +
                          ph + bicarbonate + base_excess + pco2 + lactate +
                          hr_pre + s(gamma) + s(invasive_mbp_time0_value) +
                          ti(gamma, offset) +
                          invasive_mbp_lpf_offset_minus30 +
                          invasive_mbp_lpf_offset_minus25 +
                          invasive_mbp_lpf_offset_minus20 +
                          invasive_mbp_lpf_offset_minus15 +
                          invasive_mbp_lpf_offset_minus10 +
                          invasive_mbp_lpf_offset_minus5,
                        data = df_sepsis,
                        family = gaussian(link = 'identity'),
                        method = 'REML')
  
  gam_mbp_non_sepsis <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
                              first_sofa + on_vent + past_vent +
                              ph + bicarbonate + base_excess + pco2 + lactate +
                              hr_pre + s(gamma) + s(invasive_mbp_time0_value) +
                              ti(gamma, offset) +
                              invasive_mbp_lpf_offset_minus30 +
                              invasive_mbp_lpf_offset_minus25 +
                              invasive_mbp_lpf_offset_minus20 +
                              invasive_mbp_lpf_offset_minus15 +
                              invasive_mbp_lpf_offset_minus10 +
                              invasive_mbp_lpf_offset_minus5,
                            data = df_non_sepsis,
                            family = gaussian(link = 'identity'),
                            method = 'REML')
  
  pred_sepsis     <- predict_slope_max(df_sepsis,     gam_mbp_sepsis)
  pred_non_sepsis <- predict_slope_max(df_non_sepsis, gam_mbp_non_sepsis)
  
  dose_cols <- c("high_dose", "intermediate_dose", "low_dose")
  
  make_tbl <- function(tp) {
    
    sepsis_tbl <- pred_sepsis[[tp]] %>%
      select(all_of(dose_cols)) %>%
      rename_with(~ paste0(.x, "_sepsis"))
    
    non_tbl <- pred_non_sepsis[[tp]] %>%
      select(all_of(dose_cols)) %>%
      rename_with(~ paste0(.x, "_non_sepsis"))
    
    bind_cols(sepsis_tbl, non_tbl) %>%
      mutate(
        non_sepsis_minus_sepsis_high_dose = high_dose_non_sepsis - high_dose_sepsis,
        non_sepsis_minus_sepsis_intermediate_dose = intermediate_dose_non_sepsis - intermediate_dose_sepsis,
        non_sepsis_minus_sepsis_low_dose = low_dose_non_sepsis - low_dose_sepsis
      )
  }
  
  results_subgrop <- list(
    t15  = make_tbl("t15"),
    t120 = make_tbl("t120")
  )
  
  return(results_subgrop)
}

plan(multisession, workers = 10)
handlers('progress') 

bootstrap_once <- function(df) {
  unique_ids <- df %>% distinct(icu_stay_id)
  
  resample_ids <- unique_ids %>%
    slice_sample(n = nrow(unique_ids), replace = TRUE)
  
  resample_df <- resample_ids %>% 
    left_join(df, by = 'icu_stay_id', relationship = 'many-to-many')
  
  results_overall <- gam_slope_max_overall(resample_df)
  results_subgroup <- gam_slope_subgroup(resample_df)
  
  return(list(t15_overall = results_overall$t15,
              t120_overall = results_overall$t120,
              t15_subgroup = results_subgroup$t15,
              t120_subgroup = results_subgroup$t120))
}

bootstrap_gam <- function(df, n_iter) {
  
  future_map(
    seq_len(n_iter),
    ~ bootstrap_once(df),
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )
}

run_all_imputations <- function(data_dir,
                                n_imputations,
                                n_iter) {
  
  summarise_mv <- function(x) {
    bind_rows(x) %>%
      summarise(across(everything(),
                       list(mean = ~mean(.x, na.rm = TRUE),
                            var  = ~var(.x,  na.rm = TRUE))))
  }
  
  map_dfr(seq_len(n_imputations), function(i) {
    
    df_imp <- get(load(file.path(
      data_dir, glue::glue('mi_data/imp{i}.RData')
    )))
    
    bs <- bootstrap_gam(df_imp, n_iter)
    
    agg <- list(
      t15_overall   = summarise_mv(map(bs, 't15_overall')),
      t120_overall  = summarise_mv(map(bs, 't120_overall')),
      t15_subgroup  = summarise_mv(map(bs, 't15_subgroup')),
      t120_subgroup  = summarise_mv(map(bs, 't120_subgroup'))
    )
    
    tibble(
      imputation    = i,
      t15_overall   = list(agg$t15_overall),
      t120_overall  = list(agg$t120_overall),
      t15_subgroup  = list(agg$t15_subgroup),
      t120_subgroup  = list(agg$t120_subgroup)
    )
  })
}


rubins_rule <- function(mean_value, var_value, n_imputations){
  estimate <- mean(mean_value)
  WM <- mean(var_value)
  sigma <- as.vector(NULL)
  for (i in 1:n_imputations){
    sigma[i] <- (mean_value[[i]] - estimate)^2
  }
  BM <- sum(sigma)/(n_imputations - 1)
  TM <- WM + (1+(1/n_imputations))*BM
  se <- sqrt(TM)
  z <- estimate / se
  
  return(
    tibble(estimate = estimate,
           ll_95 = estimate - qnorm(0.975)*se,
           ul_95 = estimate + qnorm(0.975)*se,
           se,
           p_value = sprintf('%.4f', 2 * (1 - pnorm(abs(z)))))
  )
}

pool_with_rubin <- function(impute_tbl,
                            element = c('t15_overall', 't120_overall', 't15_subgroup', 't120_subgroup'),
                            n_imputations) {
  
  element <- match.arg(element)
  
  impute_tbl %>%
    select(imputation, all_of(element)) %>%
    unnest_wider(all_of(element)) %>%
    pivot_longer(
      -imputation,
      names_to      = c('metric', 'stat'),
      names_pattern = '^(.*)_(mean|var)$'
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    group_by(metric) %>%
    summarise(rubins_rule(mean, var, n_imputations), .groups = 'drop')
}

set.seed(813)

n_imputations <- 15
n_iter <- 500

time_start <- Sys.time()

results_imp <- run_all_imputations(data_dir, n_imputations, n_iter)

pooled_t15_overall  <- pool_with_rubin(results_imp, 't15_overall', n_imputations)
pooled_t120_overall <- pool_with_rubin(results_imp, 't120_overall', n_imputations)
pooled_t15_subgroup  <- pool_with_rubin(results_imp, 't15_subgroup', n_imputations)
pooled_t120_subgroup  <- pool_with_rubin(results_imp, 't120_subgroup', n_imputations)

time_end <- Sys.time()
message('Elapsed time: ',
        round(difftime(time_end, time_start, units = 'mins'), 2),
        ' minutes')

pooled_t15_overall
pooled_t120_overall
pooled_t15_subgroup
pooled_t120_subgroup

write.csv(pooled_t15_overall,  file.path(output_dir, 'pooled_t15_overall.csv'),  row.names = FALSE)
write.csv(pooled_t120_overall, file.path(output_dir, 'pooled_t120_overall.csv'), row.names = FALSE)
write.csv(pooled_t15_subgroup, file.path(output_dir, 'pooled_t15_subgroup.csv'), row.names = FALSE)
write.csv(pooled_t120_subgroup, file.path(output_dir, 'pooled_t120_subgroup.csv'), row.names = FALSE)