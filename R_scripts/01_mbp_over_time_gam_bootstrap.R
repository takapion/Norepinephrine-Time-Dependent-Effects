if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)
if (!require('furrr')) install.packages('furrr')
library(furrr)
if (!require('future')) install.packages('future')
library(future)
if (!require('progressr')) install.packages('progressr')
library(progressr)

data_dir <- './data/'
output_dir <- './output_0519/'
ver <- '250519'
ver_diagnosis <- '250524'
df <- read.csv(paste0(data_dir, 'blood_pressure_filtered_', ver, '.csv'))
df_base_original <- read.csv(paste0(data_dir, 'base_', ver, '.csv'))
df_diagnosis_original<- read.csv(paste0(data_dir, ver_diagnosis, '_diagnosis_shock_category', '.csv'))

# Extract mean blood pressure at time zero and store it as a new variable
first_mbp <- df %>% 
  filter(offset == 0) %>% 
  mutate(
    invasive_mbp_time0_value = invasive_mbp_lpf
  ) %>% 
  select(icu_stay_id, invasive_mbp_time0_value)

# Prepare diagnosis data by selecting sepsis information
df_diagnosis <- df_diagnosis_original %>% select(icu_stay_id, sepsis)
df_diagnosis$sepsis[is.na(df_diagnosis$sepsis)] <- 0

df_base_before_naomit <- df_base_original %>% select(-sepsis) %>%
  inner_join(first_mbp, by = 'icu_stay_id') %>%
  inner_join(df_diagnosis, by = 'icu_stay_id') 

# Count NA values per column
na_counts <- sapply(df_base_before_naomit, function(x) sum(is.na(x)))
na_counts <- sort(na_counts[na_counts > 0], decreasing = TRUE)  # Only show columns with NA
na_counts

df_base <- df_base_before_naomit %>% na.omit

df_base %>%
  summarise(
    total_rows = n(),
    unique_icu_stay_ids = n_distinct(icu_stay_id),
    sepsis_1_count = sum(sepsis == 1)
  )

save(df_base, file = paste0(data_dir, 'df_base_', ver, '.RData'))

# Create and save a histogram showing the distribution of initial norepinephrine dose
ne_dose_hist <- ggplot(df_base, aes(x = gamma)) +
  geom_histogram(binwidth = 0.005, fill = 'skyblue', color = 'black') +
  scale_x_continuous(
    limits = c(0, 0.3),
    breaks = seq(0, 0.3, by = 0.025)
  ) +
  labs(
    title = 'Distribution of Initial Norepinephrine dose',
    x = 'Norepinephrine dose (μg/kg/min)',
    y = 'Count'
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.2),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 10)
  )

ne_dose_hist

ggsave(paste0(output_dir, 'ne_dose_hist', ver, '.png'), ne_dose_hist,
       width = 5.5, height = 4, dpi=600)

# Prepare the dataset for prediction
df_joined <- df %>%
  inner_join(df_base, by = 'icu_stay_id') 

complete_df <- df_joined %>% 
  select(icu_stay_id, offset, invasive_mbp_lpf, female, age, sepsis, first_sofa,
         on_vent, past_vent, ph, bicarbonate, base_excess, pco2, lactate,
         hr_pre, invasive_mbp_time0_value, gamma) %>% 
  mutate(
    invasive_mbp_lpf_diff = invasive_mbp_lpf - invasive_mbp_time0_value
  )
save(complete_df, file = paste0(data_dir, 'complete_df_', ver, '.RData'))


# Fit a GAM model for invasive MBP and generate predicted values across offset using median covariates
gam_mbp_main <- gam(invasive_mbp_lpf ~ s(offset) + female + age +
                      sepsis + first_sofa + on_vent + past_vent +
                      ph + bicarbonate + base_excess + pco2 + lactate +
                      hr_pre + s(gamma) + s(invasive_mbp_time0_value),
                    data = complete_df,
                    family = gaussian(link = 'identity'),
                    method = 'REML')

predict_mbp_all <- df_base %>% 
  summarise(across(
    c(female, age, first_sofa, sepsis, 
      on_vent, past_vent, ph, bicarbonate, base_excess, 
      pco2, lactate, hr_pre, gamma, invasive_mbp_time0_value),
    ~ median(.)
  )) %>% 
  slice(rep(1, 151))  

predict_mbp_all$offset <- seq(-30, length.out = 151)

preds_all <- predict(gam_mbp_main, newdata = predict_mbp_all, type = 'link')

# Set up parallel processing and define a bootstrap function to fit the GAM model and predict MAP using resampled data
plan(multisession, workers = 8)
handlers('progress') 

# Define a single bootstrap iteration for parallel execution
bootstrap_once <- function(df_base, df) {
  df_id_sample <- df_base %>%
    select(icu_stay_id) %>% 
    sample_n(nrow(df_base), replace = TRUE)
  
  df_base_sample <- df_id_sample %>% 
    left_join(df_base, by = 'icu_stay_id', relationship = 'many-to-many')
  
  df_sample <- df_id_sample %>% 
    left_join(df, by = 'icu_stay_id', relationship = 'many-to-many')
  
  cat('Number of unique patients:', df_sample$icu_stay_id %>% n_distinct, '\n')
  cat('Number of non-unique patients for median calculation:', df_base_sample %>% nrow, '\n')
  cat('Number of BP measurements:', df_sample %>% nrow, '\n')
  
  model <- gam(invasive_mbp_lpf ~ s(offset) + female + age +
                 sepsis + first_sofa + on_vent + past_vent +
                 ph + bicarbonate + base_excess + pco2 + lactate +
                 hr_pre + s(gamma) + s(invasive_mbp_time0_value),
               data = df_sample,
               family = gaussian(link = 'identity'),
               method = 'REML')
  
  # Compute median values for baseline covariates
  median_vals <- df_base_sample %>%
    summarise(across(
      c(female, age, first_sofa, sepsis, on_vent, past_vent,
        ph, bicarbonate, base_excess, pco2, lactate, hr_pre, gamma, invasive_mbp_time0_value),
      ~ median(.)
    ))
  
  predict_df <- median_vals[rep(1, 151), ]
  predict_df$offset <- seq(-30, length = 151)
  
  predict(model, newdata = predict_df, type = 'link')
}

# Run parallel bootstrap GAM predictions and summarize results (median and standard error) across iterations
bootstrap_gam <- function(df_base, df, n_iter) {
  pred_list <- future_map(1:n_iter, ~ bootstrap_once(df_base, df),
                          .progress = TRUE,
                          .options = furrr_options(seed = TRUE))
  pred_mat <- do.call(cbind, pred_list)
  
  offset_seq <- seq(-30, length = 151)
  tibble(
    offset = offset_seq,
    predicted_mbp_median = apply(pred_mat, 1, median),
    se = apply(pred_mat, 1, sd)
  )
}

# Execute bootstrap and measure runtime
start_time <- Sys.time()

set.seed(916)
results_boot <- bootstrap_gam(df_base, complete_df, n_iter = 500) 

end_time <- Sys.time()
print(end_time - start_time)

# Combine bootstrap results with predicted MAP and plot the trajectory with 95% confidence intervals
results_plot <- results_boot %>%
  mutate(predicted_mbp_all = preds_all) %>% 
  mutate(
    ci_low = predicted_mbp_all - se * qnorm(0.975),
    ci_high = predicted_mbp_all + se * qnorm(0.975)
  )



# Plot
mbp_spline_all <- ggplot(results_plot, aes(x = offset)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = '95% CI'), alpha = 0.4) +
  geom_line(aes(y = predicted_mbp_all), color = 'black', linetype = 'solid', linewidth = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 65, linetype = 'dotted', color = 'black', linewidth = 0.6) +
  ggtitle('Predicted MAP over Time') +
  labs(x = 'Time since Noradrenaline Initiation (min)',
       y = 'Predicted MAP',
       fill = NULL) +
  scale_fill_manual(values = c('95% CI' = 'skyblue')) +
  scale_x_continuous(limits = c(-30, 120), breaks = seq(-30, 120, by = 30)) +
  ylim(55, 75) +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.05),  
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = 'white', color = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 10)
  )

print(mbp_spline_all)

ggsave(paste0(output_dir, 'fig_mbp_alldata_', ver, '.png'), mbp_spline_all,
       width = 5.5, height = 4, dpi=600)