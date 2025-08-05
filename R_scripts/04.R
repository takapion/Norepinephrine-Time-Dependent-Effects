if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)

data_dir <- './data/'
output_dir <- './output_0519/'
ver <- '250519'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))


# Reshape invasive MBP data from offset -30 to 0 into wide format for modeling
mbp_pre_wide <- complete_df %>%
  filter(offset >= -30 & offset <= 0) %>%
  select(icu_stay_id, offset, invasive_mbp_lpf) %>%
  pivot_wider(names_from = offset, values_from = invasive_mbp_lpf,
              names_prefix = "invasive_mbp_lpf_offset_") %>% 
  rename_with(~ gsub("-", "minus", .x))

# Prepare data for training using offset 1–120 and join with pre-offset wide-format MBP data
df_post1 <- complete_df %>%
  filter(offset >= 1 & offset <= 120) %>%
  select(icu_stay_id, offset, invasive_mbp_lpf, female, age, first_sofa,
         on_vent, past_vent, ph, bicarbonate, base_excess, pco2, lactate,
         hr_pre, invasive_mbp_time0_value, gamma, sepsis) %>%
  mutate(invasive_mbp_lpf_diff = invasive_mbp_lpf - invasive_mbp_time0_value) %>%
  inner_join(mbp_pre_wide %>% select(-invasive_mbp_lpf_offset_0), by = "icu_stay_id")

df_pre0 <- df_base %>%
  select(-gamma) %>% 
  inner_join(mbp_pre_wide %>% select(
    icu_stay_id, invasive_mbp_lpf_offset_minus30, invasive_mbp_lpf_offset_minus25,
    invasive_mbp_lpf_offset_minus20, invasive_mbp_lpf_offset_minus15,
    invasive_mbp_lpf_offset_minus10, invasive_mbp_lpf_offset_minus5
  ),
  by = 'icu_stay_id')

# Extract non-septic/septic cases from the dataset for model training
df_post_shock <- df_post1 %>%
  filter(sepsis == 0)

# Fit a GAM model including past 5 MBP points and time 0 value
gam_model <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
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
                 data = df_post_shock,
                 family = gaussian(link = "identity"),
                 method = "REML")

# Create a representative median profile for non-septic/septic cases to use in prediction
df_pre0_shock <- df_pre0 %>%
  filter(sepsis == 0 ) %>%
  select(-sepsis) 

predict_mbp <- df_pre0_shock %>%
  select(-icu_stay_id) %>%
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
  slice(rep(1, 120)) %>%
  mutate(offset = seq(1, 120))

# Calculate and reshape the median invasive MBP for each offset (-30 to 0) across all patients
mbp_median_pre <- mbp_pre_wide %>%
  filter(icu_stay_id %in% df_post_shock$icu_stay_id) %>%
  summarise(across(starts_with("invasive_mbp_lpf_offset_"),
                   ~ median(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "offset", values_to = "invasive_mbp_raw") %>%
  mutate(offset = gsub("invasive_mbp_lpf_offset_minus", "-", offset),
         offset = gsub("invasive_mbp_lpf_offset_", "", offset),
         offset = as.numeric(offset)) %>%
  arrange(offset)

mbp_median_time0 <- mbp_median_pre %>%
  filter(offset == 0) %>%
  pull(invasive_mbp_raw)

mbp_diff_median_pre <- mbp_median_pre %>%
  mutate(invasive_mbp_diff = invasive_mbp_raw - mbp_median_time0) %>%
  select(offset, invasive_mbp_diff)

# Predict MAP changes for each norepinephrine dose
predict_list <- list()

for (gamma_val in c(0.025, 0.05, 0.1)) {
  predict_input <- predict_mbp %>% mutate(gamma = gamma_val)
  
  pred <- predict(gam_model, newdata = predict_input, type = "link", se.fit = TRUE)
  
  predict_temp <- predict_input %>%
    mutate(
      invasive_mbp_diff = pred$fit,
      fit_mbp_low95 = invasive_mbp_diff - qnorm(0.975) * pred$se.fit,
      fit_mbp_up95 = invasive_mbp_diff + qnorm(0.975) * pred$se.fit,
      gamma_group = paste0(gamma_val, " μg/kg/min")
    )
  
  predict_list[[as.character(gamma_val)]] <- predict_temp
}

# Add offset 0 row (baseline) and combine with prediction results for plotting
offset0_row <- data.frame(
  offset = 0,
  invasive_mbp_diff = 0,
  fit_mbp_low95 = NA,
  fit_mbp_up95 = NA,
  gamma_group = paste0(c(0.025, 0.05, 0.1), " μg/kg/min")
)

predict_mbp_shock <- bind_rows(offset0_row,
                               bind_rows(predict_list) %>%
                                 select(offset, invasive_mbp_diff, fit_mbp_low95, fit_mbp_up95, gamma_group))

# Plot predicted MAP trajectories over time by norepinephrine dose group
mbp_spline_all <- ggplot() +
  geom_line(data = mbp_diff_median_pre,
            aes(x = offset, y = invasive_mbp_diff),
            color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_line(data = predict_mbp_shock,
            aes(x = offset, y = invasive_mbp_diff, color = gamma_group),
            linewidth = 0.6) +
  scale_colour_brewer(palette = "Blues") +
  ggtitle(paste0("Predicted MAP diff over Time - ", "other shock")) +
  labs(x = "Time since Noradrenaline Initiation (min)",
       y = "Predicted MAP diff",
       color = "noradrenaline dose") +
  scale_x_continuous(limits = c(-30, 120), breaks = seq(-30, 120, by = 30)) +
  ylim(-3, 18) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  )

print(mbp_spline_all)

ggsave(
  filename = paste0(output_dir, "fig_mbp_", "_by_gamma", ver, "_", "other shock", "_alldata_5point_model.png"),
  plot = mbp_spline_all, width = 5, height = 3, dpi = 600
)