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
  select(icu_stay_id, offset, invasive_mbp_lpf, female, age, sepsis, first_sofa,
         on_vent, past_vent, ph, bicarbonate, base_excess, pco2, lactate,
         hr_pre, invasive_mbp_time0_value, gamma) %>%
  mutate(invasive_mbp_lpf_diff = invasive_mbp_lpf - invasive_mbp_time0_value) %>%
  inner_join(mbp_pre_wide %>% select(-invasive_mbp_lpf_offset_0), by = "icu_stay_id")

df_post1$icu_stay_id %>% unique %>% length

df_pre0 <- df_base %>%
  select(-gamma) %>% 
  inner_join(mbp_pre_wide %>% select(
    icu_stay_id, invasive_mbp_lpf_offset_minus30, invasive_mbp_lpf_offset_minus25,
    invasive_mbp_lpf_offset_minus20, invasive_mbp_lpf_offset_minus15,
    invasive_mbp_lpf_offset_minus10, invasive_mbp_lpf_offset_minus5
  ),
  by = 'icu_stay_id')

# Calculate and reshape the median invasive MBP for each offset (-30 to 0) across all patients
mbp_median_pre <- mbp_pre_wide %>%
  summarise(across(starts_with("invasive_mbp_lpf_offset_"),
                   ~ median(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "offset", values_to = "invasive_mbp_raw") %>%
  mutate(offset = gsub("invasive_mbp_lpf_offset_minus", "-", offset),
         offset = gsub("invasive_mbp_lpf_offset_", "", offset),
         offset = as.numeric(offset)) %>%
  arrange(offset)

# Calculate the median at time 0 and compute the difference from it for each pre-offset MBP value
predict_mbp <- df_pre0 %>%
  select(-icu_stay_id) %>% 
  summarise(across(everything(),
                   ~ median(., na.rm = TRUE))) %>%
  slice(rep(1, 120)) %>%
  mutate(offset = seq(1, 120))

mbp_median_time0 <- mbp_median_pre %>%
  filter(offset == 0) %>%
  pull(invasive_mbp_raw)

mbp_diff_median_pre <- mbp_median_pre %>%
  mutate(invasive_mbp_diff = invasive_mbp_raw - mbp_median_time0) %>%
  select(offset, invasive_mbp_diff)

# Run MAP predictions for each specified norepinephrine dose value
gamma_values <- c(0.025, 0.05, 0.1)
predict_mbp_list <- list()

# Fit a GAM model including past 5 MBP points and time 0 value
gam_mbp_post1 <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
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
                     data = df_post1,
                     family = gaussian(link = "identity"),
                     method = "REML")

# Predict MAP changes for each norepinephrine dose
for (gamma_val in gamma_values) {
  predict_mbp_temp <- predict_mbp %>% mutate(gamma = gamma_val)
  
  preds_mbp_temp <- predict(gam_mbp_post1,
                            newdata = predict_mbp_temp,
                            type = "link", se.fit = TRUE)
  
  predict_mbp_temp <- predict_mbp_temp %>%
    mutate(
      invasive_mbp_diff = preds_mbp_temp$fit,
      fit_mbp_low95 = invasive_mbp_diff - qnorm(0.975) * preds_mbp_temp$se.fit,
      fit_mbp_up95 = invasive_mbp_diff + qnorm(0.975) * preds_mbp_temp$se.fit,
      gamma_group = paste0(gamma_val, " μg/kg/min")
    )
  
  predict_mbp_list[[as.character(gamma_val)]] <- predict_mbp_temp
}

# Combine offset -30 to 0 data for each gamma value
predict_mbp_wide_combined <- bind_rows(
  mbp_median_pre %>% mutate(gamma_group = "gamma = 0.025"),
  mbp_median_pre %>% mutate(gamma_group = "gamma = 0.05"),
  mbp_median_pre %>% mutate(gamma_group = "gamma = 0.1")
)

# Combine all data from offset -30 to 120
# Add offset 0 row of invasive_mbp_diff for each gamma_group
offset0_rows <- data.frame(
  offset = 0,
  invasive_mbp_diff = 0,
  fit_mbp_low95 = NA,
  fit_mbp_up95 = NA,
  gamma_group = paste0(gamma_val, " μg/kg/min")
)

# Add offset 0 row to predicted data (offset 0–120) for each gamma value
predict_mbp_by_gamma <- bind_rows(offset0_rows,
                                  bind_rows(predict_mbp_list) %>%
                                    select(offset, invasive_mbp_diff, fit_mbp_low95, fit_mbp_up95, gamma_group))

# Plot
mbp_spline_all <- ggplot() +
  geom_line(data = mbp_diff_median_pre,
            aes(x = offset, y = invasive_mbp_diff),
            color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_line(data = predict_mbp_by_gamma,
            aes(x = offset, y = invasive_mbp_diff, color = gamma_group),
            linewidth = 0.6) +
  scale_colour_brewer(palette = "Blues") +
  ggtitle("Predicted MAP over Time") +
  labs(x = "Time since Noradrenaline Initiation (min)",
       y = "Predicted MAP",
       color = "noradrenaline dose") +
  scale_x_continuous(limits = c(-30, 120), breaks = seq(-30, 120, by = 30)) +
  ylim(-2, 20) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )


print(mbp_spline_all)

ggsave(
  filename = paste0(output_dir, "fig_mbp_", "gamma", ver, "_alldata_5point_model.png"),
  plot = mbp_spline_all, width = 5, height = 3, dpi = 600
)