if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)

data_dir <- './data/'
output_dir <- './output_0519/'
model_dir <- './model/'
ver <- '250519'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))


# Prepare data (offset 1–120) for model training 
df_post2 <- complete_df %>%
  select(icu_stay_id, offset, invasive_mbp_lpf, female, age, sepsis, first_sofa,
         on_vent, past_vent, ph, bicarbonate, base_excess, pco2, lactate,
         hr_pre, invasive_mbp_time0_value, gamma) %>%
  mutate(invasive_mbp_lpf_diff = invasive_mbp_lpf - invasive_mbp_time0_value) 

df_post2$icu_stay_id %>% unique %>% length

# Fit GAM model
gam_time_to_reach_65 <- gam(invasive_mbp_lpf_diff ~ s(offset) + female + age +
                              sepsis + first_sofa + on_vent + past_vent +
                              ph + bicarbonate + base_excess + pco2 + lactate +
                              hr_pre + s(gamma) + s(invasive_mbp_time0_value) +
                              ti(gamma, offset) + ti(invasive_mbp_time0_value, offset),
                            data = df_post2,
                            family = gaussian(link = "identity"),
                            method = "REML")
save(gam_time_to_reach_65, file = paste0(model_dir, 'gam_time_to_reach_65', '.RData'))

# Create representative median case data
df_pre0 <- df_base %>% 
  select(-icu_stay_id, -gamma, -invasive_mbp_time0_value)

df_base_median <- df_pre0 %>%
  summarise(across(
    everything(),
    ~ median(.)
  ))

# Create prediction dataset (offset 0–120, time0 = 45–65 mmHg)
mbp_time0_values <- seq(45, 65, by = 0.01)
predict_mbp_over_time <- expand_grid(
  offset = seq(0, 60, by = 0.1),
  invasive_mbp_time0_value = mbp_time0_values
) %>%
  crossing(df_base_median) %>%
  select(offset, invasive_mbp_time0_value, everything())
save(predict_mbp_over_time, file = paste0(data_dir, 'predict_mbp_over_time', ver, '.RData'))

# Predict for each norepinephrine dose and extract minimum offset where MAP exceeds 65 mmHg (handle NA cases)
gamma_values <- c(0.025, 0.05, 0.1)
exceed_offset_dfs <- list()

for (gamma_val in gamma_values) {
  predict_mbp_temp <- predict_mbp_over_time %>% mutate(gamma = gamma_val)
  
  preds_mbp_temp <- predict(gam_time_to_reach_65,
                            newdata = predict_mbp_temp,
                            type = "link", se.fit = TRUE)
  
  predict_mbp_temp <- predict_mbp_temp %>%
    mutate(
      invasive_mbp_diff = preds_mbp_temp$fit,
      invasive_mbp_predicted = invasive_mbp_diff + invasive_mbp_time0_value
    )
  
  # For each initial MAP value, find the minimum offset at which predicted MAP ≥ 65 mmHg; return NA if none
  exceed_offset_df <- mbp_time0_values %>%
    map_dfr(function(val) {
      sub_df <- predict_mbp_temp %>% filter(invasive_mbp_time0_value == val)
      first_offset <- sub_df %>%
        filter(invasive_mbp_predicted >= 65) %>%
        summarise(
          offset_exceed_65 = if (n() == 0) NA_real_ else min(offset),
          .groups = "drop"
        ) %>% 
        pull(offset_exceed_65)
      
      tibble(
        invasive_mbp_time0_value = val,
        gamma_value = gamma_val,
        gamma_group = paste0("gamma = ", gamma_val),
        offset_exceed_65 = first_offset
      )
    })
  
  exceed_offset_dfs[[as.character(gamma_val)]] <- exceed_offset_df
}

# Combine results for all norepinephrine dose values
exceed_offset_all_gamma <- bind_rows(exceed_offset_dfs)

# plot
time_to_reach_map65 <- ggplot(exceed_offset_all_gamma, aes(x = invasive_mbp_time0_value, y = offset_exceed_65, color = as.factor(gamma_value))) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_colour_brewer(palette="Blues") +
  labs(
    title = "Predicted Time to Achieve MAP of 65 mmHg",
    x = "Initial MAP (mmHg)",
    y = "Time from Norepinephrine Initiation (min)",
    color = "Norepinephrine\ndose (μg/kg/min)"
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(0, 10, 20, 30, 40, 50, 60)
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 2.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

print(time_to_reach_map65)

ggsave(paste0(output_dir, "time_to_reach_map65_", ver, ".png"),
       time_to_reach_map65, width = 6, height = 5, dpi = 600)