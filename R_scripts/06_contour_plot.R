if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)
if (!require('RColorBrewer')) install.packages('RColorBrewer')
library(RColorBrewer)
if (!require('ggnewscale')) install.packages('ggnewscale')
library(ggnewscale)


data_dir <- './data/'
output_dir <- './output_0519/'
model_dir <- './model/'
ver <- '250519'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))
load(paste0(data_dir, 'predict_mbp_over_time', ver, '.RData'))
load(paste0(model_dir, 'gam_time_to_reach_65', '.RData'))

gamma_values <- seq(0.001, 0.1, by = 0.0001)

first_offset_list <- vector("list", length(gamma_values))

for (i in seq_along(gamma_values)) {
  g <- gamma_values[i]
  
  temp_df <- predict_mbp_over_time %>% 
    mutate(gamma = g)
  
  temp_df$predicted_mbp <- predict(
    gam_time_to_reach_65,
    newdata = temp_df,
    type   = "response"
  ) + temp_df$invasive_mbp_time0_value
  
  summary_df <- temp_df %>% 
    group_by(invasive_mbp_time0_value, gamma) %>% 
    summarise(
      time_to_reach_65 = if (any(predicted_mbp >= 65)) {
        min(offset[predicted_mbp >= 65])
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  ## d) store & clean up
  first_offset_list[[i]] <- summary_df
  rm(temp_df, summary_df)
  if (i %% 20 == 0) gc(verbose = FALSE)
}

## ── 3. Bind the small summaries (≪ memory) ──────────────────────────────────
first_offset <- bind_rows(first_offset_list)
rm(first_offset_list); gc()

labels_pred <- c(
  '0 to 10', '10 to 20', '20 to 30',
  '30 to 40', '40 to 50', '50 to 60',
  'Do not reach 65\nwithin 60 min'
)

labels_unadjust <- c(
  '0 to 10', '10 to 20', '20 to 30',
  '30 to 40', '40 to 50', '50 to 60',
  'Did not reach 65\nwithin 60 min'
)

# Define color palette (6 levels + black)
color_values <- c(
  colorRampPalette(brewer.pal(9, 'BuGn'))(6),
  'black'
)

# Create factor with explicit NA label
df_plot <- first_offset %>%
  mutate(time_group = cut(
    time_to_reach_65,
    breaks = c(0, 10, 20, 30, 40, 50, Inf),
    labels = labels_pred[1:6],
    right = FALSE
  )) %>%
  mutate(
    time_group = fct_na_value_to_level(time_group, labels_pred[7])
  )

save(df_plot, file = paste0(data_dir, 'df_plot_', ver, '.RData'))

# Plot
discrete_contour_plot <- ggplot(df_plot, aes(x = invasive_mbp_time0_value, y = gamma, fill = time_group)) +
  geom_raster() +
  scale_fill_manual(
    values = setNames(color_values, labels_pred),
    name = 'Predicted time (min)'
  ) +
  scale_x_continuous(limits = c(45, 65), name = 'Initial MAP (mmHg)') +
  scale_y_continuous(limits = c(0, 0.1), name = 'Norepinephrine Dose (μg/kg/min)') +
  labs(title = 'Predicted Time to Reach MAP of 65 mmHg') +
  theme_classic() +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )
discrete_contour_plot

ggsave(paste0(output_dir, 'discrete_contour_plot_', ver, '.png'),
       discrete_contour_plot, width = 7, height = 5, dpi = 600)

df_crude_offset <- complete_df %>%
  filter(icu_stay_id %in% (complete_df %>% filter(offset >= 60) %>% pull(icu_stay_id))) %>%
  filter(offset >= 0 & offset <= 60 & invasive_mbp_time0_value <=65 & invasive_mbp_time0_value >= 45 & gamma <= 0.1) %>% 
  group_by(icu_stay_id) %>%
  summarise(
    time_to_reach_65 = if (any(invasive_mbp_lpf >= 65)) min(offset[invasive_mbp_lpf >= 65]) else NA_real_,
    .groups = 'drop'
  ) %>% inner_join(df_base %>% select(icu_stay_id, invasive_mbp_time0_value, gamma), by = 'icu_stay_id') %>%
  mutate(time_group = cut(
    time_to_reach_65,
    breaks = c(0, 10, 20, 30, 40, 50, Inf),
    labels = labels_unadjust[1:6],
    right = FALSE
  )) %>%
  mutate(time_group = forcats::fct_na_value_to_level(time_group, labels_unadjust[7]))

save(df_crude_offset, file = paste0(data_dir, 'df_crude_offset_', ver, '.RData'))

unadjusted_data_scatter_plot <- ggplot() +
  geom_point(
    data = df_crude_offset,
    aes(x = invasive_mbp_time0_value, y = gamma, fill = time_group),
    shape = 21,
    size = 2.5,
    stroke = 0.4
  ) +
  scale_fill_manual(
    values = setNames(color_values, labels_unadjust),
    name = 'Observed time (min)'
  ) +
  scale_x_continuous(limits = c(45, 65), name = 'Initial MAP (mmHg)') +
  scale_y_continuous(limits = c(0, 0.1), name = 'Norepinephrine Dose (μg/kg/min)') +
  labs(title = 'Observed Time to Reach MAP of 65 mmHg') +
  theme_classic() +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 1.3),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )
unadjusted_data_scatter_plot

ggsave(paste0(output_dir, 'unadjusted_data_scatter_plot_', ver, '.png'),
       unadjusted_data_scatter_plot, width = 7, height = 5, dpi = 600)
