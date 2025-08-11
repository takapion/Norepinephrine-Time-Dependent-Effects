if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('mgcv')) install.packages('mgcv')
library(mgcv)
if (!require('RColorBrewer')) install.packages('RColorBrewer')
library(RColorBrewer)

data_dir <- './data/'
output_dir <- './output_0519/'
model_dir <- './model/'
ver <- '250519'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))
load(paste0(model_dir, 'gam_time_to_reach_65', '.RData'))

df_model_validity <- complete_df %>% 
  filter(icu_stay_id %in% (complete_df %>% filter(offset >= 60) %>% pull(icu_stay_id))) %>% 
  filter(offset >= 1) %>% 
  select(icu_stay_id, offset, invasive_mbp_lpf, female, age, sepsis, first_sofa,
         on_vent, past_vent, ph, bicarbonate, base_excess, pco2, lactate,
         hr_pre, invasive_mbp_time0_value, gamma)

df_model_validity <- df_model_validity %>% 
  mutate(
    predicted_mbp = predict(gam_time_to_reach_65, df_model_validity, type = 'response') + invasive_mbp_time0_value
  )

df_reached_65 <- df_model_validity %>%
  group_by(icu_stay_id) %>%
  summarise(
    max_mbp = max(invasive_mbp_lpf),
    max_predicted_mbp = max(predicted_mbp),
    truely_reached_65 = if_else(max_mbp >= 65, 1, 0),
    predicted_reach_65 = if_else(max_predicted_mbp >= 65, 1, 0),
    .groups = 'drop'
  )

conf_matrix <- df_reached_65 %>%
  count(truely_reached_65, predicted_reach_65) %>%
  mutate(
    freq = n / sum(n),
    truely_reached_65 = factor(truely_reached_65, levels = c(1, 0), labels = c('Yes', 'No')),
    predicted_reach_65 = factor(predicted_reach_65, levels = c(1, 0), labels = c('Yes', 'No'))
  )

TP <- conf_matrix %>% 
  filter(truely_reached_65 == 'Yes', predicted_reach_65 == 'Yes') %>%
  pull(n)

FN <- conf_matrix %>% 
  filter(truely_reached_65 == 'Yes', predicted_reach_65 == 'No') %>%
  pull(n)

TN <- conf_matrix %>% 
  filter(truely_reached_65 == 'No', predicted_reach_65 == 'No') %>%
  pull(n)

FP <- conf_matrix %>% 
  filter(truely_reached_65 == 'No', predicted_reach_65 == 'Yes') %>%
  pull(n)

cat(sprintf('Sensitivity: %.3f\n', TP / (TP + FN)))
cat(sprintf('Specificity: %.3f\n', TN / (TN + FP)))
cat(sprintf('Correctly predicted: %.f\n', TP+TN))
cat(sprintf('Accuracy: %.3f\n', (TP + TN) / (TP + TN + FP + FN)))

color_values <- c(colorRampPalette(brewer.pal(9, 'Blues'))(4))

confusion_matrix <- ggplot(conf_matrix, aes(x = truely_reached_65, y = predicted_reach_65, fill = freq)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = n), color = 'black', size = 6) +
  scale_fill_gradient(low = color_values[2], high = color_values[3]) +
  labs(
    x = 'Truly Reached MAP of 65mmHg',
    y = 'Predicted to Reach MAP of 65mmHg',
    fill = 'Proportion',
    title = 'Confusion Matrix Heatmap'
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'right',
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.6),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
  )
confusion_matrix

ggsave(paste0(output_dir, 'confusion_matrix_', ver, '.png'),
       confusion_matrix, width = 6, height = 5, dpi = 600, bg = 'white')