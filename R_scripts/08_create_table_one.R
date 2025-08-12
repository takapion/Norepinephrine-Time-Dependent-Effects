if (!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if (!require('tableone')) install.packages('tableone')
library(tableone)

data_dir <- './data/'
output_dir <- './output_0519/'
ver <- '250519'
ver_diagnosis <- '250524'

load(paste0(data_dir, 'df_base_', ver, '.RData'))
load(paste0(data_dir, 'complete_df_', ver, '.RData'))
df_diagnosis_original<- read.csv(paste0(data_dir, ver_diagnosis, '_diagnosis_shock_category', '.csv'))
df_diagnosis_original <- df_diagnosis_original %>%
  mutate(across(c(sepsis, trauma, cardiovascular_disease, others), ~replace_na(., 0)))


df_base <- df_base %>% select(-sepsis) %>% 
  inner_join(df_diagnosis_original, by = 'icu_stay_id')

myVars <- c('age', 'female', 'first_bodyweight', 'first_sofa',
            'sepsis', 'trauma', 'cardiovascular_disease', 'others',
            'gamma', 'hr_pre', 'ph', 'pco2', 'bicarbonate', 'base_excess', 'lactate', 'on_vent')

catVars <- c('female', 'on_vent', 'sepsis', 'trauma', 'cardiovascular_disease', 'others')

medVars <- c('age', 'first_bodyweight', 'first_sofa', 'gamma',
             'hr_pre', 'ph', 'pco2', 'bicarbonate', 'base_excess', 'lactate')

tab1 <- CreateTableOne(vars = myVars, 
                       data = df_base, 
                       factorVars = catVars) 

tab1Mat <- print(tab1,
                 nonnormal = medVars,
                 catDigits = 1,
                 contDigits = 1,
                 formatOptions = list(big.mark = ","),
                 noSpaces = TRUE,
                 #printToggle = FALSE,
                 test = FALSE,
                 smd = FALSE)

write.csv(tab1Mat, paste0(output_dir, 'Table1.csv'), row.names = TRUE)