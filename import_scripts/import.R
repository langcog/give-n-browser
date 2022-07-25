# schema:
# 1. datasets: dataset_id, lab, method, cite  
# 2. subjects: subject_id, dataset_id, age, sex, language, kl (computed)
# 3. trials: trial_id, subject_id, quantity, response
rm(list = ls())
library(here)
library(magrittr)
library(tidyverse)

source(here("import_scripts/helper.R"))


## NB: in some cases, KL for participants will have to be added later. This will be done via Wynn KL (included at the end of this script)

## read in processed data 
kl_only_data <- read_csv(here("data/processed-data/kl_data_processed.csv")) #data is cleaned and processed in another script
trial_level_data <- read_csv(here('data/processed-data/trial_level_processed_data.csv'))

## ------------------- reformat and factor data to schema

# # delete processed data in main data
file.remove(here::here("data/processed-data/datasets.csv"))
file.remove(here::here("data/processed-data/subjects.csv"))
file.remove(here::here("data/processed-data/trials.csv"))

# # delete processed data in shiny data 
file.remove(here::here("shiny_app/data/processed-data/datasets.csv"))
file.remove(here::here("shiny_app/data/processed-data/subjects.csv"))
file.remove(here::here("shiny_app/data/processed-data/trials.csv"))

## combine trial level and kl only data
all_data <- combine_data(kl_only_data, trial_level_data)

## Add highest count ====
all_data <- add_hc(all_data)

## Add country information ====
all_data <- add_country(all_data)

# Write data -----
##datasets
all_data %>%
  select(Experiment, lab, method, cite) %>%
  distinct() %>%
  rename(dataset_id = Experiment) %>%
  write_to_datasets()

##subjects ## CHANGED
all_data %>% 
  select(Experiment, Subject, Language, Country, Age_months, KL, Sex, highest_count) %>%
  distinct() %>%
  rename(dataset_id = Experiment, 
         subject_id = Subject, 
         language = Language, 
         age_months = Age_months) %>%
  write_to_subjects()

## trials -- this isn't working rn?
trial_level_data <- create_zero_index(trial_level_data)

trial_level_data %>%
  select(Experiment, trial_id, Subject, Query, Response)%>%
  rename(dataset_id = Experiment, 
                subject_id = Subject)%>%
  write_to_trials()


## Run KL models on everyone ====

kl.dir = "import_scripts/KLmodels/"
kl.sources = paste0(here(kl.dir),list.files(here(kl.dir)))
sapply(kl.sources, source)

