# schema:
# 1. datasets: dataset_id, lab, method, cite  
# 2. subjects: subject_id, dataset_id, age, sex, language, kl (computed)
# 3. trials: trial_id, subject_id, quantity, response
source('import_scripts/helper.R')
library(magrittr)
library(tidyverse)


## TO-DO: set input and output paths more clearly

## NB: in some cases, KL for participants will have to be added later

## read in processed data
kl_only_data <- read.csv(here::here("data/processed-data/kl_data.csv")) #data is cleaned and processed in another script
trial_level_data <- read.csv(here::here('data/processed-data/trial_level_processed_data.csv'))

## ------------------- reformat and factor data to schema

# # delete processed data
file.remove(here::here("data/processed-data/datasets.csv"))
file.remove(here::here("data/processed-data/subjects.csv"))
file.remove(here::here("data/processed-data/trials.csv"))

# KL-only data ----
##datasets
kl_only_data %>%
  dplyr::select(Experiment, lab, method, cite) %>%
  droplevels()%>%
  dplyr::rename(dataset_id = Experiment) %>%
  write_to_datasets()

##subjects
##To-DO: Get Sex information
kl_only_data %>% 
  select(Experiment, Subject, Language, Age_months, KL) %>%
  distinct() %>%
  rename(dataset_id = Experiment, 
         subject_id = Subject, 
         language = Language, 
         age = Age_months) %>%
  write_to_subjects()

## Trial-level data ----
## function for trial_id
create_zero_index<- function(data, id_column_name="Subject") {
  data <- data %>%
    mutate(query_lag = lag(Query), 
           temp = ifelse(Query != query_lag, 1, 0), 
           temp_id = cumsum(c(0, temp[!is.na(temp)])), 
           trial_id = temp_id)
}

trial_level_data <- create_zero_index(trial_level_data)

trial_level_data %>%
  dplyr::select(Experiment, trial_id, Subject, Query, Response)%>%
  dplyr::rename(dataset_id = Experiment, 
                subject_id = Subject)%>%
  write_to_trials()

## ------------------- run KL models on everyone and add data to subjects

#