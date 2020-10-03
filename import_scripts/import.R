# schema:
# 1. datasets: dataset_id, lab, method, cite  
# 2. subjects: subject_id, dataset_id, age, sex, language, kl (computed)
# 3. trials: trial_id, subject_id, quantity, response
rm(list = ls())
library(here)
library(magrittr)
library(tidyverse)

source(here("import_scripts/helper.R"))

## NB: in some cases, KL for participants will have to be added later

## read in processed data
kl_only_data <- read_csv(here("data/processed-data/kl_data_processed.csv")) #data is cleaned and processed in another script
trial_level_data <- read_csv(here('data/processed-data/trial_level_processed_data.csv'))

## ------------------- reformat and factor data to schema

# # delete processed data
file.remove(here::here("data/processed-data/datasets.csv"))
file.remove(here::here("data/processed-data/subjects.csv"))
file.remove(here::here("data/processed-data/trials.csv"))

## combine trial level and kl only data
all_data <- combine_data(kl_only_data, trial_level_data)

# Write data -----
##datasets
all_data %>%
  select(Experiment, lab, method, cite) %>%
  distinct() %>%
  rename(dataset_id = Experiment) %>%
  write_to_datasets()

##subjects
##To-DO: Get Sex information
all_data %>% 
  select(Experiment, Subject, Language, Age_months, KL) %>%
  distinct() %>%
  rename(dataset_id = Experiment, 
         subject_id = Subject, 
         language = Language, 
         age_months = Age_months) %>%
  write_to_subjects()

## trials
trial_level_data <- create_zero_index(trial_level_data)

trial_level_data %>%
  select(Experiment, trial_id, Subject, Query, Response)%>%
  rename(dataset_id = Experiment, 
                subject_id = Subject)%>%
  write_to_trials()

# --- run KL models on everyone
## ... Wynn KL assignment ----
## This will only be run with trial-level data 
##get 0s and 1s for responses
tmp <- trial_level_data%>%
  filter(Experiment == "Almoammer2013")%>%
  mutate(correct = ifelse(Query == Response, 1, 0))

#this will work for non-titrated
tmp1 <- tmp %>%
  group_by(Subject, Query)%>%
  summarise(num_trials = n(), 
            num_correct = sum(correct))

create_wynn_df <- function(df) {
  full_wynn_df <- data.frame() #placeholder df
  
  df <- df %>%
    mutate(correct = ifelse(Query == Response, 1, 0)) #add correct responses
  unique_datasets <- as.vector(unique(df$Experiment)) #get unique datasets
  for (d in unique_datasets){ #for every dataset
    d.subset <- df %>%
      filter(Experiment == d)
    unique_queries <- as.vector(unique(d.subset$Query)) #get unique queries for false gives
    
    d.subset %<>% #this gives us false gives within a particular datset
      mutate(false_give = ifelse((correct == 0 & Response %in% unique_queries), 
                                 1, 0), 
             resp_false_give = ifelse(false_give == 1, Response, NA))
    #this gives us correct responses
    correct_df <-  d.subset%>% #correct
      group_by(Subject, Query)%>%
      summarise(num_trials = n(), 
                num_correct = sum(correct))
    
    false_give_df <- d.subset %>% # false gives
      filter(!is.na(resp_false_give))%>%
      group_by(Subject, resp_false_give)%>%
      summarise(num_false_give = sum(false_give))%>%
      dplyr::rename("Query" = "resp_false_give")
    
    #left join these two together
    tmp_wynn_df <- left_join(correct_df, false_give_df, by = c("Subject", "Query"))%>%
      mutate(dataset = d)
    full_wynn_df <- bind_rows(full_wynn_df, tmp_wynn_df)
  }
  return(full_wynn_df)
}

x <- create_wynn_df(trial_level_data) #hooray this works!



