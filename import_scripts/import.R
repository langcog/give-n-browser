# schema:
# 1. datasets: dataset_id, lab, cite, method
# 2. subjects: subject_id, dataset_id, age, sex, language, kl (computed)
# 3. trials: trial_id, subject_id, quantity, response, counted

library(magrittr)
library(tidyverse)

## read raw data 
almoammer_raw <- read_csv(here::here("raw_data/Almoammer2013_english.csv"))
piantadosi_raw <- read_csv(here::here("raw_data/Piantadosi2014.csv"))                      

## ------------------- reformat and factor data to schema

# delete processed data
file.remove(here::here("processed_data/datasets.csv"))
file.remove(here::here("processed_data/subjects.csv"))
file.remove(here::here("processed_data/trials.csv"))


###### almoammer
almoammer <- almoammer_raw %>%
  gather(quantity, response, `1`:`3_2`) %>%
  mutate(quantity = as.numeric(unlist(map(almoammer$quantity,
                                          function(x) {str_split(x, pattern = "_")[[1]][1]}))))

almoammer %>% 
  select(Experiment, Participant, Language, PrimaryLang, Age_months, SEX) %>%
  distinct() %>%
  rename(dataset_id = Experiment, 
         subject_id = Participant, 
         language = Language, 
         age = Age_months, 
         sex = SEX) %>%
  write_to_subjects()

almoammer %>%
  select(Participant, quantity, response) %>%
  rename(subject_id = Participant) %>%
  write_to_trials()

almoammer %>%
  select(Experiment) %>%
  rename(dataset_id = Experiment) %>%
  mutate(lab = "Barner") %>%
  write_to_datasets()

###### piantadosi




## ------------------- run KL models on everyone and add data to subjects