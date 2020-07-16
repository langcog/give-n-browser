## Code for processing and cleaning knower-level only data with general template: 
#Experiment, Language, SID, KL, Age_months, method, cite, Age_years 

#load libraries
# rm(list = ls())
library(tidyverse)
library(tidylog)
library(here)

# Read in all data from kl-only raw data ----
##TO-DO: add column for sex
data.raw <-
  list.files(path = "../data-raw/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Wrangling and cleaning ----
### TO-DO: when copying data, rename columns to fit general template

#mostly renaming
data.raw %<>%
  mutate(Experiment = ifelse(Language == "Mandarin", "Almoammer2013", Experiment), 
         Subject = ifelse((is.na(Subject) & !is.na(Subnum)), Subnum, 
                          ifelse((is.na(Subject) & !is.na(Participant)), Participant, Subject)), 
         Age_months = ifelse((is.na(Age) & !is.na(Age.Mos)), Age.Mos, Age), 
         KL = ifelse((is.na(KL) & !is.na(Knower.Level)), Knower.Level, KL), 
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual")), 
         Sex = NA)

#select relevant columns, round for age and mutate years
data.raw %<>%
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, Language, Subject, KL, Age_months, Age_years, method, cite)

##TO-DO: Add 'lab' column when importing data
data.raw %<>%
  mutate(lab = NA)

# Save and export ----
write_csv(data.raw, here("data/processed-data/kl_data.csv"))
