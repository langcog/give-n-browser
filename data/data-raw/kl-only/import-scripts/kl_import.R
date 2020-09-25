## Code for processing and cleaning knower-level only data with general template: 
#Experiment, Language, SID, KL, Age_months, method, cite, Age_years 

#load libraries
rm(list = ls())
library(tidyverse)
library(tidylog)
library(magrittr)

# Read in all data from kl-only raw data ----
##TO-DO: add column for sex
data.raw <-
  list.files(path = "data/data-raw/kl-only/data-raw/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Wrangling and cleaning ----

#mostly renaming
data.raw %<>%
  mutate(Experiment = ifelse(Language == "Mandarin", "Almoammer2013", Experiment), 
         Subject = ifelse((is.na(Subject) & !is.na(Subnum)), Subnum, 
                          ifelse((is.na(Subject) & !is.na(Participant)), Participant, Subject)), 
         Age_months = ifelse((is.na(Age) & !is.na(Age.Mos)), Age.Mos, Age), 
         KL = ifelse((is.na(KL) & !is.na(Knower.Level)), Knower.Level, KL), 
         KL = ifelse(is.na(KL), Knower_level, KL), 
         KL = ifelse(is.na(KL), `Knower-level`, KL),
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual")), 
         Sex = NA)%>%
  filter(KL != "ChangeMe", 
         KL != "X")%>%
  mutate(KL = ifelse((KL == "5" & (Experiment == "Marusic2016" | Experiment == "Almoammer2013")), "CP", 
                           as.character(KL)), 
         KL = ifelse(KL == "5K", "5", as.character(KL)),
         KL = ifelse(KL == "4K", "4", as.character(KL)), 
         KL = ifelse(KL == "Non", "0", as.character(KL)))

#select relevant columns, round for age and mutate years
data.raw %<>%
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, lab, Language, Subject, KL, Age_months, Age_years, method, cite)

# Save and export ----
write_csv(data.raw, here::here("data/processed-data/kl_data_processed.csv"))
