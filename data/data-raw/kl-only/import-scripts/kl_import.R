## Code for processing and cleaning knower-level only data with general template: SID, Age, KL, Language

#load libraries
rm(list = ls())
library(tidyverse)
library(tidylog)

# Read in all data from kl-only raw data ----
data.raw <-
  list.files(path = "../data-raw/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Wrangling and cleaning ----
#mostly renaming
data.raw %<>%
  mutate(Experiment = ifelse(Language == "Mandarin", "Almoammer2013", Experiment), 
         Subject = ifelse((is.na(Subject) & !is.na(Subnum)), Subnum, 
                          ifelse((is.na(Subject) & !is.na(Participant)), Participant, Subject)), 
         Age = ifelse((is.na(Age) & !is.na(Age.Mos)), Age.Mos, Age), 
         KL = ifelse((is.na(KL) & !is.na(Knower.Level)), Knower.Level, KL), 
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual")))

#select relevant columns, round for age and mutate years
data.raw %<>%
  dplyr::select(Experiment, Language, Subject, KL, Age, method, cite)%>%
  mutate(Age = round(as.numeric(as.character(Age), 4)), 
         Age_years = floor(Age)/12)

# Save and export ----
write.csv(data.raw, '../../../processed-data/kl_data.csv', 
          row.names = FALSE)
