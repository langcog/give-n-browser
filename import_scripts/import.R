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
create_wynn_df <- function(df) {
  full_wynn_df <- data.frame() #placeholder df
  
  df <- df %>%
    mutate(correct = ifelse(Query == Response, 1, 0)) #add correct responses
  unique_datasets <- as.vector(unique(df$Experiment)) #get unique datasets
  for (d in unique_datasets){ #for every dataset
    d.subset <- df %>%
      filter(Experiment == d)
    unique_queries <- as.vector(unique(d.subset$Query)) #get unique queries for false gives
    
    d.subset <- d.subset %>% #this gives us false gives within a particular datset
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
  full_wynn_df <- full_wynn_df %>%
    mutate(num_failures = num_trials - num_correct, 
           num_false_give = ifelse(is.na(num_false_give), 0, as.numeric(num_false_give)))
  return(full_wynn_df)
}

final_wynn_df <- create_wynn_df(trial_level_data) #hooray this works!

##okay now we need to assign KLs
tmp_assignment <- final_wynn_df %>%
  mutate(possibly_known = ifelse(((num_correct/num_trials >= 2/3) & num_false_give == 0), 1, #if they are successful 2/3 of the time with no false gives
                                 ifelse(((num_correct/num_trials >= 2/3) & (num_correct/(num_correct+num_false_give)) >= 2/3), 1, 0)), #if they are successful 2/3 time and of times they gave n it was in response to n at least 2/3 of the time
         definitely_unknown = ifelse(((num_correct/num_trials) < 2/3), 1, 
                                     ifelse(((num_correct/(num_correct+num_false_give)) < 2/3), 1, 0)), 
         tracker = ifelse((possibly_known == 1 & definitely_unknown == 0), 1, 
                          ifelse((possibly_known == 0 & definitely_unknown == 1), 0, 
                                 ifelse((possibly_known == 1 & definitely_unknown == 1), "HELP", 
                                        ifelse((possibly_known == 0 & definitely_unknown == 0), "CHECK", "Other"))))) 

##TODO - write function that checks for issues

track_successes <- function(df) {
  tmp.df <- df %>%
    mutate(success_tracker = ifelse((tracker == 0 & Query == 1), -100, #if this is the first # and tracker is 0, NON
                                    ifelse(tracker == 1, Query, #otherwise, keep track of last successful number
                                           ifelse((tracker == 0 & lag(tracker) == 1), -50, 
                                                  ifelse((tracker == 0 & lag(tracker) == 0), -50, "help")))))
  
  ##TODO - add function that will throw error if there is a help in this df
  
  unique_subs <- as.vector(unique(tmp.df$Subject))
  tracker.df <- data.frame()
  for (s in unique_subs) {
    s.df <- tmp.df %>%
      filter(Subject == s)
    
    if (min(s.df$success_tracker) == -100) {
      tmp.s.df <- data.frame(Subject = s, 
                     Assignment = '0')
    } else if (min(s.df$success_tracker) > 0) { #if they haven't gotten anything wrong, and there are no NAs
      highest_num <- max(s.df$success_tracker)
      tmp.s.df <- data.frame(Subject = s, 
                             Assignment = as.character(highest_num))
    } else if (min(s.df$success_tracker) == -50) { #if they have gotten something incorrect 
      #then we should get the *minimum* number for which they failed
      min.fail <- s.df %>%
        filter(success_tracker == -50)
      
      min.fail.num <- min(min.fail$Query) #get the minimum number for which they failed
      
      #filter out anything in main df above this
      s.df <- s.df %>%
        filter(Query < min.fail.num)
      
      #now get the maximum number in their success tracker after this
      max.succeed.num <- max(s.df$success_tracker)
      
      tmp.s.df <- data.frame(Subject = s, 
                             Assignment = as.character(max.succeed.num))
    } else {
      tmp.s.df <- data.frame(Subject = s, 
                             Assignment = '-500')
    }
    tracker.df <- bind_rows(tracker.df, tmp.s.df)
  }
  return(tracker.df)
  }

checking_assignments <- track_successes(tmp_assignment) %>%
  mutate(Assignment = ifelse(as.numeric(as.character(Assignment)) >= 6, "CP", as.character(Assignment)))

##TODO - special cases for CP??

#bind with original for checking
tmp_check <- left_join(all_data, checking_assignments, by = "Subject")%>%
  mutate(KL_check = ifelse(KL != Assignment, "FLAG", "Fine"))

flags <- tmp_check %>%
  filter(KL_check == "FLAG")%>%
  distinct(Subject, Experiment, KL,Assignment)


