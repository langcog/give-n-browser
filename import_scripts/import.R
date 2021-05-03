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
## Wynn KL assignment: Child is classified as an N-knower if they are able to successfully give N at least 2/3 times when asked for N && they do not falsely give that N more than once for another request


## The function below is doing the following: 
## 1. For each dataset, get the unique numbers queried (this will vary by dataset)
## 2. For each dataset, for each participant, for each number *queried* in the datset, do the following:
#### a. Get total number of trials per query
#### b. Get total number of correct responses per query
#### c. Get total number of false gives per query (ie child gave that number incorrectly for another N)
#### d. Get total number of failures per query


## This will only be run with trial-level data 
create_wynn_df <- function(df) {
  #create a placeholder df so we can somewhere to send our results
  full_wynn_df <- data.frame() 
  
  ## Add 'correct' variable for easy calculation
  df <- df %>%
    mutate(correct = ifelse(Query == Response, 1, 0))
  
  ## get the unique datasets: we need to do this by dataset because the numbers tested in each dataset may vary:
  ## this is important for determining false gives
  unique_datasets <- as.vector(unique(df$Experiment)) 
  
  ### The following loops through datasets, gets unique numbers queried
  ### For each row (ie, each trial), determines based on response and correct: 
  ### Whether that trial is correct, a failure, or a false give
  for (d in unique_datasets){ #for every dataset
    ## filter down to dataset of interest
    d.subset <- df %>%
      filter(Experiment == d)
    ## Get the numbers tested in this dataset, store as a vector
    unique_queries <- as.vector(unique(d.subset$Query)) #get unique queries for false gives
    
    ## First, add a column indicating whether this trial is a false give, AND add the particular number that they gave
    d.subset <- d.subset %>% #this gives us false gives within a particular datset
      mutate(false_give = ifelse((correct == 0 & Response %in% unique_queries), 
                                 1, 0), 
             resp_false_give = ifelse(false_give == 1, Response, NA))
    
    ## In the below, we're summarising by subject and by number to get total correct and false gives
    
    ## First, for each subject and for each query we total up the number of trials and the number of times the child correctly gave on that trial
    correct_df <-  d.subset%>% #correct
      group_by(Subject, Query)%>%
      summarise(num_trials = n(), 
                num_correct = sum(correct))
    
    ## Next, we look at the actual numbers that children were giving incorrectly on false gives
    ## This will let us sum up, for a given query, how often children gave that number falsely for another number
    false_give_df <- d.subset %>% # false gives
      filter(!is.na(resp_false_give))%>%
      group_by(Subject, resp_false_give)%>%
      summarise(num_false_give = sum(false_give))%>%
      dplyr::rename("Query" = "resp_false_give")
    
    #Now, left join the correct and the false give summaries together: 
    ## This will give us: Subject, Query, num_trials (how many trials child was asked about n), num_correct (successes), num_false_gives, dataset
    tmp_wynn_df <- left_join(correct_df, false_give_df, by = c("Subject", "Query"))%>%
      mutate(dataset = d)
    
    ## Push these to our placeholder df
    full_wynn_df <- bind_rows(full_wynn_df, tmp_wynn_df)
  }
  
  ## Finally, we're going to add failures to the whole df so we can check known/unknown below
  ## This is just the number of trials - the number of successess
  ## We're also replacing NAs with 0s for num false gves
  full_wynn_df <- full_wynn_df %>%
    mutate(num_failures = num_trials - num_correct, 
           num_false_give = ifelse(is.na(num_false_give), 0, as.numeric(num_false_give)))
  return(full_wynn_df)
}

## Run all of this on the trial data
final_wynn_df <- create_wynn_df(trial_level_data) #hooray this works!


## Next! On the basis of these totals, we need to determine whether numbers are known or unknown
### Conditions for "possibly known" (all of these are on query-level basis) (these are reflected in order of ifelse statements below): 
#### a. If the number of successes/number of trials >= 2/3 AND there are no false gives
#### b. If the number of successes/number of trials >= 2/3 AND number of successes/(number of successes + number of false gives) >= 2/3

### Conditions for definitely unknown: 
#### a. If number of successes/number of trials < 2/3
#### b. If number of successes/(number of successes + number of false gives) < 2/3
#### c. If number of successes/(number of failures + number of false gives) < 2/3

## Finally, I created a 'tracker' variable to make sure we were dealing with every case to make sure that we're uniquely assigning known/unknown
## So far this seems to be workng??

##TODO: this is failing when there are false gives and failures

tmp_assignment <- final_wynn_df %>%
  mutate(possibly_known = ifelse(((num_correct/num_trials >= 2/3) & num_false_give == 0), 1, #if they are successful 2/3 of the time with no false gives
                                 ifelse((((num_correct/num_trials >= 2/3) & 
                                           ((num_correct/(num_correct+num_false_give)) >= 2/3) & (num_correct/(num_false_give+num_failures)) >= 2/3)), 1, 0)), #if they are successful 2/3 time and of times they gave n it was in response to n at least 2/3 of the time
         definitely_unknown = ifelse(((num_correct/num_trials) < 2/3), 1, 
                                     ifelse(((num_correct/(num_correct+num_false_give)) < 2/3), 1,
                                            ifelse((num_correct/(num_false_give + num_failures) < 2/3), 1, 0))), 
         ## the 'tracker' is using known and unknown to identify a particular number as DEFINITELY known or DEFINITELY unknown
         tracker = ifelse((possibly_known == 1 & definitely_unknown == 0), 1, # this is definitely known
                          ifelse((possibly_known == 0 & definitely_unknown == 1), 0, # this is definitely unknown
                                 ifelse((possibly_known == 1 & definitely_unknown == 1), "HELP", # this is conflicting, seems to be known and unknown
                                        ifelse((possibly_known == 0 & definitely_unknown == 0), "CHECK", "Other")))))  #This would probably come up when we have no data for a trial

##TODO - write function that checks for issues

## Okay and this is where the magic happens
## For each subject, for each query that was tested within that dataset, we have it determined as known or unknown
## Now we need to use that information to assign a knower level
## The key assumption of this model is that we're assigning contiguous KLs - e.g., if kid knows 1, 2, and 4, they're credited as a 2-knower
## We're doing this row by row (ie number by number); This is assuming that the values are sorted in ascending numerical order, which I believe is not enforced anywhere


### NB will have to handle by dataset as well for SIDs that might have been tested twice but have different KLs
track_successes <- function(df) {
  
  ## Okay this is a little messy, but what I'm trying to do is identify successes and uniquely identify them so we can assign KL using the following logic:
  ## FYI I'm using -50 and -100 as numeric placeholder for failures, whereas successes are simply the queried number
  ### a. If the queried number is 1 and the child failed it, they're a non-knower (because they failed 1); This is identified as -100 in the success tracker
  ### b. Otherwise, we're keeping track of the last successful number
  ### c. If the current tracker = 0 (ie the current number is not known) but the last number is known, success tracker is -50
  ### d. if the current tracker = 0 and previous tracker = 0, success tracker is -50
  tmp.df <- df %>%
    mutate(success_tracker = ifelse((tracker == 0 & Query == 1), -100, #if this is the first # and tracker is 0, NON
                                    ifelse(tracker == 1, Query, #otherwise, keep track of last successful number
                                           ifelse((tracker == 0 & lag(tracker) == 1), -50, 
                                                  ifelse((tracker == 0 & lag(tracker) == 0), -50, "help")))))
  
  ##TODO - add function that will throw error if there is a help in this df
  
  ## Okay so now we have to do this for each subject
  ## Get all the unique subject IDs
  unique_subs <- as.vector(unique(tmp.df$Subject))
  ## also need to do this by datasets because we have sometimes non-unique SIDs - this will be fixed when we switch to indexing participant ID from 0
  unique_datasets <- as.vector(unique(tmp.df$dataset))
  ## Make a placeholder DF where we can push data
  tracker.df <- data.frame()

  ## Loop through each dataset
  for (d in unique_datasets) {  
    d.subset <- df %>%
      filter(dataset == d)
    ## Loop through each subject
    for (s in unique_subs) {
      s.df <- tmp.df %>%
        filter(Subject == s)
      
      ## If the minimum of the success tracker is -100, they're a non-knower
      if (min(s.df$success_tracker) == -100) {
        tmp.s.df <- data.frame(Subject = s, 
                       Assignment = '0', 
                       Experiment = d)
      ## Otherwise, if they haven't gotten anything wrong and there are no NAs, then they've succeeded on everything
        ## so their KL assignment is the highest number tested
      } else if (min(s.df$success_tracker) > 0) { #if they haven't gotten anything wrong, and there are no NAs
        highest_num <- max(s.df$success_tracker)
        tmp.s.df <- data.frame(Subject = s, 
                               Assignment = as.character(highest_num), 
                               Experiment = d)
      ## If they've gotten something incorrect, then get the MINIMUM number for which they failed and filter out anything above this number (because KLs are contiguous)
      ## Then get the maximum number in their success tracker after this
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
                               Assignment = as.character(max.succeed.num), 
                               Experiment = d)
      ## If we get to the end and we haven't assigned a KL, it will be -500 for debugging  
      } else {
        tmp.s.df <- data.frame(Subject = s, 
                               Assignment = '-500', 
                               Experiment = d)
      }
      #bind these suckers together
      tracker.df <- bind_rows(tracker.df, tmp.s.df)
    }
  }
  return(tracker.df)
}

checking_assignments <- track_successes(tmp_assignment) # this is taking a really long time and has a bunch of warnings, not sure why 

## Add CP assignment
checking_assignments <- checking_assignments %>%
  mutate(Assignment = ifelse(((Experiment == "Almoammer2013" | Experiment == "Marusic2016") & as.numeric(as.character(Assignment)) >=5), "CP", 
                             ifelse(as.numeric(as.character(Assignment)) >= 6, "CP", as.character(Assignment))))
           
##TODO - special cases for CP??

#bind with original for checking
tmp_check <- left_join(all_data, checking_assignments, by = c("Subject", "Experiment"))%>%
  mutate(KL_check = ifelse(KL != Assignment, "FLAG", "Fine"))

flags <- tmp_check %>%
  filter(KL_check == "FLAG")%>%
  distinct(Subject, Experiment, KL,Assignment)


