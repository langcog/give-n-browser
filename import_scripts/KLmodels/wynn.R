
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
      group_by(Experiment, Subject, Query)%>%
      summarise(num_trials = n(),
                num_correct = sum(correct))
    
    ## Next, we look at the actual numbers that children were giving incorrectly on false gives
    ## This will let us sum up, for a given query, how often children gave that number falsely for another number
    false_give_df <- d.subset %>% # false gives
      filter(!is.na(resp_false_give))%>%
      group_by(Experiment, Subject, resp_false_give)%>%
      summarise(num_false_give = sum(false_give))%>%
      dplyr::rename("Query" = "resp_false_give")
    
    #Now, left join the correct and the false give summaries together:
    ## This will give us: Subject, Query, num_trials (how many trials child was asked about n), num_correct (successes), num_false_gives, dataset
    tmp_wynn_df <- left_join(correct_df, false_give_df, by = c("Subject", "Experiment", "Query")) 
    
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

## assign whether N known for each subject's N queried
nKnow.df <- final_wynn_df %>% 
  filter(!is.na(Query))%>%
  mutate(knowN = case_when(
    num_correct / num_trials < 2/3 | 
      num_correct / (num_correct+num_false_give) < 2/3 | 
      num_correct / (num_failures+num_false_give) < 2/3 ~ -1, # definitely does not know N
    num_correct / num_trials >= 2/3 & num_false_give == 0 |
      num_correct / num_trials >= 2/3 & num_correct / (num_correct + num_false_give) >= 2/3 ~ Query, # maybe knows N
    TRUE ~ NaN # if neither of the rules, then NaN appears ==> there's an Error
  )) # no Errors seem to be occurring


### To assign KLs: 
## 1. If there are no successes, then the kid is a non-knower
## 2. If there are no failures, then the kid is a max number knower (w/e the max number is) 
##this handles the easy cases above
easyAssign <- nKnow.df %>%
  group_by(Experiment, Subject)%>%
  mutate(KL.assign  = case_when(
    Query == 1 & knowN == -1 ~ 0,
    max(knowN) <= -1 ~ min(Query), 
    min(knowN) > 0 ~ max(Query))
  )%>%
  group_by(Experiment, Subject)%>%
  mutate(KL.assign = ifelse(0 %in% KL.assign, 0, as.numeric(KL.assign)))



## Now we need to handle kids who have a mix of successes and errors
## We need to get contiguous KLs for kids, and need to only assign KLs on the basis of the numbers we've tested
## First getting the lowest failure
lowestFail <- easyAssign %>%
  filter(is.na(KL.assign), 
         knowN == -1)%>%
  group_by(Experiment, Subject)%>%
  summarise(minFail = min(Query))

## Then using that lowest failure to get the highest number below that (that was queried) for which we have data
highestSuccess <- easyAssign %>%
  filter(is.na(KL.assign))%>%
  left_join(lowestFail) %>%
  group_by(Experiment, Subject)%>%
  filter(Query < minFail)%>%
  mutate(KL.assign = max(Query))%>%
  select(-minFail)%>%
  distinct(Experiment, Subject, KL.assign)

## Now get everyone else
nonMaxKnowers <- easyAssign%>%
  filter(!is.na(KL.assign))%>%
  distinct(Experiment, Subject, KL.assign)

wynnAssignments <- bind_rows(nonMaxKnowers, highestSuccess)

## now join these to the full dataset
fullWynn <- all_data %>%
  left_join(wynnAssignments)%>%
  mutate(KL.assign = ifelse(((Experiment == "Almoammer2013" | Experiment == "Marusic2016") 
                             & as.numeric(as.character(KL.assign)) >=5), "CP", 
                            ifelse(as.numeric(as.character(KL.assign)) >= 6, "CP", as.character(KL.assign))))

##check for issues - this will only show issues when there is an experimenter-assigned KL
## TODO: spot check for other datasets
flags <- fullWynn %>%
  filter(KL != KL.assign)%>%
  filter(KL != "Subset")

##write to csv for troubleshooting
write.csv(flags, here("import_scripts/wynn_checks_1.csv"))

## now need to do a spot check for other datasets
spotCheck <- fullWynn %>%
  filter(is.na(KL))


