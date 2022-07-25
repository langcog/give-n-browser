## Helper functions for numberbank

## Writing data ====

# write_to_subjects
write_to_subjects <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/subjects.csv"))) {
    subjects <- read_csv(here::here("data/processed-data/subjects.csv")) 
    subjects <- bind_rows(subjects, df)
  } else {
    subjects <- df 
  }
  
  write_csv(subjects, here::here("data/processed-data/subjects.csv")) # write to data/processed-data
  write_csv(subjects, here::here("shiny_app/data/processed-data/subjects.csv")) # write to shiny app data/processed-data
}

# write_to_datasets
write_to_datasets <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/datasets.csv"))) {
    datasets <- read_csv(here::here("data/processed-data/datasets.csv"))
    datasets <- bind_rows(datasets, df)
  } else {
    datasets <- df 
  }
  
  write_csv(datasets, here::here("data/processed-data/datasets.csv"))
  write_csv(datasets, here::here("shiny_app/data/processed-data/datasets.csv"))
}


# write_to_trials
write_to_trials <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/trials.csv"))) {
    trials <- read_csv(here::here("data/processed-data/trials.csv"))
    trials <- bind_rows(trials, df)
  } else {
    trials <- df 
  }
  
  write_csv(trials, here::here("data/processed-data/trials.csv"))
  write_csv(trials, here::here("shiny_app/data/processed-data/trials.csv"))
}


## Wrangling data ====
## function for trial_id
create_zero_index <- function(data, id_column_name="Subject") {
  data <- data %>%
    mutate(query_lag = lag(Query), 
           temp = ifelse(Query != query_lag, 1, 0), 
           temp_id = cumsum(c(0, temp[!is.na(temp)])), 
           trial_id = temp_id)
}

#combining trial level and kl level data 
combine_data <- function(df1, df2) {
  ##strip KL data down 
  kls <- kl_only_data %>%
    dplyr::select(Experiment, Subject, KL)
  
  ##join matching rows
  ##so this df has the trial-level data, and trial-level data that has KLs
  ##but otherwise it does not have the KL data
  ##so we need to pull that in
  trial_with_kl <- left_join(trial_level_data, kls, by = c("Experiment", "Subject"))
  
  ##so now we need the kl data that does not have a match in trial_level data
  kls_without_trial <- anti_join(kl_only_data, trial_level_data, by = c("Experiment", "Subject"))
  
  ##now we can put these together
  all_data <- full_join(trial_with_kl, kls_without_trial)
  
  return(all_data)
}

## Add in highest count data when appropriate ====
add_hc <- function(df) {
  ## read in highest count data
  highest_count <- read_csv(here("shiny_app/data/processed-data/highest_count.csv"))%>%
    distinct(SID, highest_count, Experiment)%>%
    rename("Subject" = "SID")
  
  ## now combine this with all data when appropriate
  df <- left_join(df, highest_count, by = c("Subject", "Experiment"))
  return(df)
}

## Add in country information
add_country <- function(df) {
  country <- read_csv(here("shiny_app/data/processed-data/country.csv"))
  country_by_subj <- read_csv(here("shiny_app/data/processed-data/country_by_subj.csv"))
  df1 <- filter(df, Experiment %in% country$Experiment)
  df2 <- filter(df, Experiment %in% country_by_subj$Experiment)
  df1 <- left_join(df1, country, by = c("Experiment", "Language"))
  df2 <- left_join(df2, country_by_subj, by = c("Experiment", "Subject"))
  df <- rbind(df1, df2)
  return(df)
}
