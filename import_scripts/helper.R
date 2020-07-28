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
  
  write_csv(subjects, here::here("data/processed-data/subjects.csv"))
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
}

## function for trial_id
create_zero_index <- function(data, id_column_name="Subject") {
  data <- data %>%
    mutate(query_lag = lag(Query), 
           temp = ifelse(Query != query_lag, 1, 0), 
           temp_id = cumsum(c(0, temp[!is.na(temp)])), 
           trial_id = temp_id)
}
