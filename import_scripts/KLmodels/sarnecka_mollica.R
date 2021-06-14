
## ... Lee & Sarnecka (2010) KL assignment ----
## This version uses Frank Mollica's package implementing the Sarnecka Bayesian algorithm for inferring KL


##  FM: This model considers six hypotheses about a child's performance on the giveN task. A child can be a 
#####   Non-, One-, Two-, Three-, Four- or CP-knower. Each knower level $k$ makes a different prediction for 
#####   how children perform on the giveN task. The model starts by specifying a probability distribution 
#####   $\pi$ over the number of items a child will return--i.e., their response $r$, regardless of the query 
#####   $q$. Upon hearing a query, children should update this distribution depending on their knower level. 
#####   Let's call this updated distribution over responses $\pi'$. If the child is a non-knower ($k=0$), they 
#####   should not update their response distribution when they hear a query, and so $\pi = \pi'$. If a child 
#####   is a subset knower ($1 \leq k \leq 4$), they should update the correct response and downweight the 
#####   incorrect response for all the numbers they know. Lastly, if the child is a CP-knower ($k=5$), they 
#####   should upweight the correct answer and downweight the incorrect answer for all queries. 


## Loading up libraries

library(tidyverse)
library(rstan) 
library(tidybayes)
helperDir = "import_scripts/KLmodels/sarnecka_mollica_helper/"
R.sources = paste0(here(helperDir),list.files(here(helperDir)))
sapply(R.sources, source)



## rstan

create_sarnecka_samples <- function(df){
  samples = df %>%
    mutate(Participant = paste0(Experiment, "_", Subject)) %>%
    sample_kl(iter=2000,
              chains=4,
              cores=3)
  knowers = extract_kl(samples, method='mode')
  return(knowers)
}

trial_level_data %>%
  count(Experiment)

testDataSet = "SchneiderBarner_20xx"

checkrun <- function(msg="Are you sure you want to run Lee & Sarnecka (2010) KL inference? ") {
  if (interactive() ) {
    txt <- readline(msg)
    if(tolower(txt) %in% c("yes","y")){
      print("running...")
      final_sarnecka_df <- create_sarnecka_samples(filter(trial_level_data, Experiment == testDataSet)) %>%
        mutate(KL = as.character(KL), # convert character KL assignment to numbers
               KL = case_when(
                 KL == "Non" ~ "0",
                 KL == "One" ~ "1",
                 KL == "Two" ~ "2",
                 KL == "Three" ~ "3",
                 KL == "Four" ~ "4",
                 TRUE ~ KL
               )
        ) %>%
        rename(KL.sarnecka = KL)
      print("printing head of KL assignments...")
      head(final_sarnecka_df)
    }
    return(final_sarnecka_df)
  } else {
    cat(msg)
  }
}
final.df <- checkrun()

flagged <- fullWynn %>%
  filter(Experiment == testDataSet) %>%
  mutate(Participant = paste0(Experiment, "_", Subject))%>%
  rename(KL.wynn = KL.assign) %>%
  left_join(final.df) %>%
  select(Participant, Age_years, method, Query, Response, KL, KL.wynn, KL.sarnecka) %>%
  filter(KL.sarnecka != KL.wynn | KL.sarnecka != KL) # flag participants: KL.sarnecka != with either KL.wynn or manually assigned KL

View(flagged)
length(unique(flagged$Participant))
fullWynn %>%
  filter(Experiment == testDataSet) %>%
  distinct(Subject) %>%
  nrow()

# 15 / 92 participants flagged
