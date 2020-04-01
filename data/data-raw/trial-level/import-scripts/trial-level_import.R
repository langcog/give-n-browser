##Cleaning and processing script for trial-level give-n data
## Convention = Experiment; Subject; Language; Age; Age_years; Query; Response

##set up
rm(list = ls())
library(tidylog)
library(tidyverse)

# Read in data ----
## Lot of variability in file set-up; first read in the most atypical conventions
## We'll later merge these with more typical conventions

## ... almoammer2013 ----
almoammer2013 <- read.csv('../data-raw/Almoammer2013_english.csv', 
                          check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, - Participant, - Language, -PrimaryLang, -Age_months, -SEX), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, Query, Response)%>%
  dplyr::rename("Subject" = "Participant", 
                "Age" = "Age_months")
  
# ...piantadosi2014 ----
piantadosi2014 <- read.csv('../data-raw/Piantadosi2014.csv', 
                           check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language, -`Knower-level`, -Age, -Sex, -Education), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age, Query, Response)%>%
  dplyr::rename("Subject" = "Participant") ##TO-DO: Age is in years, convert to rounded months, I guess

