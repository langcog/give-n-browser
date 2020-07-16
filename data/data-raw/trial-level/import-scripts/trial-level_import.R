##Cleaning and processing script for trial-level give-n data
## Convention = Experiment; Subject; Language; Age_months; Age_years; Query; Response

##set up
library(tidylog)
library(tidyverse)

# Read in data ----
## Lot of variability in file set-up; first read in the most atypical conventions
## We'll later merge these with more typical conventions

## ... almoammer2013 ----
almoammer2013 <- read.csv('../data-raw/Almoammer2013_english.csv', 
                          check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, - Participant, - Language,
                        -PrimaryLang, -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  dplyr::rename("Subject" = "Participant", 
                "Age" = "Age_months", 
                "Sex" = "SEX")%>%
  dplyr::mutate(Query = as.integer(Query))
  
# ...piantadosi2014 ----
piantadosi2014 <- read.csv('../data-raw/Piantadosi2014.csv', 
                           check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language, -`Knower-level`,
                        -Age, -Sex, -Education, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age, method, cite, Sex, Query, Response)%>%
  dplyr::rename("Subject" = "Participant")%>%
  mutate(Age = Age*12, 
         Subject = as.character(Subject), 
         Query = as.integer(Query))##Age is in years, convert to rounded months, I guess


# ...sarnecka2007 ----
sarnecka2007 <- read.csv('../data-raw/Sarnecka2007.csv', 
                           check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language,
                        -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  dplyr::rename("Subject" = "Participant", 
                "Sex" = "SEX", 
                "Age" = "Age_months") %>%
  mutate(Query = as.numeric(Query))

## ... wagner2016 ----
wagner2016 <- read.csv('../data-raw/Wagner2016.csv', 
                          check.names = FALSE)%>%
  pivot_longer(cols = c(-Experiment, - Participant, -Language,
                        -PrimaryLang, -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  dplyr::rename("Subject" = "Participant", 
                "Age" = "Age_months", 
                "Sex" = "SEX")%>%
  mutate(Query = as.numeric(Query))

## ...boni (unpublished, ordered Give) ----
boni20xx_ordered <- read.csv('../data-raw/Boni20XX.csv', check.names = FALSE) %>%
  dplyr::select(-starts_with("GiveN"))%>%
  mutate(Experiment = "Boni20xx_ordered", 
         Language = "Tsimane")%>%
  pivot_longer(cols = c(-Experiment, -Subject, 
                        -Language, -`Location Code`, 
                        -Education, -Age, -Gender, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(-`Location Code`, -Education)%>%
  mutate(Age = 12*Age, 
         Query = as.numeric(str_remove(Query, "OrderedGiveN")), 
         Response = as.numeric(str_remove(str_remove(Response, "\\d_"), "_")), 
         Subject = as.character(Subject))%>%
  dplyr::rename("Sex" = "Gender")

## ...boni (unpublished, random Give) ----
boni20xx_random <- read.csv('../data-raw/Boni20XX.csv', check.names = FALSE) %>%
  dplyr::select(-starts_with("Ordered"))%>%
  mutate(Experiment = "Boni20xx_random", 
         Language = "Tsimane")%>%
  pivot_longer(cols = c(-Experiment, -Subject, 
                        -Language, -`Location Code`, 
                        -Education, -Age, -Gender, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(-`Location Code`, -Education)%>%
  mutate(Age = 12*Age, 
         Query = as.numeric(str_remove(str_remove(Query, "GiveNRandom"), "B")), 
         Response = as.numeric(str_remove(str_remove(Response, "\\d_"), "_")), 
         Subject = as.character(Subject))%>%
  dplyr::rename("Sex" = "Gender")

## now read in the more typical conventions
krajcsi2018 <- read.csv("../data-raw/Krajcsi2018.csv")%>%
  mutate(Query = as.numeric(Query))%>%
  rename("Subject" = "Participant")%>%
  mutate(Subject = as.character(Subject))

sarnecka2019 <- read.csv('../data-raw/SarneckaNegen2019.csv')%>%
  dplyr::rename("Sex" = "Gender", 
                "Query" = "Test",
                "Subject" = "SubjNo")%>%
  mutate(Language = "English", 
         Subject = as.character(Subject), 
         Query = as.numeric(Query))%>%
  dplyr::select(-Task)

# Bind everything together ----
all.data <- bind_rows(almoammer2013, 
                      wagner2016, 
                      piantadosi2014, 
                      sarnecka2007, 
                      sarnecka2019, 
                      krajcsi2018, 
                      boni20xx_ordered, 
                      boni20xx_random)%>%
  dplyr::rename('Age_months'='Age')%>%
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, Language, Subject, Age_months, Age_years, method, Query, Response)

# Save and export ----
write_csv(all.data, here("data/processed-data/kl_data.csv"))