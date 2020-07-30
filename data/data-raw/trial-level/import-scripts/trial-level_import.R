##Cleaning and processing script for trial-level give-n data
## Convention = Experiment; Subject; Language; Age_months; Age_years; Query; Response

##set up
rm(list =)
library(tidylog)
library(tidyverse)


# Read in data ----
## Lot of variability in file set-up; first read in the most atypical conventions
## We'll later merge these with more typical conventions
## if there is a KL assigned in the data, we'll export it to KL-only data
##otherwise, we'll have to calculate KLs using wynn criteria

## ... almoammer2013 ----
almoammer2013_english <- read_csv(here::here('data/data-raw/trial-level/data-raw/Almoammer2013_english.csv'))%>%
  pivot_longer(cols = c(-Experiment, - Participant, - Language,
                        -PrimaryLang, -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  mutate(Query = str_remove(Query, "_1"), 
         Query = str_remove(Query, "_2"), 
         lab = "Barner")%>%
  rename("Subject" = "Participant", 
                "Age" = "Age_months", 
                "Sex" = "SEX")%>%
  dplyr::mutate(Query = as.integer(Query), 
                Response = as.integer(Response))
  
# ...piantadosi2014 ----
piantadosi2014 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Piantadosi2014.csv'))%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language, -`Knower-level`,
                        -Age, -Sex, -Education, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age, method, cite, Sex, Query, Response)%>%
  rename("Subject" = "Participant")%>%
  mutate(Age = Age*12, 
         Subject = as.character(Subject), 
         Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Piantadosi")##Age is in years, convert to rounded months, I guess

## pull out KL data, send to raw data for processing
piantadosi2014_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/Piantadosi2014.csv'))%>%
  distinct(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  dplyr::select(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  rename("Subject" = "Participant")%>%
  mutate(Age = Age*12, 
         Subject = as.character(Subject), 
         lab = "Piantadosi")

write_csv(piantadosi2014_kl, 'data/data-raw/kl-only/data-raw/piantadosi_2014.csv')

# ...sarnecka2007 ----
sarnecka2007 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Sarnecka2007.csv'))%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language,
                        -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  mutate(Query = str_remove(Query, "_1"), 
         Query = str_remove(Query, "_2"))%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  rename("Subject" = "Participant", 
                "Sex" = "SEX", 
                "Age" = "Age_months") %>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Sarnecka")

## ... wagner2016 ----
wagner2016 <- read.csv(here::here('data/data-raw/trial-level/data-raw/Wagner2016.csv'))%>%
  pivot_longer(cols = c(-Experiment, - Participant, -Language,
                        -PrimaryLang, -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  mutate(Query = str_remove(str_remove(str_remove(Query, "X"), ".1"), ".2"), 
         Experiment = ifelse(Experiment == "Excluded", "Wagner2019", "Wagner2019"))%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  dplyr::rename("Subject" = "Participant", 
                "Age" = "Age_months", 
                "Sex" = "SEX")%>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response),
         lab = "Barner")

## ...boni (unpublished, ordered Give) ----
boni20xx_ordered <- read_csv(here::here('data/data-raw/trial-level/data-raw/Boni20XX.csv')) %>%
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
         Query = as.integer(str_remove(Query, "OrderedGiveN")), 
         Response = as.integer(str_remove(str_remove(Response, "\\d_"), "_")), 
         Subject = as.character(Subject), 
         lab= "Piantadosi")%>%
  dplyr::rename("Sex" = "Gender")

## ...boni (unpublished, random Give) ----
boni20xx_random <- read_csv(here::here('data/data-raw/trial-level/data-raw/Boni20XX.csv')) %>%
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
         Query = as.integer(str_remove(str_remove(Query, "GiveNRandom"), "B")), 
         Response = as.integer(str_remove(str_remove(Response, "\\d_"), "_")), 
         Subject = as.character(Subject), 
         lab= "Piantadosi")%>%
  dplyr::rename("Sex" = "Gender")

## now read in the more typical conventions
krajcsi2018 <- read_csv(here::here("data/data-raw/trial-level//data-raw/Krajcsi2018.csv"))%>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response))%>%
  rename("Subject" = "Participant")%>%
  mutate(Subject = as.character(Subject), 
         lab = "Krajcsi")

sarnecka2019 <- read_csv(here::here('data/data-raw/trial-level/data-raw/SarneckaNegen2019.csv'))%>%
  dplyr::rename("Sex" = "Gender", 
                "Query" = "Test",
                "Subject" = "SubjNo")%>%
  mutate(Language = "English", 
         Subject = as.character(Subject), 
         Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Sarnecka", 
         Experiment = "SarneckaNegen2019")%>%
  dplyr::select(-Task)

## schneider & barner --- 
##1-1 sharing
schneider_barner_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_20xx.csv'))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderBarner_20xx', 
         Experiment = cite,
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner")%>%
  filter(!is.na(Task_item))%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
schneider_barner_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_20xx.csv'))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderBarner_20xx', 
         Experiment = cite,
         Age = Age*12, 
         lab = "Barner")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_barner_20xx_kl, 'data/data-raw/kl-only/data-raw/schneider_barner_20xx_kl.csv')

##1-1 baseline
schneider_barner_2020 <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_2020.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderBarner_2020', 
         Experiment = cite,
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
schneider_barner_2020_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_2020.csv'))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderBarner_2020', 
         Experiment = cite,
         Age = Age*12, 
         lab = "Barner")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_barner_2020_kl, 'data/data-raw/kl-only/data-raw/schneider_barner_2020.csv')

##small sf
schneider_etal_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderEtAl_20xx', 
         Experiment = cite,
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
schneider_etal_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx.csv'))%>%
mutate(method = 'titrated', 
       cite = 'SchneiderEtAl_20xx', 
       Experiment = cite,
       Age = Age*12, 
       lab = "Barner")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_etal_20xx_kl, 'data/data-raw/kl-only/data-raw/schneider_etal_20xx_kl.csv')

##small sf 2
schneider_etal_20xx_2 <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx_2.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderEtAl_20xx', 
         Experiment = cite,
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw
schneider_etal_20xx_2_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx_2.csv'))%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderEtAl_20xx', 
         Experiment = cite,
         Age = Age*12, 
         lab = "Barner")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_etal_20xx_2_kl, 'data/data-raw/kl-only/data-raw/schneider_etal_20xx_2_kl.csv')

##conservation
schneider_yen_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_yen_barner_20xx.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderYen_20xx', 
         Experiment = cite,
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##write kls
schneider_yen_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_yen_barner_20xx.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         cite = 'SchneiderYen_20xx', 
         Experiment = cite,
         Age = Age*12, 
         lab = "Barner")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_yen_20xx_kl, 'data/data-raw/kl-only/data-raw/schneider_yen_20xx_kl.csv')

# Bind everything together ----
all.data <- bind_rows(almoammer2013_english, 
                      wagner2016, 
                      piantadosi2014, 
                      sarnecka2007, 
                      sarnecka2019, 
                      krajcsi2018, 
                      boni20xx_ordered, 
                      boni20xx_random, 
                      schneider_barner_2020, 
                      schneider_barner_20xx, 
                      schneider_etal_20xx, 
                      schneider_etal_20xx_2, 
                      schneider_yen_20xx)%>%
  dplyr::rename('Age_months'='Age')%>%
  filter(!is.na(Response),
         !is.na(Age_months))%>% ##get rid of NAs 
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, lab, Language, Subject, Age_months, Age_years, method, Query, Response, cite)

# Save and export ----
write_csv(all.data, here::here('data/processed-data/trial_level_processed_data.csv'))
