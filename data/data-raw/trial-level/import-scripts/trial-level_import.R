##Cleaning and processing script for trial-level give-n data
## Convention = Experiment; Subject; Language; Age (decimal); Query; Response; cite

##set up
rm(list = ls())
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
         lab = "Barner", 
         cite = "Almoammer, A., Sullivan, J., Donlan, C., Marusic, F., O’Donnell, T., & Barner, D. (2013). Grammatical morphology as a source of early number word meanings. Proceedings of the National Academy of Sciences, 110(46), 18448-18453.")%>%
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
         lab = "Piantadosi", 
         cite = "Piantadosi, S. T., Jara‐Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming‐foraging group. Developmental Science, 17(4), 553-563.")##Age is in years, convert to rounded months, I guess

## ..... pull out KL data, send to raw data for processing ----
piantadosi2014_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/Piantadosi2014.csv'))%>%
  distinct(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  dplyr::select(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  rename("Subject" = "Participant")%>%
  mutate(Age = Age*12, 
         Subject = as.character(Subject), 
         lab = "Piantadosi", 
         cite = "Piantadosi, S. T., Jara_Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming_foraging group. Developmental Science, 17(4), 553-563.")

write_csv(piantadosi2014_kl, here::here('data/data-raw/kl-only/data-raw/piantadosi_2014.csv'))
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
         lab = "Sarnecka", 
         cite = "Sarnecka, B. W., Kamenskaya, V. G., Yamana, Y., Ogura, T., & Yudovina, Y. B. (2007). From grammatical number to exact numbers: Early meanings of ‘one’,‘two’, and ‘three’in English, Russian, and Japanese. Cognitive psychology, 55(2), 136-168.")

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
         lab = "Barner", 
         cite = "Wagner, K., Chu, J., & Barner, D. (2019). Do children's number words begin noisy?. Developmental science, 22(1), e12752.")

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
         lab= "Piantadosi", 
         cite = "Boni et al. (unpublished data)")%>%
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
         lab= "Piantadosi", 
         cite = "Boni et al. (unpublished data)")%>%
  dplyr::rename("Sex" = "Gender")

## now read in the more typical conventions 
## ...krajcsi2018 ----
krajcsi2018 <- read_csv(here::here("data/data-raw/trial-level//data-raw/Krajcsi2018.csv"))%>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response))%>%
  rename("Subject" = "Participant")%>%
  mutate(Subject = as.character(Subject), 
         lab = "Krajcsi", 
         cite = "Krajcsi, A. (2019, March 19). Follow-up questions influence the measured number knowledge in the Give a number task. https://doi.org/10.31234/osf.io/fky69")

## ...sarnecka2019 ----
sarnecka2019 <- read_csv(here::here('data/data-raw/trial-level/data-raw/SarneckaNegen2019.csv'))%>%
  dplyr::rename("Sex" = "Gender", 
                "Query" = "Test",
                "Subject" = "SubjNo")%>%
  mutate(Language = "English", 
         Subject = as.character(Subject), 
         Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Sarnecka", 
         Experiment = "SarneckaNegen2019", 
         cite = "Sarnecka, B. W., & Negen, J. (2019, May 30). Longitudinal Number-Knower Data. https://doi.org/10.17605/OSF.IO/EZNHT")%>%
  dplyr::select(-Task)

## ... schneider & barner --- 
##1-1 sharing
schneider_barner_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_20xx.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderBarner_20xx',
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R.M., Feiman, R., & Barner, D. (unpublished dataset).")%>%
  filter(!is.na(Task_item))%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

## .....output KL data to data-raw -----
schneider_barner_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_20xx.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderBarner_20xx',
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R.M., Feiman, R., & Barner, D. (unpublished dataset).")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_barner_20xx_kl, here::here('data/data-raw/kl-only/data-raw/schneider_barner_20xx_kl.csv'))

##1-1 baseline
schneider_barner_2020 <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_2020.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderBarner_2020',
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R.M., & Barner, D. (2020). Children use one-to-one correspondence to establish equality after learning to count. 42nd Annual Meeting of the Cognitive Science Society.")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
schneider_barner_2020_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_barner_2020.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderBarner_2020', 
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R.M., & Barner, D. (2020). Children use one-to-one correspondence to establish equality after learning to count. 42nd Annual Meeting of the Cognitive Science Society.")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_barner_2020_kl, here::here('data/data-raw/kl-only/data-raw/schneider_barner_2020.csv'))

##small sf
schneider_etal_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderEtAl_20xx',
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
schneider_etal_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx.csv'))%>%
mutate(method = 'titrated', 
       Experiment = 'SchneiderEtAl_20xx',
       Age = Age*12, 
       lab = "Barner", 
       cite = "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_etal_20xx_kl, here::here('data/data-raw/kl-only/data-raw/schneider_etal_20xx_kl.csv'))

##small sf 2
schneider_etal_20xx_2 <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx_2.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderEtAl_20xx', 
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw
schneider_etal_20xx_2_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx_2.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderEtAl_20xx',
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_etal_20xx_2_kl, here::here('data/data-raw/kl-only/data-raw/schneider_etal_20xx_2_kl.csv'))

##conservation
schneider_yen_20xx <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_yen_barner_20xx.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderYen_20xx', 
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##write kls
schneider_yen_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_yen_barner_20xx.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         Experiment = 'SchneiderYen_20xx', 
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(schneider_yen_20xx_kl, here::here('data/data-raw/kl-only/data-raw/schneider_yen_20xx_kl.csv'))

## xculture
schneider_etal_2020 <- read.csv(here::here('data/data-raw/trial-level/data-raw/SchneiderEtAl_2020.csv'))%>%
  filter(Age != "#VALUE!", 
         Query != "TireHydrant",
         Query != "DuckClover", 
         Query != "CloverKite", 
         !is.na(Query), 
         !is.na(Response))%>%
  mutate(Age = 12*as.numeric(as.character(Age)), 
         Query = as.numeric(as.character(Query)), 
         method = "Non-titrated", 
         cite = "Schneider, R. M., Sullivan, J., Marusic, F., Biswas, P., Mismas, P., Plesnicar, V., & Barner, D. (2020). Do children use language structure to discover the recursive rules of counting?. Cognitive psychology, 117, 101263.")

#calculate KL - this is either subset or CP
xculture.kl <- schneider_etal_2020 %>%
  mutate(Correct = ifelse(Query == Response, 1, 0))%>%
  group_by(Subject)%>%
  summarise(n_correct = sum(Correct))%>%
  mutate(KL = ifelse(n_correct >= 4, "CP", "Subset"))%>%
  select(-n_correct)

## get demo information for KL
schneider_etal_2020_kl <- schneider_etal_2020 %>%
  distinct(Experiment, Subject, Age, Language, Sex)%>%
  left_join(xculture.kl, by = "Subject")

## write to KL 
write.csv(schneider_etal_2020_kl, here::here("data/data-raw/kl-only/data-raw/SchneiderEtAl_2020.csv"))


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
                      schneider_yen_20xx, 
                      schneider_etal_2020)%>%
  dplyr::rename('Age_months'='Age')%>%
  filter(!is.na(Response),
         !is.na(Age_months), 
         Response >= 1)%>% ##get rid of NAs 
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  mutate(typical = ifelse(Experiment == "SchneiderEtAl_2020", "non-typical", "typical"))%>%
  dplyr::select(Experiment, lab, Language, Subject, Age_months, Age_years, method, Query, Response, cite, typical)

# Save and export ----
write_csv(all.data, here::here('data/processed-data/trial_level_processed_data.csv'))
