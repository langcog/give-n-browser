##Cleaning and processing script for trial-level give-n data
## Convention = Experiment; Subject; Language; Age (decimal); Query; Response; cite

##set up
rm(list = ls())
library(tidylog)
library(tidyverse)
library(haven)


# Read in data ----
## Lot of variability in file set-up; first read in the most atypical conventions
## We'll later merge these with more typical conventions
## if there is a KL assigned in the data, we'll export it to KL-only data
##otherwise, we'll have to calculate KLs using wynn criteria

## functions ##

reformatDate <- function(date){
  date.a <- as.Date(date, format="%m/%d/%Y")
  date.b <- as.Date(date, format="%m.%d.%Y")
  date.c <- as.Date(date, format="%m-%d-%y")
  date.d <- as.Date(date, format="%m-%d-%Y")
  case_when(
    !is.na(date.a) ~ date.a,
    !is.na(date.b) ~ date.b,
    !is.na(date.c) ~ date.c,
    !is.na(date.d) ~ date.d,
    TRUE ~ as.Date(date, format="%Y-%m-%d")
  )
}


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
krajcsi2018 <- read_csv(here::here("data/data-raw/trial-level/data-raw/Krajcsi2018.csv"))%>%
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
Schneider_Barner_UnderReview <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_UnderReview.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'Schneider_Barner_UnderReview',
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R.M., Brockbank, E., Feiman, R., & Barner, D. (under review). Counting and the ontogenetic origins of exact equality. ")%>%
  filter(!is.na(Task_item))%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

## .....output KL data to data-raw -----
Schneider_Barner_UnderReview_KL <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_UnderReview.csv'))%>%
  mutate(method = 'titrated', 
         Experiment = 'Schneider_Barner_UnderReview',
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R.M., Brockbank, E., Feiman, R., & Barner, D. (under review). Counting and the ontogenetic origins of exact equality. ")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(Schneider_Barner_UnderReview_KL, here::here('data/data-raw/kl-only/data-raw/Schneider_Barner_UnderReview.csv'))



##small sf (Schneider, Pankonin, Schachner, Barner, 2021)
Schneider_Barner_2021 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_2021.csv'))%>%
  filter(!is.na(Task_item))%>%
  mutate(method = 'titrated', 
         Experiment = 'Schneider_Barner_2021',
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R. M., Pankonin, A., Schachner, A., & Barner, D. (2021). Starting small: Exploring the origins of successor function knowledge. Developmental Science, 24(4), e13091.")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level, -Correct)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##output KL data to data-raw 
<<<<<<< HEAD
Schneider_Barner_2021_KL <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_2021.csv'))%>%
mutate(method = 'titrated', 
       Experiment = 'Schneider_Barner_2021',
       Age = Age*12, 
       lab = "Barner", 
       cite = "Schneider, R. M., Pankonin, A., Schachner, A., & Barner, D. (2021). Starting small: Exploring the origins of successor function knowledge. Developmental Science, 24(4), e13091.")%>%
=======
schneider_etal_20xx_kl <- read_csv(here::here('data/data-raw/trial-level/data-raw/schneider_etal_20xx.csv'))%>%
  mutate(method = 'titrated', 
        Experiment = 'SchneiderEtAl_20xx',
        Age = Age*12, 
        lab = "Barner", 
        cite = "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.")%>%
>>>>>>> bb5d309201a1cb370f77f1e2bf1c547a785e961a
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(Schneider_Barner_2021_KL, here::here('data/data-raw/kl-only/data-raw/Schneider_Barner_2021.csv'))


##conservation
Schneider_Barner_Unpublished <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_Unpublished.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         Experiment = 'Schneider_Barner_Unpublished', 
         Age = Age*12, 
         Task_item = as.integer(Task_item), 
         Response = as.integer(Response), 
         lab = "Barner", 
         cite = "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).")%>%
  dplyr::select(-Task, -Trial_number, -Knower_level)%>%
  rename("Query" = "Task_item", 
         "Subject" = "SID")

##write kls
Schneider_Barner_Unpublished_KL <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_Unpublished.csv'))%>%
  filter(Task_item != "Enter Number!")%>%
  mutate(method = 'titrated', 
         Experiment = 'Schneider_Barner_Unpublished', 
         Age = Age*12, 
         lab = "Barner", 
         cite = "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).")%>%
  rename("Subject" = "SID")%>%
  distinct(Experiment, lab, Subject, Age, Sex, Language, Knower_level, method, cite)

write_csv(Schneider_Barner_Unpublished_KL, here::here('data/data-raw/kl-only/data-raw/Schneider_Barner_Unpublished.csv'))

## xculture
Schneider_Barner_2020 <- read.csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_2020.csv'))%>%
  filter(Age != "#VALUE!", 
         Query != "TireHydrant",
         Query != "DuckClover", 
         Query != "CloverKite", 
         !is.na(Query), 
         !is.na(Response))%>%
  mutate(Age = 12*as.numeric(as.character(Age)), 
         Query = as.numeric(as.character(Query)), 
         method = "Non-titrated", 
         cite = "Schneider, R. M., Sullivan, J., Marusic, F., Biswas, P., Mismas, P., Plesnicar, V., & Barner, D. (2020). Do children use language structure to discover the recursive rules of counting?. Cognitive psychology, 117, 101263.", 
         lab = "Barner")

#calculate KL - this is either subset or CP
Schneider_Barner_2020_KL <- Schneider_Barner_2020 %>%
  mutate(Correct = ifelse(Query == Response, 1, 0))%>%
  group_by(Subject)%>%
  summarise(n_correct = sum(Correct))%>%
  mutate(Knower_level = ifelse(n_correct >= 4, "CP", "Subset"))%>%
  select(-n_correct)

## get demo information for KL
Schneider_Barner_2020_KL <- Schneider_Barner_2020 %>%
  left_join(Schneider_Barner_2020_KL)%>%
  distinct(Subject, Experiment, Age, Knower_level, method, Language, Sex, cite, lab)

## write to KL 
write.csv(Schneider_Barner_2020_KL, here::here("data/data-raw/kl-only/data-raw/Schneider_Barner_2020.csv"))

## ....Marchand and Barner ====
marchandBarner_titrated_1 <- read.csv(here::here("data/data-raw/trial-level/data-raw/marchand_barner_titrated.csv"))%>%
  dplyr::select(-GNtitrated_method, -X2.GNtitrated_request, -X2.GNtitrated_answer, -X2.GNtitrated_KL, -Age.Yr)%>%
  filter(!is.na(X1.GNtitrated_KL))%>%
  rename("Age" = "Age.Mo", 
         "method" = "Give.N.Method", 
         "Query" = "X1.GNtitrated_request", 
         "Response" = "X1.GNtitrated_answer", 
         "Knower_level" = "X1.GNtitrated_KL", 
         "Subject" = "SubjID", 
         "Sex" = "Gender")%>%
  mutate(Language = "English", 
         Knower_level = str_remove(Knower_level,  "k|K"), 
         Experiment = "Marchand_Barner_2020_Titrated_1", 
         cite = "Marchand, E., & Barner, D. (2021). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.", 
         lab = "Barner")
View(marchandBarner_titrated_1)
##write KLs
marchandBarner_titrated_1_kl <- marchandBarner_titrated_1 %>%
  distinct(Subject, Experiment, Age, Knower_level, method, Language, Sex, cite, lab)
write.csv(marchandBarner_titrated_1_kl, here::here("data/data-raw/kl-only/data-raw/marchand_barner_2020_titrated.csv"))

#remove KLs from the above
marchandBarner_titrated_1 <- marchandBarner_titrated_1 %>%
  select(-Knower_level)

### Second titration
marchandBarner_titrated_2 <- read.csv(here::here("data/data-raw/trial-level/data-raw/marchand_barner_titrated.csv"))%>%
  dplyr::select(-GNtitrated_method, -X1.GNtitrated_request, -X1.GNtitrated_answer, -X1.GNtitrated_KL, -Age.Yr)%>%
  filter(!is.na(X2.GNtitrated_KL))%>%
  rename("Age" = "Age.Mo", 
         "method" = "Give.N.Method", 
         "Query" = "X2.GNtitrated_request", 
         "Response" = "X2.GNtitrated_answer", 
         "Knower_level" = "X2.GNtitrated_KL", 
         "Subject" = "SubjID", 
         "Sex" = "Gender")%>%
  mutate(Language = "English", 
         Knower_level = str_remove(Knower_level,  "k|K"), 
         Experiment = "Marchand_Barner_2020_Titrated_2", 
         cite = "Marchand, E., & Barner, D. (2021). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.", 
         lab = "Barner")

##write KLs
marchandBarner_titrated_2_kl <- marchandBarner_titrated_2 %>%
  distinct(Subject, Experiment, Age, Knower_level, method, Language, Sex, cite, lab)
write.csv(marchandBarner_titrated_2_kl, here::here("data/data-raw/kl-only/data-raw/marchand_barner_2020_titrated_2.csv"))

#remove KLs from the above
marchandBarner_titrated_2 <- marchandBarner_titrated_2 %>%
  select(-Knower_level)

## marchand barner non-titrated- 1
marchand_barner_non_titrated_1 <- read.csv(here::here("data/data-raw/trial-level/data-raw/marchand_barner_non-titrated.csv"))%>%
  dplyr::select(-X2.GNntitrated_request, -X2.GNntitrated_answer, -X2.GNntitrated_KL, -Age.Yr)%>%
  filter(!is.na(X1.GNntitrated_KL))%>%
  rename("Age" = "Age.Mo", 
         "Query" = "X1.GNntitrated_request", 
         "Response" = "X1.GNntitrated_answer", 
         "Knower_level" = "X1.GNntitrated_KL", 
         "Subject" = "SubjID", 
         "Sex" = "Gender", 
         "method" = "Give.N.Method")%>%
  mutate(Language = "English", 
         Knower_level = stringr::str_remove(Knower_level,  "k|K"), 
         Experiment = "Marchand_Barner_2020_nonTitrated_1", 
         cite = "Marchand, E., & Barner, D. (2021). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.", 
         lab = "Barner", 
         method = "non-titrated")

##write KLs
marchandBarner_non_titrated_1_kl <- marchand_barner_non_titrated_1 %>%
  distinct(Subject, Experiment, Age, Knower_level, method, Language, Sex, cite, lab)
write.csv(marchandBarner_non_titrated_1_kl, here::here("data/data-raw/kl-only/data-raw/marchand_barner_2020_non_titrated_1.csv"))

#remove KLs from the above
marchand_barner_non_titrated_1 <- marchand_barner_non_titrated_1 %>%
  select(-Knower_level)

## marchand barner non-titrated - 2
marchand_barner_non_titrated_2 <- read.csv(here::here("data/data-raw/trial-level/data-raw/marchand_barner_non-titrated.csv"))%>%
  dplyr::select(-X1.GNntitrated_request, -X1.GNntitrated_answer, -X1.GNntitrated_KL, -Age.Yr)%>%
  filter(!is.na(X2.GNntitrated_KL))%>%
  rename("Age" = "Age.Mo", 
         "Query" = "X2.GNntitrated_request", 
         "Response" = "X2.GNntitrated_answer", 
         "Knower_level" = "X2.GNntitrated_KL", 
         "Subject" = "SubjID", 
         "Sex" = "Gender", 
         "method" = "Give.N.Method")%>%
  mutate(Language = "English", 
         Knower_level = stringr::str_remove(Knower_level,"k|K"), 
         Experiment = "Marchand_Barner_2020_nonTitrated_2", 
         cite = "Marchand, E., & Barner, D. (2021). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.", 
         lab = "Barner", 
         method = "non-titrated")

##write KLs
marchandBarner_non_titrated_2_kl <- marchand_barner_non_titrated_2 %>%
  distinct(Subject, Experiment, Age, Knower_level, method, Language, Sex, cite, lab)
write.csv(marchandBarner_non_titrated_2_kl, here::here("data/data-raw/kl-only/data-raw/marchand_barner_2020_non_titrated_2.csv"))

#remove KLs from the above
marchand_barner_non_titrated_2 <- marchand_barner_non_titrated_2 %>%
  select(-Knower_level)


### Marchand barner mixed
# file equivalent to marchand_barner_mixed_REDUNDANT.csv
marchandUndRev <- read_csv(here::here("data/data-raw/trial-level/data-raw/Marchand2019_Data_104c_Reliability.csv"))

marchandUndRev %<>%
  filter(is.na(Exclusion_Reason) || Exclusion_Reason == "5k") %>%
  rename(Subject = SubjID,
         Age = Age_Mo,
         Sex = Gender,
         Language = L1) %>%
  select(Subject, Age, Sex, Language, Titrated_request, Nontitrated_request, Titrated_answer, Nontitrated_answer, Titrated_KL, Nontitrated_KL) %>%
  gather(method, "Query", ends_with("request")) %>%
  gather(method2, "Response", ends_with("answer")) %>%
  gather(method3, "KL", ends_with("KL")) %>%
  filter(str_extract(method, "[^_]+") == str_extract(method2, "[^_]+") & 
           str_extract(method2, "[^_]+") == str_extract(method3, "[^_]+")) %>%
  select(-method2, -method3) %>%
  mutate_at(c("Query", "Response"), as.numeric) %>%
  mutate(method = tolower(str_remove(method, "_request")),
         Experiment = "MarchandUnderRev",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2021). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.") %>%
  arrange(Subject, method, Query, Response)

marchandUndRev_kl <- marchandUndRev %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab) %>%
  mutate(KL = str_remove(KL, "k"))
write.csv(marchandUndRev_kl, here::here("data/data-raw/kl-only/data-raw/marchandUndRev.csv"))

marchandUndRev <- select(marchandUndRev, -KL)


marchand2019 <- read_csv(here::here("data/data-raw/trial-level/data-raw/Marchand2018_103Data_FrenchUn.csv"))
marchand2019 %<>%
  filter(Exclusion == "no" || Comment == "0K") %>%
  rename(Subject = SubID,
         Age = Age.Mo,
         Sex = Gender,
         Language = L1,
         Query = GN_request,
         Response = GN_ans,
         KL = GN_KL) %>%
  select(Subject, Age, Sex, Language, Query, Response, KL) %>%
  mutate(Response = ifelse(Response == "tous", "10", Response),
         Response = as.numeric(Response),
         Experiment = "Marchand2019",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2019). The acquisition of French Un. 41st Meeting of the Annual Cognitive Sciences Society.",
         method = "titrated")
marchand2019_kl <- marchand2019 %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab) %>%
  mutate(KL = str_remove(KL, "K"))
write.csv(marchand2019_kl, here::here("data/data-raw/kl-only/data-raw/marchand2019.csv"))

marchand2019 <- select(marchand2019, -KL)




### Gunderson_unpublished
gunderson_unpub <- read_sav(here::here("data/data-raw/trial-level/data-raw/Gunderson2015_unpublished.sav"))

gunderson_unpub %<>%
  select(1:31) %>%
  pivot_longer(cols = 2:31, 
               names_to = "Query", 
               values_to = "Response") %>%
  rename(Subject = T1GNSubjectID) %>%
  mutate(Query = substr(Query, 6, 6), # extract query which is after "T1GNG"
         Experiment = "GundersonUnpub",
         lab = "Gunderson",
         cite = "Gunderson, E. A. (2015). Unpublished data.",
         method = "titrated") 
  
  



# Bind everything together ----
all.data <- bind_rows(almoammer2013_english, 
                      wagner2016, 
                      piantadosi2014, 
                      sarnecka2007, 
                      sarnecka2019, 
                      krajcsi2018, 
                      boni20xx_ordered, 
                      boni20xx_random, 
                      Schneider_Barner_UnderReview,
                      Schneider_Barner_2021, 
                      Schneider_Barner_Unpublished, 
                      Schneider_Barner_2020, 
                      marchandBarner_titrated_1, 
                      marchandBarner_titrated_2, 
                      marchand_barner_non_titrated_1, 
                      marchand_barner_non_titrated_2,
                      marchandUndRev,
                      marchand2019)%>%
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


# all.data <- read_csv('data/processed-data/trial_level_processed_data.csv')
glimpse(all.data)
