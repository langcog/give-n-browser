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
Almoammer_Barner_2013 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Almoammer_Barner_2013.csv'), name_repair = "minimal")%>%
  pivot_longer(cols = c(-Experiment, - Participant, - Language,
                        -PrimaryLang, -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  mutate(lab = "Barner", 
         cite = "Almoammer, A., Sullivan, J., Donlan, C., Marusic, F., O’Donnell, T., & Barner, D. (2013). Grammatical morphology as a source of early number word meanings. Proceedings of the National Academy of Sciences, 110(46), 18448-18453.")%>%
  rename("Subject" = "Participant", 
         "Age" = "Age_months", 
         "Sex" = "SEX") %>%
  dplyr::mutate(Query = as.integer(Query), 
                Response = as.integer(Response))


# ...piantadosi2014 ----
Piantadosi_Gibson_2014 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Piantadosi_Gibson_2014.csv'))%>%
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
Piantadosi_Gibson_2014_KL <- read_csv(here::here('data/data-raw/trial-level/data-raw/Piantadosi_Gibson_2014.csv'))%>%
  distinct(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  dplyr::select(Experiment, Participant, Language, `Knower-level`, Sex, Age, method, cite)%>%
  rename("Subject" = "Participant")%>%
  mutate(Age = Age*12, 
         Subject = as.character(Subject), 
         lab = "Piantadosi", 
         cite = "Piantadosi, S. T., Jara_Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming_foraging group. Developmental Science, 17(4), 553-563.")

write_csv(Piantadosi_Gibson_2014_KL, here::here('data/data-raw/kl-only/data-raw/Piantadosi_Gibson_2014.csv'))

# ...sarnecka2007 ----
Sarnecka_Yudovina_2007 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Sarnecka_Yudovina_2007.csv'), name_repair = "minimal")%>%
  pivot_longer(cols = c(-Experiment, -Participant, -Language,
                        -Age_months, -SEX, -method, -cite), 
               names_to = "Query", 
               values_to = "Response")%>%
  dplyr::select(Experiment, Participant, Language, Age_months, method, cite, SEX, Query, Response)%>%
  rename("Subject" = "Participant", 
                "Sex" = "SEX", 
                "Age" = "Age_months") %>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Sarnecka", 
         cite = "Sarnecka, B. W., Kamenskaya, V. G., Yamana, Y., Ogura, T., & Yudovina, Y. B. (2007). From grammatical number to exact numbers: Early meanings of ‘one’,‘two’, and ‘three’in English, Russian, and Japanese. Cognitive Psychology, 55(2), 136-168.")

## ... wagner2019 ----
Wagner_Barner_2019 <- read.csv(here::here('data/data-raw/trial-level/data-raw/Wagner_Barner_2019.csv'))%>%
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
         cite = "Wagner, K., Chu, J., & Barner, D. (2019). Do children's number words begin noisy?. Developmental science, 22(1), e12752.")

## ...boni (2022, ordered Give) ----
### Reading these in separately and then combining them
Boni_Piantadosi_2022_ordered <- read_csv(here::here('data/data-raw/trial-level/data-raw/Boni_Piantadosi_2022.csv')) %>%
  dplyr::select(-starts_with("GiveNRandom"))%>%
  mutate(Experiment = "Boni_Piantadosi_2022_ordered", 
         Language = "Tsimane")%>%
  pivot_longer(cols = c(-Experiment, -Subject, 
                        -Language, -`Location Code`, 
                        -Education, -Age, -Gender, -method, -cite), 
               names_to = "QueryOrder", 
               values_to = "Response")%>%
  dplyr::select(-`Location Code`, -Education, -QueryOrder)%>%
  mutate(Age = 12*Age, 
         Query = str_extract(Response, "[^_]"),
         Response = str_remove(str_extract(Response,"\\d_*$"), "_"),
         Subject = as.character(Subject), 
         lab= "Piantadosi", 
         cite = "Boni, I., Jara-Ettinger, J., Sackstein, S., & Piantadosi (2022). Verbal counting and the timing of number acquisition in an indigenous Amazonian group. PLoS One, 17(8), e0270739.")%>%
  dplyr::rename("Sex" = "Gender")

## ...boni (2022, random Give) ----
Boni_Piantadosi_2022_random <- read_csv(here::here('data/data-raw/trial-level/data-raw/Boni_Piantadosi_2022.csv')) %>%
  dplyr::select(-starts_with("OrderedGiveN"))%>%
  mutate(Experiment = "Boni_Piantadosi_2022_random", 
         Language = "Tsimane")%>%
  pivot_longer(cols = c(-Experiment, -Subject, 
                        -Language, -`Location Code`, 
                        -Education, -Age, -Gender, -method, -cite), 
               names_to = "QueryOrder", 
               values_to = "Response")%>%
  dplyr::select(-`Location Code`, -Education, -QueryOrder)%>%
  mutate(Age = 12*Age, 
         Query = str_extract(Response, "[^_]"),
         Response = str_remove(str_extract(Response,"\\d_*$"), "_"),
         Subject = as.character(Subject), 
         lab= "Piantadosi", 
         cite = "Boni, I., Jara-Ettinger, J., Sackstein, S., & Piantadosi (2022). Verbal counting and the timing of number acquisition in an indigenous Amazonian group. PLoS One, 17(8), e0270739.")%>%
  dplyr::rename("Sex" = "Gender")

## Bind these together
Boni_Piantadosi_2022 <- bind_rows(Boni_Piantadosi_2022_ordered, Boni_Piantadosi_2022_random)

## now read in the more typical conventions 
## ...krajcsi2018 ----
Krajcsi_2018 <- read_csv(here::here("data/data-raw/trial-level/data-raw/Krajcsi_2018.csv"))%>%
  mutate(Query = as.integer(Query), 
         Response = as.integer(Response))%>%
  rename("Subject" = "Participant")%>%
  mutate(Subject = as.character(Subject), 
         lab = "Krajcsi", 
         cite = "Krajcsi, A. (2019, March 19). Follow-up questions influence the measured number knowledge in the Give a number task. https://doi.org/10.31234/osf.io/fky69")

## ...sarnecka2019 ----
Sarnecka_Negen_2019 <- read_csv(here::here('data/data-raw/trial-level/data-raw/Sarnecka_Negen_2019.csv'))%>%
  dplyr::rename("Sex" = "Gender", 
                "Query" = "Test",
                "Subject" = "SubjNo")%>%
  mutate(Language = "English", 
         Subject = as.character(Subject), 
         Query = as.integer(Query), 
         Response = as.integer(Response), 
         lab = "Sarnecka", 
         Experiment = "Sarnecka_Negen_2019", 
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
Schneider_Barner_2021_KL <- read_csv(here::here('data/data-raw/trial-level/data-raw/Schneider_Barner_2021.csv'))%>%
mutate(method = 'titrated', 
       Experiment = 'Schneider_Barner_2021',
       Age = Age*12, 
       lab = "Barner", 
       cite = "Schneider, R. M., Pankonin, A., Schachner, A., & Barner, D. (2021). Starting small: Exploring the origins of successor function knowledge. Developmental Science, 24(4), e13091.")%>%
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
## marchand barner titrated
Marchand_Barner_2020_Titrated <- read.csv(here::here("data/data-raw/trial-level/data-raw/Marchand_Barner_2020_Titrated.csv")) %>%
  gather(turn, "Query", ends_with("request")) %>%
  gather(turn2, "Response", ends_with("answer")) %>%
  gather(turn3, "KL", ends_with("KL"))  %>%
  filter(str_extract(turn, "[^_]+") == str_extract(turn2, "[^_]+") & 
         str_extract(turn2, "[^_]+") == str_extract(turn3, "[^_]+")) %>%
  mutate_at(c("Query", "Response"), as.numeric) %>%
  rename(Age = Age.Mo,
         Subject = SubjID, 
         Sex = Gender,
         method = Give.N.Method) %>%
  mutate(Subject = as.factor(paste0(as.character(Subject), "_", substr(method,2,2))), # each subject counted twice
         KL = str_remove(KL, "k"),
         Language = "English",
         Experiment = "Marchand_Barner_2020_Titrated",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2020). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.") %>%
  select(Experiment, Subject, Language, Age, method, cite, Sex, Query, Response, lab, KL)

##write KLs
Marchand_Barner_2020_Titrated_kl <- Marchand_Barner_2020_Titrated %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab)
write.csv(Marchand_Barner_2020_Titrated_kl, here::here("data/data-raw/kl-only/data-raw/Marchand_Barner_2020_Titrated.csv"))

#remove KLs from the above
Marchand_Barner_2020_Titrated <- Marchand_Barner_2020_Titrated %>%
  select(-KL)

## marchand barner non-titrated
Marchand_Barner_2020_NonTitrated <- read.csv(here::here("data/data-raw/trial-level/data-raw/Marchand_Barner_2020_NonTitrated.csv")) %>%
  gather(turn, "Query", ends_with("request")) %>%
  gather(turn2, "Response", ends_with("answer")) %>%
  gather(turn3, "KL", ends_with("KL"))  %>%
  filter(str_extract(turn, "[^_]+") == str_extract(turn2, "[^_]+") & 
           str_extract(turn2, "[^_]+") == str_extract(turn3, "[^_]+")) %>%
  mutate_at(c("Query", "Response"), as.numeric) %>%
  rename(Age = Age.Mo,
         Subject = SubjID, 
         Sex = Gender) %>%
  mutate(Subject = as.factor(paste0(as.character(Subject), "_", substr(turn,2,2))), # each subject counted twice
         method = "non-titrated",
         KL = str_remove(KL, "k"),
         Language = "English",
         Experiment = "Marchand_Barner_2020_NonTitrated",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2020). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.") %>%
  select(Experiment, Subject, Language, Age, method, cite, Sex, Query, Response, lab, KL)

##write KLs
Marchand_Barner_2020_NonTitrated_kl <- Marchand_Barner_2020_NonTitrated %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab)
write.csv(Marchand_Barner_2020_NonTitrated_kl, here::here("data/data-raw/kl-only/data-raw/Marchand_Barner_2020_NonTitrated.csv"))

#remove KLs from the above
Marchand_Barner_2020_NonTitrated <- Marchand_Barner_2020_NonTitrated %>%
  select(-KL)


### Marchand barner mixed
# file equivalent to marchand_barner_mixed_REDUNDANT.csv
Marchand_Barner_2020_Mixed <- read_csv(here::here("data/data-raw/trial-level/data-raw/Marchand_Barner_2020_Mixed.csv")) %>%
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
         Experiment = "Marchand_Barner_2020_Mixed",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2020). How Reliable is the Give-a-Number task?. 42nd Meeting of the Annual Cognitive Sciences Society.") %>%
  select(Experiment, Subject, Language, Age, method, cite, Sex, Query, Response, lab, KL)

Marchand_Barner_2020_Mixed_kl <- Marchand_Barner_2020_Mixed %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab) %>%
  mutate(KL = str_remove(KL, "k"))
write.csv(Marchand_Barner_2020_Mixed_kl, here::here("data/data-raw/kl-only/data-raw/Marchand_Barner_2020_Mixed.csv"))

Marchand_Barner_2020_Mixed <- select(Marchand_Barner_2020_Mixed, -KL)


Marchand_Barner_2019 <- read_csv(here::here("data/data-raw/trial-level/data-raw/Marchand_Barner_2019.csv")) %>%
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
         Experiment = "Marchand_Barner_2019",
         lab = "Barner",
         cite = "Marchand, E., & Barner, D. (2019). The acquisition of French Un. 41st Meeting of the Annual Cognitive Sciences Society.",
         method = "titrated") %>%
  select(Experiment, Subject, Language, Age, method, cite, Sex, Query, Response, lab, KL)

Marchand_Barner_2019_kl <- Marchand_Barner_2019 %>%
  distinct(Subject, Experiment, Age, KL, method, Language, Sex, cite, lab) %>%
  mutate(KL = str_remove(KL, "K"))
write.csv(Marchand_Barner_2019_kl, here::here("data/data-raw/kl-only/data-raw/Marchand_Barner_2019.csv"))

Marchand_Barner_2019 <- select(Marchand_Barner_2019, -KL)




### Gunderson_unpublished
Gunderson_Unpublished <- read_sav(here::here("data/data-raw/trial-level/data-raw/Gunderson_Unpublished.sav")) %>%
  select(1:31) %>%
  pivot_longer(cols = 2:31, 
               names_to = "Query", 
               values_to = "Response") %>%
  rename(Subject = T1GNSubjectID) %>%
  mutate(Query = substr(Query, 6, 6), # extract query which is after "T1GNG"
         Experiment = "Gunderson_Unpublished",
         lab = "Gunderson",
         cite = "Gunderson, E. A. (Unpublished).",
         method = "titrated") 
glimpse(Gunderson_Unpublished)
  



# Bind everything together ----
all.data <- bind_rows(Almoammer_Barner_2013, 
                      Wagner_Barner_2019, 
                      Piantadosi_Gibson_2014, 
                      Sarnecka_Yudovina_2007, 
                      Sarnecka_Negen_2019, 
                      Krajcsi_2018, 
                      Boni_Unpublished,
                      Schneider_Barner_UnderReview,
                      Schneider_Barner_2021, 
                      Schneider_Barner_Unpublished, 
                      Schneider_Barner_2020, 
                      Marchand_Barner_2020_Titrated, 
                      Marchand_Barner_2020_NonTitrated,
                      Marchand_Barner_2020_Mixed,
                      Marchand_Barner_2019) %>%
  filter(!is.na(Response), ##get rid of NAs
         !is.na(Age),
         Response >= 1)%>% ##get rid of 0s 
  mutate(Age_months = round(as.numeric(as.character(Age), 4)), 
         Age_years = floor(Age_months)/12)%>%
  mutate(typical = ifelse(Experiment == "Schneider_Barner_2020", "non-typical", "typical"))%>%
  select(Experiment, lab, Language, Subject, Age_months, Age_years, Sex, method, Query, Response, cite, typical)

# Save and export ----
write_csv(all.data, here::here('data/processed-data/trial_level_processed_data.csv')) # to main data
write_csv(all.data, here::here('shiny_app/data/processed-data/trial_level_processed_data.csv')) # to shiny app


