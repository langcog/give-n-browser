## Code for processing and cleaning knower-level only data with general template: 
#Experiment, Language, SID, KL, Age_months, method, cite, Age_years 

#load libraries
rm(list = ls())
library(tidyverse)
library(tidylog)
library(magrittr)
files = list.files(path = here::here("data/data-raw/kl-only/data-raw"),
                   #pattern = "*.csv", 
                   full.names = T)
files <- files[!str_detect(files,"Chernyak")]


# Read in all data from kl-only raw data ----
##TO-DO: add column for sex
data.raw <- files %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Wrangling and cleaning ----

## Chernyak_Cordes_Submitted
Chernyak_Cordes_Submitted_Study1 <- read_csv(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_Submitted_Study1.csv")) %>%
  mutate(`Subject ID` = paste0("1_",`Subject ID`))
Chernyak_Cordes_Submitted_Study2 <- read_csv(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_Submitted_Study2.csv")) %>%
  mutate(`Subject ID` = paste0("2_",`Subject ID`))
Chernyak_Cordes_Submitted <- Chernyak_Cordes_Submitted_Study1 %>%
  bind_rows(Chernyak_Cordes_Submitted_Study2) %>%
  rename(Subject = `Subject ID`,
         Sex = Gender,
         KL = give_n_score,
         Age_years = Age) %>%
  mutate(Age_months = Age_years * 12, # age is in years
         KL = as.character(KL),
         KL = ifelse(KL == "6", "CP", KL),
         Language = "English",
         method="titrated",
         Experiment = "Chernyak_Cordes_Submitted",
         lab = "Cordes",
         cite = "Chernyak, N., Harris, P., & Cordes, S. (submitted). Numerical cognition, not cognitive control, causally and uniquely explains equal sharing.",
         typical = "typical") %>%
  select(Experiment, Language, Subject, KL, Age_months, Age_years, Sex, method, cite, lab, typical)



## Chernyak_Cordes_2019
Chernyak_Cordes_2019_Study1 <- read_sav(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_2019_Study1.sav")) %>%
  mutate(SubjectID = paste0("1_",SubjectID))
Chernyak_Cordes_2019_Study2 <- read_sav(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_2019_Study2.sav")) %>%
  mutate(SubjectID = paste0("2_",SubjectID))
Chernyak_Cordes_2019 <- Chernyak_Cordes_2019_Study1 %>%
  bind_rows(Chernyak_Cordes_2019_Study2) %>%
  rename(Subject = SubjectID,
         Sex = Gender,
         KL = GiveN,
         Age_years = Age) %>%
  mutate(Age_months = Age_years * 12, # age is in years
         KL = as.character(KL),
         KL = ifelse(KL == "6", "CP", KL),
         Language = "English",
         method="titrated",
         Experiment = "Chernyak_Cordes_2019",
         lab = "Cordes",
         cite = "Chernyak, N., Harris, P., & Cordes, S. (2019). Explaining early moral hypocrisy: Numerical cognition promotes equal sharing behavior in preschool-aged children. Developmental Science, 22(1), e12695.",
         typical = "typical") %>%
  select(Experiment, Language, Subject, KL, Age_months, Age_years, Sex, method, cite, lab, typical)




## Chernyak_Cordes_2020 -- not included yet
read_csv(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_2020.csv")) %>%
  filter(!is.na(GiveNCorrectTrials)) %>%
  mutate(highestCorrIndex = sapply(HighestNumberCorrect, function(x){ which(c(0,1,3,6,8,10) %in% x) }) - 1,
         KL = ifelse(GiveNCorrectTrials == highestCorrIndex, HighestNumberCorrect, NA)) %>%
  count(HighestNumberCorrect, highestCorrIndex, GiveNCorrectTrials, KL)

Chernyak_Cordes_2020 <- read_csv(here::here("data/data-raw/kl-only/data-raw/Chernyak_Cordes_2020.csv")) %>%
  filter(!is.na(GiveNCorrectTrials))
  rename(Subject = `Subject ID`,
         Sex = Gender,
         KL = GiveNCorrectTrials) %>% #number of trials out of 5 got correct
  mutate(Age = Age * 12, # age is in years
         Language = "English",
         method="non-titrated",
         Experiment = "Chernyak_Cordes_2020",
         lab = "Cordes",
         cite = "Chernyak, N., Turnbull, V., Gordon, R., Harris, P. & Cordes, S. (2020). Counting promotes proportional social evaluation in preschool-aged children. Cognitive Development, 56, 100969.",
         typical = "non-typical") %>%
  select(Experiment, lab, Language, Subject, KL, Age, Sex, method, cite, typical)








#mostly renaming
data.raw %<>%
  unite(Subject, c(Subject, Participant), na.rm=TRUE) %>%
  unite(KL, c(KL, Knower.Level, Knower_level, `Knower-level`), na.rm=TRUE) %>% #collapse rows onto KL
  mutate(KL = ifelse(KL == "", NA, KL),
         KL = str_remove(KL, "K"),
         KL = ifelse(KL == "Non", 0, KL),
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual"))) %>%
  filter(KL != "X")



#select relevant columns, round for age and mutate years

data.raw %<>%
  mutate(Age_months = round(as.numeric(as.character(Age), 4)),
         Age_years = floor(Age_months)/12) %>%
  bind_rows(Chernyak_Cordes_Submitted, Chernyak_Cordes_2019) %>%
  mutate(cite = case_when(
    Experiment == "Almoammer_Barner_2013" ~ "Almoammer, A., Sullivan, J., Donlan, C., Marusic, F., O'Donnell, T., & Barner, D. (2013). Grammatical morphology as a source of early number word meanings. Proceedings of the National Academy of Sciences, 110(46), 18448-18453.", 
    Experiment == "Barner_Yang_2009" | Experiment == "BarnerChowYoungE2" ~ "Barner, D., Chow, K., & Yang, S. J. (2009). Finding one's meaning: A test of the relation between quantifiers and integers in language development. Cognitive Psychology, 58(2), 195-219.", 
    Experiment == "Barner_Takasaki_2009" ~ "Barner, D., Libenson, A., Cheung, P., & Takasaki, M. (2009). Cross-linguistic relations between quantifiers and numerals in language acquisition: Evidence from Japanese. Journal of Experimental Child Psychology, 103(4), 421-440.", 
    Experiment == "Marusic_Barner_2016" ~ "Marusic, F., Zaucer, R., Plesnicar, V., Razborsek, T., Sullivan, J., & Barner, D. (2016). Does grammatical structure accelerate number word learning? Evidence from learners of dual and non-dual dialects of Slovenian. PLoS one, 11(8), e0159208.", 
    Experiment == "Piantadosi_Gibson_2014" ~ "Piantadosi, S. T., Jara‐Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming‐foraging group. Developmental Science, 17(4), 553-563.", 
    Experiment == "Schneider_Barner_UnderReview" ~ "Schneider, R.M., Brockbank, E., Feiman, R., & Barner, D. (2022). Counting and the ontogenetic origins of exact equality. Cognition, 218, 104952.",
    Experiment == "Schneider_Barner_2021" ~ "Schneider, R. M., Pankonin, A., Schachner, A., & Barner, D. (2021). Starting small: Exploring the origins of successor function knowledge. Developmental Science, 24(4), e13091.",
    Experiment == "Schneider_Barner_2020" ~ "Schneider, R. M., Sullivan, J., Marusic, F., Biswas, P., Mismas, P., Plesnicar, V., & Barner, D. (2020). Do children use language structure to discover the recursive rules of counting? Cognitive Psychology, 117, 101263.",
    Experiment == "Schneider_Barner_Unpublished" ~ "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).", 
    Experiment == "Marchand2019" ~ "Marchand, E., & Barner, D. (2019). The acquisition of French Un. 41st Meeting of the Annual Cognitive Sciences Society (pp. 756-762).", 
    Experiment == "MarchandUnderRev" ~ "Marchand, E., Lovelett, J. T., Kendro, K., & Barner, D. (2022). Assessing the knower-level framework: How reliable is the Give-a-Number task? Cognition, 222, 104998.",
    TRUE ~ cite #catch the rest
  ),
  typical = ifelse(Experiment == "Schneider_Barner_2020", "non-typical", "typical")) %>%
  select(Experiment, lab, Language, Subject, Sex, KL, Age_months, Age_years, method, cite, typical)

data.raw %>%
  distinct(cite)

# Save and export ----
write_csv(data.raw, here::here("data/processed-data/kl_data_processed.csv")) #to main data
write_csv(data.raw, here::here("shiny_app/data/processed-data/kl_data_processed.csv")) #to main data
