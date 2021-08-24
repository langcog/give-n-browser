## Code for processing and cleaning knower-level only data with general template: 
#Experiment, Language, SID, KL, Age_months, method, cite, Age_years 

#load libraries
rm(list = ls())
library(tidyverse)
library(tidylog)
library(magrittr)
files = list.files(path = "data/data-raw/kl-only/data-raw",
                   pattern = "*.csv", 
                   full.names = T)
files <- c(files[1:3], files[6], files[13:20])

# Read in all data from kl-only raw data ----
##TO-DO: add column for sex
data.raw <-
  files %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
# Wrangling and cleaning ----

chernyak2020 <- read_csv("data/data-raw/kl-only/data-raw/Chernyak2020.csv")
chernyak2020 %<>%
  rename(Subject = `Subject ID`,
         Sex = Gender,
         KL = GiveNCorrectTrials,
         Age_years = Age) %>%
  mutate(Age_months = Age_years * 12,
         Language = "English",
         method="non-titrated",
         Experiment = "Chernyak2020",
         lab = "Cordes",
         cite = "Chernyak, N., Turnbull, V., Gordon, R., Harris, P. & Cordes, S. (2020). Counting promotes proportional social evaluation in preschool-aged children. Cognitive Development, 56.",
         typical = "non-typical") %>%
  select(Experiment, lab, Language, Subject, KL, Age_months, Age_years, method, cite)
glimpse(chernyak2020)


chernyak_submitted1 <- read_csv("data/data-raw/kl-only/data-raw/Chernyak_submitted_Study1.csv") %>%
  mutate(`Subject ID` = paste0("1_",`Subject ID`))
chernyak_submitted2 <- read_csv("data/data-raw/kl-only/data-raw/Chernyak_submitted_Study2.csv") %>%
  mutate(`Subject ID` = paste0("2_",`Subject ID`))
chernyak_submitted <- chernyak_submitted1 %>%
  bind_rows(chernyak_submitted2) %>%
  rename(Subject = `Subject ID`,
         Sex = Gender,
         KL = give_n_score,
         Age_years = Age) %>%
  mutate(Age_months = Age_years * 12,
         KL = as.character(KL),
         KL = ifelse(KL == "6", "CP", KL),
         Language = "English",
         method="titrated",
         Experiment = "Chernyak_submitted",
         lab = "Cordes",
         cite = "Chernyak, N., Harris, P., & Cordes, S. (submitted). Numerical cognition, not cognitive control, causally and uniquely  explains equal sharing.",
         typical = "typical") %>%
  select(Experiment, lab, Language, Subject, KL, Age_months, Age_years, method, cite)
glimpse(chernyak_submitted)




chernyak2019_1 <- read_sav("data/data-raw/kl-only/data-raw/ChernyakHarrisCordes2019_Study1.sav") %>%
  mutate(SubjectID = paste0("1_",SubjectID))
chernyak2019_2 <- read_sav("data/data-raw/kl-only/data-raw/ChernyakHarrisCordes2019_Study2.sav") %>%
  mutate(SubjectID = paste0("2_",SubjectID))
chernyak2019 <- chernyak2019_1 %>%
  bind_rows(chernyak2019_2) %>%
  rename(Subject = SubjectID,
         Sex = Gender,
         KL = GiveN,
         Age_years = Age) %>%
  mutate(Age_months = Age_years * 12,
         KL = as.character(KL),
         KL = ifelse(KL == "6", "CP", KL),
         Language = "English",
         method="titrated",
         Experiment = "Chernyak_submitted",
         lab = "Cordes",
         cite = "Chernyak, N., Harris, P., & Cordes, S. (2019). Explaining early moral hypocrisy: Numerical cognition promotes equal sharing behavior in preschool-aged children. Developmental Science, 22(1), e12695.",
         typical = "typical") %>%
  select(Experiment, lab, Language, Subject, KL, Age_months, Age_years, method, cite)
glimpse(chernyak2019)





#mostly renaming
data.raw %<>%
  mutate(Experiment = ifelse(Language == "Mandarin", "Almoammer_Barner_2013", Experiment), 
         Subject = case_when(
            # is.na(Subject) & !is.na(Subnum) ~ Subnum, #subnum no longer included
            is.na(Subject) & !is.na(Participant) ~ Participant,
            TRUE ~ Subject
         ),
         # Age_months = ifelse((is.na(Age) & !is.na(Age.Mos)), Age.Mos, Age), ## looks like this is no longer an issue
         KL = ifelse((is.na(KL) & !is.na(Knower.Level)), Knower.Level, KL), 
         KL = ifelse(is.na(KL), Knower_level, KL), 
         KL = ifelse(is.na(KL), `Knower-level`, KL),
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual")), 
         Sex = NA)%>%
  filter(KL != "ChangeMe", 
         KL != "X")%>%
  mutate(KL = ifelse((KL == "5" & (Experiment == "Marusic_Barner_2016" | Experiment == "Almoammer_Barner_2013")), "CP", 
                           as.character(KL)), 
         KL = ifelse(KL == "5K", "5", as.character(KL)),
         KL = ifelse(KL == "4K", "4", as.character(KL)), 
         KL = ifelse(KL == "Non", "0", as.character(KL)))



#select relevant columns, round for age and mutate years

data.raw %<>%
  mutate(Age_months = round(as.numeric(as.character(Age), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, lab, Language, Subject, Sex, KL, Age_months, Age_years, method)%>%
  mutate(cite = case_when(
    Experiment == "Almoammer_Barner_2013" ~ "Almoammer, A., Sullivan, J., Donlan, C., Marusic, F., O’Donnell, T., & Barner, D. (2013). Grammatical morphology as a source of early number word meanings. Proceedings of the National Academy of Sciences, 110(46), 18448-18453.", 
    Experiment == "Barner_Yang_2009" | Experiment == "BarnerChowYoungE2" ~ "Barner, D., Chow, K., & Yang, S. J. (2009). Finding one’s meaning: A test of the relation between quantifiers and integers in language development. Cognitive psychology, 58(2), 195-219.", 
    Experiment == "Barner_Takasaki_2009" ~ "Barner, D., Libenson, A., Cheung, P., & Takasaki, M. (2009). Cross-linguistic relations between quantifiers and numerals in language acquisition: Evidence from Japanese. Journal of experimental child psychology, 103(4), 421-440.", 
    Experiment == "Marusic_Barner_2016" ~ "Marusic, F., Zaucer, R., Plesnicar, V., Razborsek, T., Sullivan, J., & Barner, D. (2016). Does grammatical structure accelerate number word learning? Evidence from learners of dual and non-dual dialects of Slovenian. PloS one, 11(8), e0159208.", 
    Experiment == "Piantadosi_Gibson_2014" ~ "Piantadosi, S. T., Jara‐Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming‐foraging group. Developmental Science, 17(4), 553-563.", 
    Experiment == "Schneider_Barner_UnderReview" ~ "Schneider, R.M., Brockbank, E., Feiman, R., & Barner, D. (under review). Counting and the ontogenetic origins of exact equality.", #LAO: changed SchneiderBarner2020 to SchneiderBarner_2020; check
    Experiment == "Schneider_Barner_2021" ~ "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.",
    Experiment == "Schneider_Barner_2020" ~ "Schneider, R. M., Sullivan, J., Marusic, F., Biswas, P., Mismas, P., Plesnicar, V., & Barner, D. (2020). Do children use language structure to discover the recursive rules of counting?. Cognitive psychology, 117, 101263.",
    Experiment == "Schneider_Barner_Unpublished" ~ "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset).", 
    Experiment == "Marchand2019" ~ "Marchand, E., & Barner, D. (2019). Acquisition of French Un. Proceedings of the 41st Annual Conference of the Cognitive Science Society.", 
    Experiment == "MarchandUnderRev" ~ "Marchand, E., & Barner, D. (2020). How reliable if the Give-a-number task?. Proceedings of the 42nd Annual Conference of the Cognitive Science Society."
  )) %>%
  mutate(typical = ifelse(Experiment == "Schneider_Barner_2020", "non-typical", "typical"))


# Save and export ----
write_csv(data.raw, here::here("data/processed-data/kl_data_processed.csv")) #to main data
write_csv(data.raw, here::here("shiny_app/data/processed-data/kl_data_processed.csv")) #to main data
