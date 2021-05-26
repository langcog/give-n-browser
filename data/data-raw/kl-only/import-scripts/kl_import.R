## Code for processing and cleaning knower-level only data with general template: 
#Experiment, Language, SID, KL, Age_months, method, cite, Age_years 

#load libraries
rm(list = ls())
library(tidyverse)
library(tidylog)
library(magrittr)

# Read in all data from kl-only raw data ----
##TO-DO: add column for sex
data.raw <-
  list.files(path = here::here("data/data-raw/kl-only/data-raw/"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
# Wrangling and cleaning ----

#mostly renaming
data.raw %<>%
  mutate(Experiment = ifelse(Language == "Mandarin", "Almoammer2013", Experiment), 
         Subject = case_when(
            is.na(Subject) & !is.na(Subnum) ~ Subnum,
            is.na(Subject) & !is.na(Participant) ~ Participant,
            TRUE ~ Subject
         ),
         Age_months = ifelse((is.na(Age) & !is.na(Age.Mos)), Age.Mos, Age), 
         KL = ifelse((is.na(KL) & !is.na(Knower.Level)), Knower.Level, KL), 
         KL = ifelse(is.na(KL), Knower_level, KL), 
         KL = ifelse(is.na(KL), `Knower-level`, KL),
         Language = ifelse(is.na(Dual), Language, 
                      ifelse(Dual == "Dual", "Slovenian_dual", "Slovenian_nonDual")), 
         Sex = NA)%>%
  filter(KL != "ChangeMe", 
         KL != "X")%>%
  mutate(KL = ifelse((KL == "5" & (Experiment == "Marusic2016" | Experiment == "Almoammer2013")), "CP", 
                           as.character(KL)), 
         KL = ifelse(KL == "5K", "5", as.character(KL)),
         KL = ifelse(KL == "4K", "4", as.character(KL)), 
         KL = ifelse(KL == "Non", "0", as.character(KL)))

#select relevant columns, round for age and mutate years

data.raw %<>%
  mutate(Age_months = round(as.numeric(as.character(Age_months), 4)), 
         Age_years = floor(Age_months)/12)%>%
  dplyr::select(Experiment, lab, Language, Subject, KL, Age_months, Age_years, method)%>%
  mutate(cite = case_when(
    Experiment == "Almoammer2013" ~ "Almoammer, A., Sullivan, J., Donlan, C., Marusic, F., O’Donnell, T., & Barner, D. (2013). Grammatical morphology as a source of early number word meanings. Proceedings of the National Academy of Sciences, 110(46), 18448-18453.", 
    Experiment == "BarnerChowYoungE1" | Experiment == "BarnerChowYoungE2" ~ "Barner, D., Chow, K., & Yang, S. J. (2009). Finding one’s meaning: A test of the relation between quantifiers and integers in language development. Cognitive psychology, 58(2), 195-219.", 
    Experiment == "BarnerTakasaki2009" ~ "Barner, D., Libenson, A., Cheung, P., & Takasaki, M. (2009). Cross-linguistic relations between quantifiers and numerals in language acquisition: Evidence from Japanese. Journal of experimental child psychology, 103(4), 421-440.", 
    Experiment == "Marusic2016" ~ "Marusic, F., Zaucer, R., Plesnicar, V., Razborsek, T., Sullivan, J., & Barner, D. (2016). Does grammatical structure accelerate number word learning? Evidence from learners of dual and non-dual dialects of Slovenian. PloS one, 11(8), e0159208.", 
    Experiment == "Piantadosi2014" ~ "Piantadosi, S. T., Jara‐Ettinger, J., & Gibson, E. (2014). Children's learning of number words in an indigenous farming‐foraging group. Developmental Science, 17(4), 553-563.", 
    Experiment == "SchneiderBarner_2020" ~ "Schneider, R.M., & Barner, D. (2020). Children use one-to-one correspondence to establish equality after learning to count. 42nd Annual Meeting of the Cognitive Science Society.", #LAO: changed SchneiderBarner2020 to SchneiderBarner_2020; check
    Experiment == "SchneiderBarner_20xx" ~ "Schneider, R.M., Feiman, R., & Barner, D. (unpublished dataset).", 
    Experiment == "SchneiderEtAl_20xx" ~ "Schneider, R. M., Pankonin, A. H., Schachner, A., & Barner, D. (2020, September 23). Starting small: Exploring the origins of successor function knowledge. https://doi.org/10.31234/osf.io/3zngr.",
    Experiment == "SchneiderEtAl_2020" ~ "Schneider, R. M., Sullivan, J., Marusic, F., Biswas, P., Mismas, P., Plesnicar, V., & Barner, D. (2020). Do children use language structure to discover the recursive rules of counting?. Cognitive psychology, 117, 101263.",
    Experiment == "SchneiderYen_20xx" ~ "Schneider, R.M., Yen, A., & Barner, D. (unpublished dataset)."
  )) %>%
  mutate(typical = ifelse(Experiment == "SchneiderEtAl_2020", "non-typical", "typical"))


# Save and export ----
write_csv(data.raw, here::here("data/processed-data/kl_data_processed.csv"))
