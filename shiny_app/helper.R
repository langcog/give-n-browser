library(tidyverse)
library(ggthemes)
library(langcog)
library(shinyWidgets)
library(DT)

# read data
trials <- read_csv(here::here("data/processed-data/trials.csv"))
subjects <- read_csv(here::here("data/processed-data/subjects.csv"))
datasets <- read_csv(here::here("data/processed-data/datasets.csv"))


## join the data, rename KLs
all_data <- full_join(subjects, trials) %>%
  left_join(datasets)%>%
  mutate(Response = ifelse(Response > 10, 10, as.numeric(Response)), 
         KL = case_when(
           KL == "Non" | KL == "0" ~ "0-knower", 
           KL == "4K" | KL == "4" ~ "4-knower",
           KL == "5K" | KL == "5" ~ "5-knower", 
           KL == "X" ~ "NA", 
           KL == "1" ~ "1-knower", 
           KL == "2" ~ "2-knower", 
           KL == "3" ~ "3-knower", 
           TRUE ~ "CP-knower"
         ),
         language = case_when(
           str_detect(language, "English") ~ "English", 
           language == "Saudi" ~ "Arabic", 
           TRUE ~ as.character(language)
         ), 
         method = ifelse(method == "Non-titrated", "non-titrated", as.character(method)), 
         CP_subset = ifelse(KL == "CP-knower", "CP-knower", "Subset-knower"), 
         CP_subset = factor(CP_subset, levels = c("Subset-knower", "CP-knower")))


# organize citations

getLastNames <- function(origAuthors){
  temp <- matrix(strsplit(origAuthors, ",")[[1]], ncol = 2, byrow=TRUE) #splits string by commas
  authors <- paste(trimws(temp[,1]), collapse=", ") #removes first initial and retains last names or et als
  return(authors)
}

methodMetaData <- all_data %>%
  distinct(cite, method) %>%
  pivot_wider(names_from = method, values_from = method) %>%
  unite("method", 2:3, sep=", ") %>%
  mutate(method = case_when(
    method == "non-titrated, NA" ~ "non-titrated",
    method == "NA, titrated" ~ "titrated",
    TRUE ~ method
  ))

metaData <- all_data %>%
  distinct(cite, subject_id, age_months) %>%
  group_by(cite) %>%
  summarise(min.age = min(age_months, na.rm = TRUE),
            max.age = max(age_months, na.rm = TRUE),
            n = n())

citeMap <- all_data %>%
  distinct(cite) %>%
  mutate(beforeYear = gsub("^(.*?)[(].*", "\\1", cite), # get content before first parenthesis
         beforeYear = sapply(beforeYear, getLastNames), # pull out just the last names
         inYear = str_extract(cite, "[(].*?[)]"), # get content between parentheses
         inYear = str_replace_all(inYear, "[[:punct:]]", ""), # remove punctuations -- parentheses & commas
         inYear = word(inYear, 1), # get first word
         shortCite = paste0(beforeYear, " (", inYear, ")"), # paste together with parentheses around year
         firstAuthor = str_replace_all(word(beforeYear, 1), "[[:punct:]]", ""),
         otherAuthors = str_replace_all(word(beforeYear, 2, -1), "[[:punct:]]", ""),
         orderCite = paste(firstAuthor, inYear, otherAuthors)) %>% # for APA ordering
  select(cite, shortCite, orderCite)

all_data <- all_data %>%
  left_join(citeMap)


## set variables
age_min <- floor(min(all_data$age_months, na.rm = TRUE))
age_max <- ceiling(max(all_data$age_months, na.rm = TRUE))
kls <- c("0-knower", "1-knower", "2-knower", "3-knower", "4-knower", "5-knower", "CP-knower")
all_datasets <- all_data %>%
  distinct(cite, shortCite, orderCite) %>%
  arrange(orderCite) %>%
  mutate(cite_id = paste0("cite",1:n()))
all_datasets_full <- tibble(cite = all_datasets$cite,
                            cite_id = all_datasets$cite_id) %>%
  left_join(methodMetaData) %>%
  left_join(metaData) %>%
  mutate(htmlTxt = paste0("<button class='collapsed' id='", cite_id, "' onclick='show(\"", cite_id, "\");'>", 
                          cite,
                          "</button>",
                          "<div class='content' id='", cite_id, "_content'>",
                          "<p>",
                          "N = ", n, "<br>",
                          "Min age (months) = ", min.age, "<br>",
                          "Max age (months) = ", max.age, "<br>",
                          "Method(s) = ", method,
                          "</p></div>"))
  
all_datasets_short <- all_datasets$shortCite
##get only language for which we have KLs
languages_KL <- c(unique(subset(all_data, !is.na(KL))$language))
##get only language for which we have Queries
languages_item <- c(unique(subset(all_data, !is.na(Query))$language))
# #get only datasets for which we have KLs
datasets_KL <- c(unique(subset(all_data, !is.na(KL))$dataset_id))
# #get only datasets for which we have queries
# datasets_item <- c(unique(subset(all_data, !is.na(Query))$dataset_id))
methods <- c("titrated", "non-titrated")
queries <- c(as.character(sort(unique(all_data$Query))))
n.samps <- 100


