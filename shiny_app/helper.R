library(tidyverse)
library(ggthemes)
library(langcog)
library(shinyWidgets)
library(DT)
library(haven)

# read data
trials <- read_csv(here::here("data/processed-data/trials.csv"))
subjects <- read_csv(here::here("data/processed-data/subjects.csv"), col_types = cols(highest_count = col_number()))
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
  mutate(method = str_remove(method, ", NA"),
         method = str_remove(method, "NA, "))

langMetaData <- all_data %>%
  distinct(cite, language) %>%
  pivot_wider(names_from = language, values_from = language) %>%
  unite("language",2:ncol(.), sep=", ") %>%
  mutate(language = str_remove_all(language, ", NA"),
         language = str_remove(language, "NA, "),
         language = ifelse(str_detect(language, "Slovenian_dual, Slovenian_nonDual"), "Slovenian", language))
  

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
  left_join(langMetaData) %>%
  left_join(metaData) %>%
  mutate(htmlTxt = paste0("<button class='collapsed' id='", cite_id, "' onclick='show(\"", cite_id, "\");'>", 
                          cite,
                          "</button>",
                          "<div class='content' id='", cite_id, "_content'>",
                          "<p>",
                          "N = ", n, "<br>",
                          "Min age (months) = ", min.age, "<br>",
                          "Max age (months) = ", max.age, "<br>",
                          "Method(s) = ", method, "<br>",
                          "Language(s) = ", language,
                          "</p></div>"))
  
all_datasets_short <- all_datasets$shortCite
##get only language for which we have KLs
languages_KL <- c(unique(subset(all_data, !is.na(KL))$language))
##get only language for which we have Queries
languages_item <- c(unique(subset(all_data, !is.na(Query))$language))
##get only language for which we have highest_counts
languages_hc <- c(unique(subset(all_data, !is.na(highest_count))$language))
# #get only datasets for which we have KLs
datasets_KL <- c(unique(subset(all_data, !is.na(KL))$dataset_id))
# #get only datasets for which we have highest counts
all_datasets_short_hc <- c(unique(subset(all_data, !is.na(highest_count))$shortCite))
# #get only datasets for which we have queries
# datasets_item <- c(unique(subset(all_data, !is.na(Query))$dataset_id))
methods <- c("titrated", "non-titrated")
queries <- c(as.character(sort(unique(all_data$Query))))
n.samps <- 100
defaultParams = list(22, 
                     144, 
                     c("1-knower", "2-knower", "3-knower", "CP-knower"), 
                     "English",
                     unique(all_data$shortCite))



##sampling functions
sample.ns <- function(df) {
  df %<>% 
    sample_n(nrow(df), replace=TRUE) %>%
    group_by(KL, language, age_months) %>%
    summarise(n = n()) %>%
    mutate(cum.n = cumsum(n),
           prop = cum.n / sum(n))
  return(df)
}

sample.dat <- function(df) {
  bind_rows(replicate(n.samps, sample.ns(df), simplify=FALSE)) %>%
    group_by(KL, language, age_months) %>%
    summarise(ci.low = quantile(prop, .025), 
              ci.high = quantile(prop, .975))
}

#get ns, cumulative sums, and props for dataset from default parameters
get_all_data <- all_data %>%
  filter(!is.na(age_months), 
         !is.na(KL),
         age_months >= defaultParams[[1]],
         age_months <= defaultParams[[2]],
         KL %in% defaultParams[[3]],
         language == defaultParams[[4]],
         shortCite %in% defaultParams[[5]])%>%
  select(KL, language, age_months)

sample.n_all <- sample.dat(get_all_data)

n_all <- get_all_data %<>% 
  group_by(KL, language, age_months) %>%
  summarise(n = n()) %>%
  mutate(cum.n = cumsum(n),
         prop = cum.n / sum(n))
n_all <- left_join(n_all, sample.n_all)


# flexible reformatting of submitted data

callback.colnames <- c(
  "table.on('dblclick.dt', 'thead th', function(e) {",
  "  var $th = $(this);",
  "  var index = $th.index();",
  "  var colname = $th.text(), newcolname = colname;",
  "  var $input = $('<input type=\"text\">')",
  "  $input.val(colname);",
  "  $th.empty().append($input);",
  "  $input.on('change', function(){",
  "    newcolname = $input.val();",
  "    if(newcolname != colname){",
  "      $(table.column(index).header()).text(newcolname);",
  "    }",
  "    $input.remove();",
  "  }).on('blur', function(){",
  "    $(table.column(index).header()).text(newcolname);",
  "    $input.remove();",
  "  });",
  "});"
)


## trial level data
convert_trialLevel <- function(df){
  
}











#brainstorm plots

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}


lang_data <- all_data %>%
  mutate(language = ifelse(str_detect(language, "Slovenian"), "Slovenian", language)) %>%
  distinct(dataset_id, subject_id, language) %>%
  count(language) %>%
  arrange(desc(n)) %>%
  filter(n > 1)
lang_data_lab <- lang_data %>%
  mutate(i = 1:n(),
         angle = 90 - 360 * (i-0.5) / n(),
         hjust = ifelse( angle < -90, 1, 0),
         angle = ifelse(angle < -90, angle+180, angle),
         n = ifelse(language=="French", log2(n)-4, log2(n)-5))



langPlot <- ggplot(lang_data, aes(x=reorder(language, -n), y=log2(n), fill=reorder(language, -n))) +
  geom_bar(stat="identity") +
  geom_text(data=lang_data_lab,
            aes(x=reorder(language, -n), y=n, label=language, hjust=hjust), 
            color="white", fontface="bold",alpha=0.8, size=5.5, 
            angle=lang_data_lab$angle, inherit.aes = FALSE) + 
  ylim(-10,15) +
  scale_fill_manual(values=darken(
    rainbow(nrow(lang_data_lab)),
    1.2
    )) +
  guides(fill="none") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  ) +
  coord_polar(start = 0)
# ggsave("www/img/langPlot.png", langPlot, width=14, height=14)
