library(tidyverse)
library(ggthemes)
library(langcog)

# read data
trials <- read_csv(here::here("data/processed-data/trials.csv"))
subjects <- read_csv(here::here("data/processed-data/subjects.csv"))
datasets <- read_csv(here::here("data/processed-data/datasets.csv"))

all_data <- full_join(subjects, trials) %>%
  left_join(datasets)%>%
  mutate(Response = ifelse(Response > 10, 10, as.numeric(Response)), 
         KL = ifelse(KL == "Non", "0", 
                     ifelse(KL == "4K", "4",
                            ifelse(KL == "5K", "5", 
                                   ifelse(KL == "X", NA, as.character(KL))))),
         language = ifelse(str_detect(language, "English"), "English", as.character(language)))

kls <- c("0", "1", "2", "3", "4", "5", "CP")
##get only language for which we have KLs
languages <- c(unique(subset(all_data, !is.na(KL))$language))

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  filtered_data <- reactive({
    all_data %>%
      distinct(dataset_id, subject_id, age_months, KL, language)%>%
      filter(!is.na(KL),
             !is.na(age_months),
             KL %in% input$kl_range, 
             language %in% input$language_choice)
      
  })
 
  ## ----------------------- SELECTORS -----------------------

  output$kl_range_selector <- renderUI({
    selectInput("kl_range", 
                label = "Knower levels to include:", 
                choices = kls, 
                selected = c("1", "2", "3", "CP"), 
                multiple = TRUE)
  })
  
  output$language_selector <- renderUI({
    selectInput("language_choice", 
                label = "Languages to include:", 
                choices = languages, 
                selected = "English", 
                multiple = TRUE)
  })
  
  # output$language_selector <- renderUI({
  #   selectInput("language", "Language:", 
  #               choices=unique(all_data$language))
  # })

  
  ## ----------------------- PLOTS -----------------------
  
  ## ---------- HISTOGRAM
  output$boxplot <- renderPlot({
    req(filtered_data())
  
      ggplot(filtered_data(), 
             aes(x = language, y=age_months, fill = KL))+
      geom_boxplot(alpha = .5, 
                   color = "black") +
        theme_bw() + 
        scale_fill_solarized() + 
        labs(x = 'Language', 
             y = "Age (months)") +
      coord_flip()
  })
  
}

