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

age_min <- floor(min(all_data$age_months, na.rm = TRUE))
age_max <- ceiling(max(all_data$age_months, na.rm = TRUE))
queries <- c(as.character(sort(unique(all_data$Query))))
# kls <- c("0", "1", "2", "3", "4", "5", "CP")

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  filtered_data <- reactive({
    all_data %>%
      filter(!is.na(Query),
             !is.na(KL),
             age_months >= input$age_range[1], 
             age_months <= input$age_range[2], 
             Query == as.numeric(input$query_range))
  })
 
  ## ----------------------- SELECTORS -----------------------

  output$age_range_selector <- renderUI({
    sliderInput("age_range",
                label = "Ages to include (months):",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })
  
  output$query_range_selector <- renderUI({
    selectInput("query_range", 
                       label = "Queried numbers to include:", 
                       choices = queries, 
                       selected = c("1", "2", "3"), 
                       multiple = TRUE)
  })
  
  # output$kl_range_selector <- renderUI({
  #   selectInput("kl_range", 
  #               label = "Knower levels to include:", 
  #               choices = kls, 
  #               selected = c("1", "2", "3", "CP"), 
  #               multiple = TRUE)
  # })
  
  # output$language_selector <- renderUI({
  #   selectInput("language", "Language:", 
  #               choices=unique(all_data$language))
  # })

  
  ## ----------------------- PLOTS -----------------------
  
  ## ---------- HISTOGRAM
  output$histogram <- renderPlot({
    req(filtered_data())
    
    ggplot(filtered_data(), 
           aes(x = Response, fill = as.factor(Query))) + 
      geom_histogram(binwidth = 1, color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw() + 
      theme(legend.position = "none") +
      labs(y = "Frequency", x = "Number given")+
      facet_wrap(~Query)
  })
  
}

