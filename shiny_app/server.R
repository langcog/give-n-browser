library(tidyverse)
library(ggthemes)
library(langcog)

# read data
trials <- read_csv(here::here("data/processed-data/trials.csv"))
subjects <- read_csv(here::here("data/processed-data/subjects.csv"))
datasets <- read_csv(here::here("data/processed-data/datasets.csv"))

#note that this doesn't include anyone who *only* has KL data
#looks like we have an NA dataset, not sure why that might be

all_data <- left_join(trials, subjects) %>%
  left_join(datasets)%>%
  distinct(dataset_id, trial_id, subject_id, Query, 
           Response, language, age, KL, lab, method, cite) %>% #some weird duplication, fix this
  mutate(Response = ifelse(Response > 10, 10, as.numeric(Response)))

age_min <- floor(min(all_data$age, na.rm = TRUE))
age_max <- ceiling(max(all_data$age, na.rm = TRUE))
query_min <- floor(min(all_data$Query, na.rm = TRUE))
query_max <- ceiling(max(all_data$Query, na.rm = TRUE))

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  filtered_data <- reactive({
    all_data %>%
      filter(age >= input$age_range[1], 
             age <= input$age_range[2], 
             Query >= input$query_range[1], 
             Query <= input$query_range[2])
  })
 
  ## ----------------------- SELECTORS -----------------------

  output$age_range_selector <- renderUI({
    sliderInput("age_range",
                label = "Ages to include (months)",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })
  
  output$query_range_selector <- renderUI({
    sliderInput("query_range",
                label = "Queried numbers to include",
                value = c(query_min, query_max),
                step = 1,
                min = query_min, max =query_max)
  })
  
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
      theme(legend.position = "none") +
      labs(y = "Frequency", x = "Number given")+
      facet_wrap(~Query)
  })
  
}

