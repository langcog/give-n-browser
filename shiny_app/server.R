library(tidyverse)
library(ggthemes)
library(langcog)

# read data
trials <- read_csv(here::here("processed_data/trials.csv"))
subjects <- read_csv(here::here("processed_data/subjects.csv"))
datasets <- read_csv(here::here("processed_data/datasets.csv"))

all_data <- left_join(trials, subjects) %>%
  left_join(datasets)

# ggplot(d, aes(x = response)) + 
#   geom_histogram() + 
#   facet_wrap(~quantity)

age_min <- floor(min(subjects$age, na.rm = TRUE))
age_max <- ceiling(max(subjects$age, na.rm = TRUE))

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  filtered_data <- reactive({
    all_data %>%
      filter(age > input$age_range[1], 
             age < input$age_range[2])
  })
 
  ## ----------------------- SELECTORS -----------------------

  output$age_range_selector <- renderUI({
    sliderInput("age_range",
                label = "Ages to include (months)",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })

  
  ## ----------------------- PLOTS -----------------------
  
  ## ---------- HISTOGRAM
  output$histogram <- renderPlot({
    req(filtered_data())
   
    ggplot(filtered_data(), 
           aes(x = response)) + 
      geom_histogram(binwidth = 1) + 
      facet_wrap(~quantity)
  })
  
}
