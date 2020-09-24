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
kls <- c("0", "1", "2", "3", "4", "5", "CP")
##get only language for which we have KLs
languages <- c(unique(subset(all_data, !is.na(KL))$language))
methods <- c("titrated", "non-titrated")

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  
  ## ... KL DATA ----
  filtered_data_kl <- reactive({
    all_data %>%
      distinct(dataset_id, subject_id, age_months, KL, method, language)%>%
      filter(!is.na(KL),
             !is.na(age_months),
             age_months >= input$age_range_kl[1], 
             age_months <= input$age_range_kl[2], 
             KL %in% input$kl_range_kl, 
             language %in% input$language_choice_kl, 
             method %in% input$method_choice_kl)
      
  })
  
  filtered_data_prop_kl <- reactive({
    all_data %>%
      distinct(dataset_id, subject_id, age_months, method, KL, language)%>%
      filter(!is.na(KL),
             !is.na(age_months),
             age_months >= input$age_range_kl[1], 
             age_months <= input$age_range_kl[2], 
             KL %in% input$kl_range_kl, 
             language %in% input$language_choice_kl, 
             method %in% input$method_choice_kl)%>%
      group_by(language, KL, age_months)%>%
      summarise(n = n())%>%
      group_by(language, age_months)%>%
      mutate(total.n = sum(n), 
             prop = n/total.n)
    
  })
  
  ## ... ITEM DATA -----
  filtered_data_item <- reactive({
    all_data %>%
      filter(!is.na(Query),
             !is.na(age_months),
             age_months >= input$age_range_item[1], 
             age_months <= input$age_range_item[2], 
             Query %in% as.numeric(input$query_range_item), 
             language %in% input$language_choice_item, 
             method %in% input$method_choice_item)
  })
  
 
  ## ----------------------- SELECTORS -----------------------
  
  ## ... KL selectors####

  output$kl_range_selector <- renderUI({
    selectInput("kl_range_kl", 
                label = "Knower levels to include:", 
                choices = kls, 
                selected = c("1", "2", "3", "CP"), 
                multiple = TRUE)
  })
  
  output$language_selector <- renderUI({
    selectInput("language_choice_kl", 
                label = "Languages to include:", 
                choices = languages, 
                selected = "English", 
                multiple = TRUE)
  })
  
  output$method_selector <- renderUI({
    checkboxGroupInput("method_choice_kl", 
                "Method",
                choices = methods, 
                selected = methods,
                inline = TRUE)
  })
  
  output$age_range_selector <- renderUI({
    sliderInput("age_range_kl",
                label = "Ages to include (months):",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })
  
  ## ... Item selectors####
  output$age_range_selector_item <- renderUI({
    sliderInput("age_range_item",
                label = "Ages to include (months):",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })
  
  output$language_selector_item <- renderUI({
    selectInput("language_choice_item", 
                label = "Languages to include:", 
                choices = languages, 
                selected = "English", 
                multiple = TRUE)
  })
  
  output$query_range_selector <- renderUI({
    selectInput("query_range_item", 
                label = "Queried numbers to include:", 
                choices = queries, 
                selected = c("1", "2", "3"), 
                multiple = TRUE)
  })
  
  output$method_selector_item <- renderUI({
    selectInput("method_choice_item", 
                label = "Method", 
                choices = methods, 
                selected = methods, 
                multiple = TRUE)
  })

  
  ## ----------------------- PLOTS -----------------------
  
  ## ... KL PLOTS ----
  ## ---------- BOXPLOT OF KL AND AGE BY LANGUAGE
  output$age_boxplot <- renderPlot({
    req(filtered_data_kl())
  
      ggplot(filtered_data_kl(), 
             aes(x = language, y=age_months, fill = KL))+
      geom_boxplot(alpha = .5, 
                   color = "black") +
        theme_bw() + 
        scale_fill_solarized() + 
        labs(x = 'Language', 
             y = "Age (months)") +
      coord_flip()
  })
  
  ## ---- BOXPLOT OF KL BY AGE BY LANGUAGE BY METHOD
  output$method_boxplot <- renderPlot({
    req(filtered_data_kl())
    
    ggplot(filtered_data_kl(), 
           aes(x = language, y=age_months, fill = KL))+
      geom_boxplot(alpha = .5, 
                   color = "black") +
      theme_bw() + 
      scale_fill_solarized() + 
      labs(x = 'Language', 
           y = "Age (months)") +
      facet_grid(~method) +
      coord_flip()
  })
  
  ##----BARPLOT OF PROP OF KIDS AT PARTICULAR KL AT AGE BY LANGUAGE
  output$prop_barplot <- renderPlot({
    req(filtered_data_prop_kl())
    
    ggplot(filtered_data_prop_kl(),
           aes(x = age_months, y=prop, fill = KL))+
      geom_bar(stat = 'identity', color = 'black') +
      theme_bw() +
      scale_fill_solarized() +
      theme(legend.position = "right") +
      labs(x = 'Age (months)',
           y = "Proportion of children") +
      facet_wrap(~language, ncol = 2, scales = "free_x")
  })
  
 ## ... ITEM PLOTS ----
  
  ## .... AVG HISTOGRAM ----
  output$avg_histogram <- renderPlot({
    req(filtered_data_item())
    
    ggplot(filtered_data_item(), 
           aes(x = Response, fill = as.factor(Query))) +
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_histogram(binwidth = 1, color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw() + 
      theme(legend.position = "none", 
            panel.grid = element_blank()) +
      labs(y = "Frequency", x = "Number given")+
      facet_wrap(~Query)
  })
  
  ## .... LANG HISTOGRAM ----
  output$lang_histogram <- renderPlot({
    req(filtered_data_item())
    
    ggplot(filtered_data_item(), 
           aes(x = Response, fill = as.factor(Query))) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_histogram(binwidth = 1, color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw() + 
      theme(legend.position = "none", 
            panel.grid = element_blank()) +
      labs(y = "Frequency", x = "Number given")+
      facet_grid(language~Query)
  })
  
  ## .... METHOD HISTOGRAM ----
  output$method_histogram <- renderPlot({
    req(filtered_data_item())
    
    ggplot(filtered_data_item(), 
           aes(x = Response, fill = method)) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_histogram(binwidth = 1, color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw() + 
      theme(legend.position = "right", 
            panel.grid = element_blank()) +
      labs(y = "Frequency", x = "Number given")+
      facet_grid(~Query)
  })
}

