library(tidyverse)
library(ggthemes)
library(langcog)
library(shinyWidgets)

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
         language = ifelse(str_detect(language, "English"), "English", 
                           ifelse(language == "Saudi", "Arabic", as.character(language))))

## set variables
age_min <- floor(min(all_data$age_months, na.rm = TRUE))
age_max <- ceiling(max(all_data$age_months, na.rm = TRUE))
kls <- c("0", "1", "2", "3", "4", "5", "CP")
##get only language for which we have KLs
languages_KL <- c(unique(subset(all_data, !is.na(KL))$language))
##get only language for which we have Queries
languages_item <- c(unique(subset(all_data, !is.na(Query))$language))
methods <- c("titrated", "non-titrated")
queries <- c(as.character(sort(unique(all_data$Query))))
n.samps <- 100

##static data that will be filtered below for sampling
ns <- all_data %>%
  filter(!is.na(age_months), 
         !is.na(KL))%>%
  select(KL, language, age_months)
##sampling function
sample.ns <- function(df) {
  df %<>% 
    sample_n(nrow(df), replace=TRUE) %>%
    group_by(KL, language, age_months) %>%
    summarise(n = n()) %>%
    mutate(cum.n = cumsum(n),
           prop = cum.n / sum(n))
  return(df)
}

#sample
samps <- bind_rows(replicate(n.samps, sample.ns(ns), simplify=FALSE)) %>%
  group_by(KL, language, age_months) %>%
  summarise(ci.low = quantile(prop, .025), 
            ci.high = quantile(prop, .975))

#get ns, cumulative sums, and props
ns %<>% group_by(KL, language, age_months) %>%
  summarise(n = n()) %>%
  mutate(cum.n = cumsum(n),
         prop = cum.n / sum(n))

#left join with samples
ns <- left_join(ns, samps)

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
             language %in% input$language_choice_kl)
      
  })
  
    ##cumulative probability 
    ##get age_months, KL, and language points
    cumul_prob <- reactive({ 
      ns %>% 
        filter(!is.na(KL),
               !is.na(age_months),
               age_months >= input$age_range_kl[1], 
               age_months <= input$age_range_kl[2], 
               KL %in% input$kl_range_kl, 
               language %in% input$language_choice_kl)%>%
    dplyr::select(age_months, KL, language, n, cum.n, prop, 
                  ci.low, ci.high)
    })
  
  
  
  ## ... ITEM DATA -----
  filtered_data_item <- reactive({
    all_data %>%
      filter(!is.na(Query),
             !is.na(age_months),
             age_months >= input$age_range_item[1], 
             age_months <= input$age_range_item[2], 
             Query %in% as.numeric(input$query_range_item), 
             language %in% input$language_choice_item)
             # method %in% input$method_choice_item)
  })
    
    filtered_data_item_language <- reactive({
      all_data %>%
        filter(!is.na(Query),
               !is.na(age_months),
               age_months >= input$age_range_item[1], 
               age_months <= input$age_range_item[2], 
               Query %in% as.numeric(input$query_range_item), 
               language %in% input$language_choice_item)%>%
        group_by(Query, Response, language)%>%
        summarise(n = n())%>%
        group_by(Query, language)%>%
        mutate(total.n = sum(n), 
               prop = n/total.n)
      
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
                choices = languages_KL, 
                selected = "English", 
                multiple = TRUE)
  })
  
  output$method_selector <- renderUI({
    prettySwitch("method_choice_kl",
                 label = "Facet by method",
                 value = FALSE)
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
                choices = languages_item, 
                selected = "English", 
                multiple = TRUE)
  })
  
  output$query_range_selector_item <- renderUI({
    selectInput("query_range_item", 
                label = "Queried numbers to include:", 
                choices = queries, 
                selected = c("1", "2", "3"), 
                multiple = TRUE)
  })
  
    output$method_selector_item <- renderUI({
      prettySwitch("method_choice_item",
                   label = "Method plotted as colors",
                   value = FALSE)
  })

  
  ## ----------------------- PLOTS -----------------------
  
  ## ... KL PLOTS ----
  ## ---------- BOXPLOT OF KL AND AGE BY LANGUAGE
  output$age_boxplot <- renderPlot({
    req(filtered_data_kl())
    
    if (input$method_choice_kl) {
       p <- ggplot(filtered_data_kl(), 
                   aes(x = language, y=age_months, fill = KL))+
         geom_boxplot(alpha = .5, 
                      color = "black") +
         theme_bw(base_size=14) +
         scale_fill_solarized("Knower level") + 
         labs(x = 'Language', 
              y = "Age (months)") +
         facet_grid(~method) +
         coord_flip()
    } else {
      p <- ggplot(filtered_data_kl(), 
                  aes(x = language, y=age_months, fill = KL))+
        geom_boxplot(alpha = .5, 
                     color = "black") +
        theme_bw(base_size=14) +
        scale_fill_solarized("Knower level") + 
        labs(x = 'Language', 
             y = "Age (months)") +
        coord_flip()
    }
    p
  })
  
  # ## ---- BOXPLOT OF KL BY AGE BY LANGUAGE BY METHOD
  # output$method_boxplot <- renderPlot({
  #   req(filtered_data_kl())
  #   
  #   ggplot(filtered_data_kl(), 
  #          aes(x = language, y=age_months, fill = KL))+
  #     geom_boxplot(alpha = .5, 
  #                  color = "black") +
  #     theme_bw(base_size=14) +
  #     scale_fill_solarized("Knower level") + 
  #     labs(x = 'Language', 
  #          y = "Age (months)") +
  #     facet_grid(~method) +
  #     coord_flip()
  # })
  
  ##----CUMULATIVE PROBABILITY OF BEING N-KNOWER
  #plot
  output$cumulative_prob <- renderPlot({
    req(cumul_prob)
    
    ggplot(cumul_prob(), 
           aes(x = age_months, y = prop, colour=KL, fill=KL,
               group=KL))+
    geom_line(size=1) + 
    xlab("Age (months)") + 
    geom_ribbon(aes(ymin = ci.low, ymax= ci.high), 
                alpha = .2,show_guide=FALSE,linetype=0) + 
    # geom_vline(aes(xintercept=age_months[prop>.75][1]), lty=3) +
    # scale_x_continuous(breaks=seq(0,24,4))+
    scale_color_solarized("Knower level")+
    scale_fill_solarized()+
    scale_y_continuous(limits = c(0,1),
                       name = "Cumulative Probability of Knower Level")+
    theme_bw(base_size=14) +
    theme(legend.position="right") + 
    facet_wrap(~language, scales = "free_x")
  })
  
 ## ... ITEM PLOTS ----
  
  ## .... AVG HISTOGRAM ----
  output$avg_histogram <- renderPlot({
    req(filtered_data_item())
    
    avg_item <- filtered_data_item() %>%
      group_by(Query, Response)%>%
      summarise(n = n())%>%
      mutate(total.n = sum(n), 
             prop = n/total.n)
    
    method_df <- filtered_data_item() %>%
      group_by(Query, Response, method)%>%
      summarise(n = n())%>%
      group_by(method)%>%
      mutate(total.n = sum(n), 
             prop = n/total.n)
  
  if (input$method_choice_item) {
    p <- ggplot(method_df, 
                aes(x = Response, y = prop, fill = method)) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_bar(stat = 'identity', position = position_dodge(), 
               color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized("Method") +
      theme_bw(base_size=14) +
      theme(legend.position = "right", 
            panel.grid = element_blank()) +
      labs(y = "Proportion of responses", x = "Number given")+
      facet_grid(~Query)
  } else {
    p <- ggplot(avg_item, 
                aes(x = Response, y = prop, fill = as.factor(Query))) +
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_bar(stat = 'identity', position = position_dodge(), color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw(base_size=14) + 
      theme(legend.position = "none", 
            panel.grid = element_blank()) +
      labs(y = "Proportion of responses", x = "Number given")+
      facet_wrap(~Query)
  }
    p
  })
  
  ## .... LANG HISTOGRAM ----
  output$lang_histogram <- renderPlot({
    req(filtered_data_item_language())
    
    ggplot(filtered_data_item_language(), 
           aes(x = Response, y = prop, fill = as.factor(Query))) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
      geom_bar(stat = 'identity', position = position_dodge(), color = 'black') + 
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw(base_size=14) +
      theme(legend.position = "none", 
            panel.grid = element_blank()) +
      labs(y = "Proportion of responses", x = "Number given")+
      facet_grid(language~Query)
  })
  
  ## .... METHOD HISTOGRAM ----
  # output$method_histogram <- renderPlot({
  #   req(filtered_data_item_method())
  #   
  #   ggplot(filtered_data_item_method(), 
  #          aes(x = Response, y = prop, fill = method)) + 
  #     geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'grey') +
  #     geom_bar(stat = 'identity', position = position_dodge(), 
  #              color = 'black') + 
  #     scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
  #     scale_fill_solarized("Method") +
  #     theme_bw(base_size=14) +
  #     theme(legend.position = "right", 
  #           panel.grid = element_blank()) +
  #     labs(y = "Proportion of responses", x = "Number given")+
  #     facet_grid(~Query)
  # })
}












