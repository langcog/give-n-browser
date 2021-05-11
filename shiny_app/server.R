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
         KL = ifelse((KL == "Non" | KL == "0"), "0-knower", 
                     ifelse((KL == "4K" | KL == "4"), "4-knower",
                            ifelse((KL == "5K" | KL == "5"), "5-knower", 
                                   ifelse(KL == "X", NA, 
                                          ifelse(KL == "1", "1-knower", 
                                                 ifelse(KL == "2", "2-knower", 
                                                        ifelse(KL == "3", "3-knower", "CP-knower"))))))),
         language = ifelse(str_detect(language, "English"), "English", 
                           ifelse(language == "Saudi", "Arabic", as.character(language))), 
         method = ifelse(method == "Non-titrated", "non-titrated", as.character(method)), 
         CP_subset = ifelse(KL == "CP-knower", "CP-knower", "Subset-knower"), 
         CP_subset = factor(CP_subset, levels = c("Subset-knower", "CP-knower")))

## set variables
age_min <- floor(min(all_data$age_months, na.rm = TRUE))
age_max <- ceiling(max(all_data$age_months, na.rm = TRUE))
kls <- c("0-knower", "1-knower", "2-knower", "3-knower", "4-knower", "5-knower", "CP-knower")
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

### CDF ###
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
    # if(input$dataset_selector) {
    #   all_data %>%
    #     distinct(dataset_id, subject_id, age_months, KL, method, language, cite)%>%
    #     filter(!is.na(KL),
    #            !is.na(age_months),
    #            age_months >= input$age_range_kl[1], 
    #            age_months <= input$age_range_kl[2], 
    #            KL %in% input$kl_range_kl,
    #            language %in% input$language_choice_kl, 
    #            dataset_id %in% input$dataset_choice_kl)
    # } else {
    #   all_data %>%
    #     distinct(dataset_id, subject_id, age_months, KL, method, language, cite)%>%
    #     filter(!is.na(KL),
    #            !is.na(age_months),
    #            age_months >= input$age_range_kl[1], 
    #            age_months <= input$age_range_kl[2], 
    #            KL %in% input$kl_range_kl,
    #            language %in% input$language_choice_kl)
    # }
    all_data %>%
          distinct(dataset_id, subject_id, age_months, KL, method, language, cite, CP_subset)%>%
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
             !is.na(KL),
             age_months >= input$age_range_item[1], 
             age_months <= input$age_range_item[2], 
             Query %in% as.numeric(input$query_range_item), 
             language %in% input$language_choice_item, 
             KL %in% input$kl_range_item)
             # method %in% input$method_choice_item)
  })
    
    
    
 
  ## ----------------------- SELECTORS -----------------------
  
  ## ... KL selectors####

  output$kl_range_selector <- renderUI({
    selectInput("kl_range_kl", 
                label = "Knower levels to include:", 
                choices = kls, 
                selected = c("1-knower", "2-knower", "3-knower", "CP-knower"), 
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
  
  output$cp_subset_selector <- renderUI({
    prettySwitch("cp_subset_kl",
                 label = "Group non-CP-knowers together",
                 value = FALSE)
  })
  
  # output$dataset_selector <- renderUI({
  #   x <- filtered_data_kl()
  #   y <- x$dataset_id
  #   
  #   selectInput("dataset_choice_kl", 
  #               label = "Datasets to include:", 
  #               choices = y, 
  #               selected = as.list(y), 
  #               multiple = TRUE) 
  # })
  
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
    
    output$kl_range_selector_item <- renderUI({
      selectInput("kl_range_item", 
                  label = "Knower levels to include:", 
                  choices = kls, 
                  selected = c("1-knower", "2-knower", "3-knower", "CP-knower"), 
                  multiple = TRUE)
    })
    
    output$kl_facet_selector <- renderUI({
      prettySwitch("kl_selector",
                   label = "Facet by knower level",
                   value = FALSE)
    })
    
    

  
  ## ----------------------- PLOTS -----------------------
  
  ## ... KL PLOTS ----
  ## ---------- BOXPLOT OF KL AND AGE BY LANGUAGE
  output$age_boxplot <- renderPlot({
    req(filtered_data_kl())
    
    if (input$method_choice_kl) {
      if (input$cp_subset_kl) {
        p <- ggplot(filtered_data_kl(), 
                    aes(x = language, y=age_months, fill = CP_subset, color = CP_subset))+
          geom_boxplot(alpha = .7, 
                       color = "black") +
          geom_point(position = position_jitterdodge(jitter.width=0.1, dodge.width = .79), alpha=0.35,
                     show.legend = FALSE)+
          theme_bw(base_size=14) +
          scale_fill_solarized("CP-/Subset-knower", rev) +
          scale_color_solarized("CP-/Subset-knower") + 
          labs(x = 'Language', 
               y = "Age (months)") +
          facet_grid(~method) +
          coord_flip() + guides(fill = guide_legend(reverse = FALSE))
      } else {
      ## get the number of observations
       p <- ggplot(filtered_data_kl(), 
                   aes(x = language, y=age_months, fill = KL, color = KL))+
         geom_boxplot(alpha = .7, 
                      color = "black") +
         geom_point(position = position_jitterdodge(jitter.width=0.1, dodge.width = .79), alpha=0.35,
                    show.legend = FALSE)+
         theme_bw(base_size=14) +
         scale_fill_solarized("Knower level", rev) +
         scale_color_solarized("Knower level") + 
         labs(x = 'Language', 
              y = "Age (months)") +
         facet_grid(~method) +
         coord_flip() + guides(fill = guide_legend(reverse = TRUE))
      }
    } else {
      if (input$cp_subset_kl) {
        p <- ggplot(filtered_data_kl(), 
                    aes(x = language, y=age_months, fill = CP_subset, color = CP_subset))+
          geom_boxplot(alpha = .7, 
                       color = "black") +
          geom_point(position = position_jitterdodge(jitter.width=0.1, dodge.width = .79), alpha=0.35,
                     show.legend = FALSE)+
          theme_bw(base_size=14) +
          scale_fill_solarized("CP-/Subset-knower", rev) +
          scale_color_solarized("CP-/Subset-knower") + 
          labs(x = 'Language', 
               y = "Age (months)") +
          coord_flip() + guides(fill = guide_legend(reverse = FALSE))
      } else {
        p <- ggplot(filtered_data_kl(), 
                    aes(x = language, y=age_months, fill =KL, color = KL))+
          geom_boxplot(alpha = .7, 
                       color = "black") +
          geom_point(position = position_jitterdodge(jitter.width=0.1, dodge.width = .79), alpha=0.35,
                     show.legend = FALSE)+
          theme_bw(base_size=14) +
          scale_fill_solarized("Knower level") + 
          scale_color_solarized("Knower level") + 
          labs(x = 'Language', 
               y = "Age (months)") +
          coord_flip() + 
          guides(fill = guide_legend(reverse = TRUE))
      }
    }
    p
  })
    
  ## ----- TABLE FOR KL BOXPLOT ----
    output$table <- renderDataTable({
      
      if (input$method_choice_kl) {
        if (input$cp_subset_kl) {
          kl_table <- filtered_data_kl() %>%
            group_by(language, method, CP_subset)%>%
            summarise(n = n(), 
                      `Mean age` = round(mean(age_months, na.rm = TRUE),2), 
                      `SD age` = round(sd(age_months, na.rm = TRUE),2), 
                      `Median age` = round(median(age_months, na.rm = TRUE), 2))
        } else {
          kl_table <- filtered_data_kl() %>%
            group_by(language, method, KL)%>%
            summarise(n = n(), 
                      `Mean age` = round(mean(age_months, na.rm = TRUE),2), 
                      `SD age` = round(sd(age_months, na.rm = TRUE),2), 
                      `Median age` = round(median(age_months, na.rm = TRUE), 2))
        }
      } else {
        if (input$cp_subset_kl) {
          kl_table <- filtered_data_kl() %>%
            group_by(language, CP_subset)%>%
            summarise(n = n(), 
                      `Mean age` = round(mean(age_months, na.rm = TRUE),2), 
                      `SD age` = round(sd(age_months, na.rm = TRUE),2), 
                      `Median age` = round(median(age_months, na.rm = TRUE), 2))
        } else {
          kl_table <- filtered_data_kl() %>%
            group_by(language, KL)%>%
            summarise(n = n(), 
                      `Mean age` = round(mean(age_months, na.rm = TRUE),2), 
                      `SD age` = round(sd(age_months, na.rm = TRUE),2), 
                      `Median age` = round(median(age_months, na.rm = TRUE), 2))
        }
      }
      kl_table
    })
    
  ## ---- CITATIONS FOR KL BOXPLOT ----
    output$citations <- renderUI({
      # str1 <- "Please cite the following datasets:"
      
      req(filtered_data_kl())
      
      cites <- filtered_data_kl()%>%
        distinct(cite)
      
      cites_all <- paste(as.vector(unique(cites$cite)), collapse = " <br/><br/>")
      
      str2 <- as.character(cites_all)
      HTML(paste("<b>Please cite:</b> <br/>", str2))
    })
    
    ## ---- DOWNLOADABLE DATA FOR KL ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("KL-data-", Sys.Date(), ".csv", sep="")
      }, 
      content = function(file) {
        write.csv(filtered_data_kl(), file)
      }
     )
  
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
    facet_wrap(~language, scales = "free_x") + 
      guides(color = guide_legend(reverse = TRUE))
  })
  
 ## ... ITEM PLOTS ----
  
  ## .... AVG HISTOGRAM ----
  output$avg_histogram <- renderPlot({
    req(filtered_data_item())
    
    #different dataframes for different plots
    
    #full data
    avg_item <- filtered_data_item() %>%
      group_by(Query, Response)%>%
      summarise(n = n())%>%
      mutate(total.n = sum(n),
             prop = n/total.n)
    #full data - method
    method_df <- filtered_data_item() %>%
      group_by(Query, Response, method)%>%
      summarise(n = n())%>%
      group_by(method)%>%
      mutate(total.n = sum(n),
             prop = n/total.n)
    #full data - kl
    kl_hist <- filtered_data_item() %>%
      group_by(Query, Response, KL)%>%
      summarise(n = n())%>%
      group_by(Query, KL)%>%
      mutate(total.n = sum(n),
             prop = n/total.n)

    #full data - method
    method_df_kl <- filtered_data_item() %>%
      group_by(Query, Response, method, KL)%>%
      summarise(n = n())%>%
      group_by(Query, KL, method)%>%
      mutate(total.n = sum(n),
             prop = n/total.n)
    
  ### Conditional for if KL if method is selected and v.v.
  if (input$kl_selector) {
    if (input$method_choice_item) {
      counts <- method_df_kl %>%
        group_by(Query, KL)%>%
        summarise(full.n = sum(total.n))
      
       p <-  ggplot(method_df_kl, 
        aes(x = Response, y = prop, fill = method)) +
        geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
        geom_bar(stat = 'identity', position = position_dodge(), color = 'black', 
                       binwidth = 1) + 
        scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
        scale_fill_solarized("Method") +
        theme_bw(base_size=14) +
        theme(legend.position = "right",
              panel.grid = element_blank()) +
        labs(y = "Proportion of responses", x = "Number given")+
        facet_grid(KL~Query) + 
        geom_text(data = counts, aes(x = max(method_df_kl$Response - 2), y = .9, label = paste("n = ", full.n)), 
                  size = 5, inherit.aes = FALSE, parse = FALSE)
    } else {
      counts <- kl_hist %>%
        group_by(Query, KL)%>%
        summarise(full.n = sum(total.n))
      
      p <- ggplot(kl_hist, 
                      aes(x = Response, y = prop, fill = as.factor(Query))) + 
        geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
        geom_bar(stat = 'identity', position = position_dodge(), color = 'black', 
                       binwidth = 1) +
        scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
        scale_fill_solarized() +
        theme_bw(base_size=14) +
        theme(legend.position = "none", 
              panel.grid = element_blank()) +
        labs(y = "Proportion of responses", x = "Number given")+
        facet_grid(KL~Query) + 
        geom_text(data = counts, aes(max(method_df_kl$Response - 2), y = .9, label = paste("n = ", full.n)), 
                  size = 5, inherit.aes = FALSE, parse = FALSE)
    }
  } else {
    if (input$method_choice_item) {
      counts <- method_df %>%
        group_by(Query)%>%
        summarise(full.n = sum(total.n))
      
       p <- ggplot(method_df, 
          aes(x = Response, y = prop, fill = method)) +
          geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
          geom_bar(stat = 'identity', position = position_dodge(), color = 'black', 
                       binwidth = 1) +
          scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
          scale_fill_solarized("Method") +
          theme_bw(base_size=14) +
          theme(legend.position = "right",
                panel.grid = element_blank()) +
          labs(y = "Proportion of responses", x = "Number given")+
          facet_grid(~Query) + 
            geom_text(data = counts, aes(max(method_df_kl$Response - 2), y = .9, label = paste("n = ", full.n)), 
                      size = 5, inherit.aes = FALSE, parse = FALSE) 
    } else {
      counts <- avg_item %>%
        group_by(Query)%>%
        summarise(full.n = sum(total.n))
      
      p <- ggplot(filtered_data_item(),
                  aes(x = Response, fill = as.factor(Query))) +
        geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
        geom_histogram(aes(y = ..density..), position = position_dodge(), color = 'black', 
                       binwidth = 1) +
        scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
        scale_fill_solarized() +
        theme_bw(base_size=14) +
        theme(legend.position = "none",
              panel.grid = element_blank()) +
        labs(y = "Density of responses", x = "Number given")+
        facet_wrap(~Query) + 
        geom_text(data = counts, aes(max(method_df_kl$Response - 2), y = .9, label = paste("n = ", full.n)), 
                  size = 5, inherit.aes = FALSE, parse = FALSE) 
    }
  } 
    p
})
  
  ## .... TABLE FOR ITEM HISTOGRAM ----
  
  output$table_item <- renderDataTable({
    #first, if there is kl selected
    if(input$kl_selector) {
      #if method selected
      if(input$method_choice_item) {
        item_table <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query, KL, method)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      } else {
        item_table <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query, KL)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      }
    } else {
      if(input$method_choice_item) {
        item_table <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query, method)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      } else {
        item_table <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      }
    }
    item_table
  })
    
  ## .... LANG HISTOGRAM ----
  output$lang_histogram <- renderPlot({
    req(filtered_data_item())
    
    # lang <- filtered_data_item() %>%
    #   group_by(Query, Response, language)%>%
    #   summarise(n = n())%>%
    #   group_by(Query, language)%>%
    #   mutate(total.n = sum(n), 
    #          prop = n/total.n)
    # 
    # lang_kl <- filtered_data_item() %>%
    #   group_by(Query, Response, language, KL)%>%
    #   summarise(n = n())%>%
    #   group_by(Query, KL, language)%>%
    #   mutate(total.n = sum(n), 
    #          prop = n/total.n)
    
  if (input$kl_selector) {
    p <- ggplot(filtered_data_item(), 
                aes(x = Response, fill = as.factor(language))) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
      geom_histogram(aes(y = ..density..), position = position_dodge(), color = 'black', 
                     binwidth = 1) +
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(base_size=14) +
      theme(legend.position = "top", 
            legend.title = element_blank(),
            panel.grid = element_blank()) +
      labs(y = "Density of responses", x = "Number given")+
      facet_grid(KL~Query, scale = "free_x")
  } else {
    p <- ggplot(filtered_data_item(), 
           aes(x = Response,  fill = as.factor(Query))) + 
      geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
      geom_histogram(aes(y = ..density..), position = position_dodge(), color = 'black', 
                     binwidth = 1) +
      scale_x_continuous(breaks = seq(1, 10, 1)) + #hardcoded, needs to change to reflect max in df
      scale_fill_solarized() +
      theme_bw(base_size=14) +
      theme(legend.position = "none", 
            panel.grid = element_blank()) +
      labs(y = "Density of responses", x = "Number given")+
      facet_grid(language~Query)
  }
    p
  })

  ## .... TABLE FOR ITEM LANGUAGE HISTOGRAM ----
  
  output$table_language_item <- renderDataTable({
    #first, if there is kl selected
    if(input$kl_selector) {
        item_table_language <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query, language, KL)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      } else {
        item_table_language <- filtered_data_item() %>%
          mutate(Query = as.integer(Query))%>%
          group_by(Query, language)%>%
          summarise(n = n(), 
                    `Mean age` = round(mean(age_months, na.rm = TRUE), 2), 
                    `SD age` = round(sd(age_months, na.rm = TRUE), 2), 
                    `Median age` = round(median(age_months, na.rm = TRUE), 2), 
                    `Mean response` = round(mean(Response, na.rm = TRUE), 2), 
                    `SD response` = round(sd(Response, na.rm = TRUE), 2), 
                    `Median response` = round(median(Response, na.rm = TRUE), 2))
      }
    item_table_language
    })
  
  ## ---- DOWNLOADABLE DATA FOR ITEM-level ----
  output$downloadDataItem <- downloadHandler(
    filename = function() {
      paste("item-data-", Sys.Date(), ".csv", sep="")
    }, 
    content = function(file) {
      write.csv(filtered_data_item(), file)
    }
  )
  
  ## ---- CITATIONS FOR ITEM ANALYSES ----
  output$citationsItemAll <- renderUI({
    # str1 <- "Please cite the following datasets:"
    
    req(filtered_data_item())
    
    cites <- filtered_data_item()%>%
      distinct(cite)
    
    cites_all <- paste(as.vector(unique(cites$cite)), collapse = " <br/><br/>")
    
    str2 <- as.character(cites_all)
    HTML(paste("<b>Please cite:</b> <br/>", str2))
  })
  
}












