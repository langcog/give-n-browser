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
  arrange(orderCite)
all_datasets_full <- all_datasets$cite
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


unique(all_data$cite)


# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  ## ----------------------- DATA -----------------------
  
  ## ... KL DATA ----
  
  filtered_data_kl <- eventReactive(input$go_kl, {
    all_data %>%
          distinct(dataset_id, subject_id, age_months, KL, method, language, cite, shortCite, orderCite, CP_subset)%>%
          filter(!is.na(KL),
                 !is.na(age_months),
                 age_months >= input$age_range_kl[1],
                 age_months <= input$age_range_kl[2],
                 if (is.null(input$kl_range_kl)) KL %in% unique(all_data$KL) else KL %in% input$kl_range_kl,
                 if (is.null(input$language_choice_kl)) language %in% unique(all_data$language) else language %in% input$language_choice_kl,
                 if (is.null(input$dataset_add_kl)) shortCite %in% unique(all_data$shortCite) else shortCite %in% input$dataset_add_kl)
  })
  
    ##cumulative probability 
    ##get age_months, KL, and language points
    cumul_prob <- eventReactive(input$go_kl, {
      
      ### CDF ###
      ##static data that will be filtered below for sampling
      ns <- all_data %>%
        filter(!is.na(age_months), 
               !is.na(KL),
               age_months >= input$age_range_kl[1],
               age_months <= input$age_range_kl[2],
               if (is.null(input$kl_range_kl)) KL %in% unique(all_data$KL) else KL %in% input$kl_range_kl,
               if (is.null(input$language_choice_kl)) language %in% unique(all_data$language) else language %in% input$language_choice_kl,
               if (is.null(input$dataset_add_kl)) shortCite %in% unique(all_data$shortCite) else shortCite %in% input$dataset_add_kl)%>%
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
      
      ns %>% 
        dplyr::select(age_months, KL, language, n, cum.n, prop, 
                  ci.low, ci.high)
    })
  
  
  
  ## ... ITEM DATA -----
  filtered_data_item <- eventReactive(input$go_item, {
    all_data %>%
      filter(!is.na(Query),
             !is.na(age_months),
             !is.na(KL),
             age_months >= input$age_range_item[1],
             age_months <= input$age_range_item[2],
             Query %in% as.numeric(input$query_range_item), 
             if (is.null(input$kl_range_item)) KL %in% unique(all_data$KL) else KL %in% input$kl_range_item,
             if (is.null(input$language_choice_item)) language %in% unique(all_data$language) else language %in% input$language_choice_item,
             if (is.null(input$dataset_add_item)) shortCite %in% unique(all_data$shortCite) else shortCite %in% input$dataset_add_item)
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
  
  output$dataset_include_selector <- renderUI({
    selectInput("dataset_add_kl",
                label = "Datasets to include:",
                choices = all_datasets_short,
                #selected = as.list(y),
                multiple = TRUE)
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
    
    output$kl_range_selector_item <- renderUI({
      selectInput("kl_range_item", 
                  label = "Knower levels to include:", 
                  choices = kls, 
                  selected = c("1-knower", "2-knower", "3-knower", "CP-knower"), 
                  multiple = TRUE)
    })
    
    output$kl_facet_selector <- renderUI({
      prettySwitch("kl_selector", # HERE: should this be "kl_range_selector"?
                   label = "Facet by knower level",
                   value = FALSE)
    })
    
    output$dataset_include_selector_item <- renderUI({
      selectInput("dataset_add_item",
                  label = "Datasets to include:",
                  choices = all_datasets_short,
                  #selected = as.list(y),
                  multiple = TRUE)
    })
    
    

  
  ## ----------------------- PLOTS -----------------------
  
  ## ... KL PLOTS ----
  ## ---------- BOXPLOT OF KL AND AGE BY LANGUAGE
  output$age_boxplot <- renderPlot({ # HERE: figure out why this is not working
    req(filtered_data_kl())
    
    # LAO: streamlining boxplot; replacing old ifelse statement
    p <- filtered_data_kl()
    
    if(input$cp_subset_kl){ #check if grouping by CP vs non-CP knower
      p <- p %>% ggplot(aes(x = language, y=age_months, fill = CP_subset, color = CP_subset)) +
        scale_fill_solarized("CP-/Subset-knower", rev) +
        scale_color_solarized("CP-/Subset-knower") +
        guides(fill = guide_legend(reverse = FALSE))
    } else{
      p <- p %>% ggplot(aes(x = language, y=age_months, fill = KL, color = KL)) +
        scale_fill_solarized("Knower level") +
        scale_color_solarized("Knower level") +
        guides(fill = guide_legend(reverse = TRUE))
    }
    
    p <- p + geom_boxplot(alpha = .7,
                   color = "black") +
      geom_point(position = position_jitterdodge(jitter.width=0.1), alpha=0.35, #LAO: removed dodge.width = 0.79
                 show.legend = FALSE)+
      theme_bw(base_size=14) +
      labs(x = 'Language',
           y = "Age (months)") +
      coord_flip()

    if(input$method_choice_kl){ #facet_wrap by titrated vs not-titrated
      p <- p + facet_grid(~method)
    }
    
    p
    
  })
    
  ## ----- TABLE FOR KL BOXPLOT ----
    output$table <- renderDataTable({
      
      # LAO: streamlining; replacing old ifelse statement
      kl_table <- filtered_data_kl() %>%
        group_by(language)
      
      if(input$method_choice_kl){
        kl_table <- kl_table %>% group_by(method)
      }
      
      if (input$cp_subset_kl){
        kl_table <- kl_table %>% group_by(CP_subset)
      } else{
        kl_table <- kl_table %>% group_by(KL)
      }
      
      kl_table <- kl_table %>%
        summarise(n = n(),
                  `Mean age` = round(mean(age_months, na.rm = TRUE),2),
                  `SD age` = round(sd(age_months, na.rm = TRUE),2),
                  `Median age` = round(median(age_months, na.rm = TRUE), 2))
      
      kl_table
      
    })
    
  ## ---- CITATIONS FOR KL BOXPLOT ----
    output$citations <- eventReactive(input$go_kl, {
      # str1 <- "Please cite the following datasets:"
      
      req(filtered_data_kl())
      
      cites <- filtered_data_kl()%>%
        distinct(cite, orderCite) %>%
        arrange(orderCite) %>%
        select(cite)
      
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
  output$citationsItemAll <- eventReactive(input$go_item, {
    # str1 <- "Please cite the following datasets:"
    
    req(filtered_data_item())

    cites <- filtered_data_item()%>%
      distinct(cite, orderCite) %>%
      arrange(orderCite) %>%
      select(cite)
    
    cites_all <- paste(as.vector(unique(cites$cite)), collapse = " <br/><br/>")
    
    str2 <- as.character(cites_all)
    HTML(paste("<b>Please cite:</b> <br/>", str2))
  })
  
  
  ## ---- ALL CITATIONS ----
  output$citationsAll <- renderUI({
    
    cites_all <- paste(all_datasets_full, collapse = " <br/><br/>")
    
    str2 <- as.character(cites_all)
    HTML(str2)
  })
}












