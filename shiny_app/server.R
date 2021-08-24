# source(here::here("helper.R"))
source("helper.R")


# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  output$uploadContents <- renderDataTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    filetype = str_sub(input$file1$datapath, -4)
    print(input$file1$datapath)
    if(filetype == ".csv"){
      df <- read_csv(input$file1$datapath)
    } else if(filetype == ".sav"){
      df <- read_sav(input$file1$datapath)
    }

    if(input$disp == "head") {
      previewdf <- head(df, 10)
    } else {
      previewdf <- df
    }
    return(previewdf)
    # return(tags$iframe(src=previewdf, width="100%", frameBorder="0", height="1000px"))

  })
  
  ## ----------------------- DATA -----------------------
  
  ## ... KL DATA ----
  
  filtered_data_kl <- eventReactive(input$go_kl, {
    inputAgeMin = input$age_range_kl[1]
    inputAgeMax = input$age_range_kl[2]
    inputKL = input$kl_range_kl
    inputLang = input$language_choice_kl
    inputDat = input$dataset_add_kl
    
    if(input$go_kl == 0){
      inputAgeMin = 22
      inputAgeMax = 144
      inputKL = c("1-knower", "2-knower", "3-knower", "CP-knower")
      inputLang = "English"
      inputDat = unique(all_data$shortCite)
    }
    
    all_data %>%
      distinct(dataset_id, subject_id, age_months, KL, method, language, cite, shortCite, orderCite, CP_subset)%>%
      filter(!is.na(age_months), 
             !is.na(KL),
             age_months >= inputAgeMin,
             age_months <= inputAgeMax,
             if (is.null(inputKL)) KL %in% unique(all_data$KL) else KL %in% inputKL,
             if (is.null(inputLang)) language %in% unique(all_data$language) else language %in% inputLang,
             if (is.null(inputDat)) shortCite %in% unique(all_data$shortCite) else shortCite %in% inputDat)
  }, ignoreNULL=FALSE)
  
  ##cumulative probability 
  ##get age_months, KL, and language points
  cumul_prob <- eventReactive(input$go_kl, {
    
    inputAgeMin = input$age_range_kl[1]
    inputAgeMax = input$age_range_kl[2]
    inputKL = input$kl_range_kl
    inputLang = input$language_choice_kl
    inputDat = input$dataset_add_kl
    
    if(input$go_kl == 0){
      inputAgeMin = defaultParams[[1]]
      inputAgeMax = defaultParams[[2]]
      inputKL = defaultParams[[3]]
      inputLang = defaultParams[[4]]
      inputDat = defaultParams[[5]]
    }
    ### CDF ###
    if(identical(defaultParams,
                 list(inputAgeMin, inputAgeMax, inputKL, inputLang, inputDat))) {
      ns <- n_all
    } else{
      ##static data that will be filtered below for sampling
      ns <- all_data %>%
        filter(!is.na(age_months), 
               !is.na(KL),
               age_months >= inputAgeMin,
               age_months <= inputAgeMax,
               if (is.null(inputKL)) KL %in% unique(all_data$KL) else KL %in% inputKL,
               if (is.null(inputLang)) language %in% unique(all_data$language) else language %in% inputLang,
               if (is.null(inputDat)) shortCite %in% unique(all_data$shortCite) else shortCite %in% inputDat)%>%
        select(KL, language, age_months)
      
      samps <- sample.dat(ns)
      
      #get ns, cumulative sums, and props
      ns %<>% group_by(KL, language, age_months) %>%
        summarise(n = n()) %>%
        mutate(cum.n = cumsum(n),
               prop = cum.n / sum(n))
      
      #left join with samples
      ns <- left_join(ns, samps)
    }
    
    ns %>% 
      dplyr::select(age_months, KL, language, n, cum.n, prop, 
                    ci.low, ci.high)
  }, ignoreNULL=FALSE)
  
  
  
  ## ... ITEM DATA -----
  filtered_data_item <- eventReactive(input$go_item, {
    inputAgeMin = input$age_range_item[1]
    inputAgeMax = input$age_range_item[2]
    inputQuery = input$query_range_item
    inputKL = input$kl_range_item
    inputLang = input$language_choice_item
    inputDat = input$dataset_add_item
    
    if(input$go_item == 0){
      inputAgeMin = 22
      inputAgeMax = 144
      inputQuery = c(1,2,3)
      inputKL = c("1-knower", "2-knower", "3-knower", "CP-knower")
      inputLang = "English"
      inputDat = unique(all_data$shortCite)
    }
    
    all_data %>%
      filter(!is.na(Query),
             !is.na(age_months),
             !is.na(KL),
             age_months >= inputAgeMin,
             age_months <= inputAgeMax,
             Query %in% as.numeric(inputQuery), 
             if (is.null(inputKL)) KL %in% unique(all_data$KL) else KL %in% inputKL,
             if (is.null(inputLang)) language %in% unique(all_data$language) else language %in% inputLang,
             if (is.null(inputDat)) shortCite %in% unique(all_data$shortCite) else shortCite %in% inputDat)
    # method %in% input$method_choice_item)
  }, ignoreNULL=FALSE)
  
  ## ... HIGHEST COUNT DATA -----
  filtered_data_hc <- eventReactive(input$go_hc, {
    inputAgeMin = input$age_range_hc[1]
    inputAgeMax = input$age_range_hc[2]
    inputKL = input$kl_range_hc
    inputLang = input$language_choice_hc
    inputDat = input$dataset_add_hc
    
    if(input$go_hc == 0){
      inputAgeMin = 22
      inputAgeMax = 144
      inputKL = c("1-knower", "2-knower", "3-knower", "CP-knower")
      inputLang = c(unique(subset(all_data, !is.na(highest_count))$language))
      inputDat = unique(all_data$shortCite)
    }
    
    all_data %>%
      filter(!is.na(highest_count),
             !is.na(age_months),
             !is.na(KL),
             age_months >= inputAgeMin,
             age_months <= inputAgeMax,
             if (is.null(inputKL)) KL %in% unique(all_data$KL) else KL %in% inputKL,
             if (is.null(inputLang)) language %in% unique(all_data$language) else language %in% inputLang,
             if (is.null(inputDat)) shortCite %in% unique(all_data$shortCite) else shortCite %in% inputDat) %>%
      distinct(subject_id, dataset_id, age_months, Sex, language, KL, shortCite, highest_count)
    # method %in% input$method_choice_item)
  }, ignoreNULL=FALSE)
  
  
  
  
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
  
  ## ... Highest count selectors####
  output$age_range_selector_hc <- renderUI({
    sliderInput("age_range_hc",
                label = "Ages to include (months):",
                value = c(age_min, age_max),
                step = 1,
                min = age_min, max = age_max)
  })
  
  
  output$language_selector_hc <- renderUI({
    selectInput("language_choice_hc", 
                label = "Languages to include:", 
                choices = languages_hc, 
                selected = languages_hc, 
                multiple = TRUE)
  })
  
  
  output$kl_range_selector_hc <- renderUI({
    selectInput("kl_range_item", 
                label = "Knower levels to include:", 
                choices = kls, 
                selected = c("1-knower", "2-knower", "3-knower", "CP-knower"), 
                multiple = TRUE)
  })
  
  # output$kl_facet_selector <- renderUI({
  #   prettySwitch("kl_selector", # HERE: should this be "kl_range_selector"?
  #                label = "Facet by knower level",
  #                value = FALSE)
  # })
  
  output$dataset_include_selector_hc <- renderUI({
    selectInput("dataset_add_item",
                label = "Datasets to include:",
                choices = all_datasets_short_hc,
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
    req(filtered_data_kl())
    
    cites <- filtered_data_kl()%>%
      distinct(cite, orderCite) %>%
      arrange(orderCite) %>%
      select(cite)
    
    cites_all <- paste(as.vector(unique(cites$cite)), collapse = " <br/><br/>")
    
    str2 <- as.character(cites_all)
    HTML(paste("<b>Please cite:</b> <br/>", str2))
  }, ignoreNULL=FALSE)
  
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
    req(filtered_data_item())
    
    cites <- filtered_data_item()%>%
      distinct(cite, orderCite) %>%
      arrange(orderCite) %>%
      select(cite)
    
    cites_all <- paste(as.vector(unique(cites$cite)), collapse = " <br/><br/>")
    
    str2 <- as.character(cites_all)
    HTML(paste("<b>Please cite:</b> <br/>", str2))
  }, ignoreNULL=FALSE)
  
  
### HIGHEST COUNT PLOTS ----
    
  ## ---- HIGHEST COUNT DOT PLOTS ----
  output$hc_age_language <- renderPlot({
    req(filtered_data_hc())
    
      p <- ggplot(filtered_data_hc(), 
                  aes(x = age_months, y = highest_count, 
                      color = as.factor(language))) + 
        geom_point() + 
        geom_smooth(method = 'lm', se = FALSE) +
        # geom_vline(aes(xintercept = Query), linetype = "dashed", color = 'black') +
        # geom_histogram(aes(y = ..density..), position = position_dodge(), color = 'black', 
        #                binwidth = 1) +
        scale_y_continuous(breaks = seq(0, 150, 10), 
                           limits = c(0, 150)) + 
        # scale_fill_brewer(palette = "Dark2") +
        scale_color_solarized() +
        theme_bw(base_size=14) +
        theme(legend.position = "right", 
              legend.title = element_blank(),
              panel.grid = element_blank()) +
        labs(y = "Highest count", x = "Age") + 
        coord_cartesian()
    p
  })

  ## ---- ALL CITATIONS ----
  output$citationsAll <- renderUI({
    
    cites_all <- paste(all_datasets_full$htmlTxt, collapse = " <br/><br/>")
    
    str2 <- paste(as.character(cites_all), "<br><br>")
    HTML(str2)
  })
}

# shinyApp(ui=ui, server=server)
