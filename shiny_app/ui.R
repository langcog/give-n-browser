library(shinythemes)
library(shinyWidgets)
library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(DT)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(title = "Numberbank",
                   theme = "spacelab",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
   # ----------------------------------
   # tab panel 1 - Home
   tabPanel("Home",
            includeHTML("home.html"),
            tags$head(
              tags$link(rel = "stylesheet", 
                        type = "text/css", 
                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
              tags$link(rel = "icon", 
                        type = "image/png", 
                        href = "img/fishwithears.png")
            )
   ),
                   
   tabPanel("Knower levels", fluid = TRUE,
            # Sidebar layout with a input and output definitions
            sidebarLayout(
              sidebarPanel(
                    uiOutput("age_range_selector"),
                    uiOutput("language_selector"),
                    uiOutput("method_selector") , 
                    uiOutput("kl_range_selector"), 
                    uiOutput("dataset_selector"),
                    
                    downloadButton("downloadData", "Download")
                  ),

      mainPanel(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        tabsetPanel(selected = "KL by age, language", 
                    tabPanel("KL by age, language",
                             plotOutput("age_boxplot")), 
                    tabPanel("Table", 
                             dataTableOutput('table')),
                    tabPanel("Datasets",
                             htmlOutput("citations"))
                    ), 
        tabsetPanel(selected = "Cumulative probability of KL", 
                    tabPanel("Cumulative probability of KL",
                             plotOutput("cumulative_prob"))
                    ) 
                )
          )
   ), 
    
    tabPanel("Item visualizations", fluid = TRUE,
             tabPanel("Item visualizations", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("age_range_selector_item"),
                          uiOutput("language_selector_item"),
                          uiOutput("method_selector_item"), 
                          uiOutput("query_range_selector_item"), 
                          uiOutput("kl_range_selector_item"), 
                          uiOutput("kl_facet_selector"), 
                          
                          downloadButton("downloadDataItem", "Download")
                        ),

              mainPanel(
                tags$style(type = "text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabsetPanel(selected = "Averaged responses", 
                            tabPanel("Averaged responses",
                                     plotOutput("avg_histogram")), 
                            tabPanel("Table", 
                                     dataTableOutput('table_item')),
                            tabPanel("Datasets",
                                     htmlOutput("citationsItemAll"))
                            ), 
                tabsetPanel(selected = "Responses by language", 
                            tabPanel("Responses by language",
                                     plotOutput("lang_histogram")), 
                            tabPanel("Table", 
                                     dataTableOutput('table_language_item'))
                            )
              )
          )
        ) 
    ), 
   tabPanel("About Give-N", fluid = TRUE,
            includeHTML("about_given.html"),
            tags$head(
              tags$link(rel = "stylesheet", 
                        type = "text/css", 
                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
              tags$link(rel = "icon", 
                        type = "image/png", 
                        href = "img/fishwithears.png")
            )
        ), 
   tabPanel("Contributors", fluid = TRUE,
            tabPanel("Contributors", fluid = TRUE,
                     mainPanel("Contributors go here")
            )
      ), 
  tabPanel("About Numberbank", fluid = TRUE,
           tabPanel("About Numberbank", fluid = TRUE,
                    includeHTML("about.html")
           )
      )
  )
)
