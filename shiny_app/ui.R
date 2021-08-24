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

# shinyUI(navbarPage())
ui = navbarPage(title = "Numberbank",
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
                           tags$link(rel = "stylesheet",
                                     type = "text/css",
                                     href = "css/site.css"),
                           tags$link(rel = "icon",
                                     type = "image/png",
                                     href = "img/fishwithears.png")
                         )
                         # tags$style(type = "text/css",
                         #            ".metaCol { float:left; font-size:24px; width:50%; text-align:center; }"),
                         # tags$div(class="metaCol", htmlOutput("metaLangText"))
                         
                ),
                
                tabPanel("Knower levels", value = 'tabKL', fluid = TRUE,
                         # Sidebar layout with a input and output definitions
                         sidebarLayout(
                           sidebarPanel(
                             uiOutput("age_range_selector"),
                             uiOutput("language_selector"),
                             uiOutput("method_selector") , 
                             uiOutput("kl_range_selector"), 
                             uiOutput("cp_subset_selector"),
                             uiOutput("dataset_include_selector"),
                             actionButton("go_kl", "Go"),
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
                                      uiOutput("dataset_include_selector_item"),
                                      actionButton("go_item", "Go"),
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
                
                tabPanel("Highest count", value = 'tabHC', fluid = TRUE,
                         # Sidebar layout with a input and output definitions
                         sidebarLayout(
                           sidebarPanel(
                             uiOutput("age_range_selector_hc"),
                             uiOutput("language_selector_hc"),
                             uiOutput("kl_range_selector_hc"),
                             # uiOutput("cp_subset_selector"),
                             uiOutput("dataset_include_selector_hc"),
                             actionButton("go_hc", "Go"),
                             downloadButton("downloadData_hc", "Download")
                           ),
                           
                           mainPanel(
                             tags$style(type = "text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"),
                             tabsetPanel(selected = "Highest count by age", 
                                         tabPanel("Highest count by age",
                                                  plotOutput("hc_age_language")), 
                                         tabPanel("Table", 
                                                  dataTableOutput('table_hc')),
                                         tabPanel("Datasets",
                                                  htmlOutput("citationsHCAll"))
                             ),
                             tabsetPanel(selected = "Highest count by language", 
                                         tabPanel("Highest count by language",
                                                  plotOutput("hc_density")), 
                                         tabPanel("Table", 
                                                  dataTableOutput('table_language_hc'))
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
                                  htmlOutput("citationsAll"),
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css", href = "css/contributors.css"),
                                    tags$script(src = "js/contributorFunctions.js")
                                  )
                         )
                ), 
                tabPanel("Submit Data", fluid = TRUE,
                         tabPanel("Submit Data", fluid = TRUE,
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput("file1", "Choose File",
                                                multiple = TRUE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv",
                                                           ".sav")),

                                      # Horizontal line ----
                                      tags$hr(),

                                      # Input: Checkbox if file has header ----
                                      checkboxInput("header", "Header", TRUE),

                                      # Input: Select separator ----
                                      radioButtons("sep", "Separator",
                                                   choices = c(Comma = ",",
                                                               Semicolon = ";",
                                                               Tab = "\t"),
                                                   selected = ","),

                                      # Input: Select quotes ----
                                      radioButtons("quote", "Quote",
                                                   choices = c(None = "",
                                                               "Double Quote" = '"',
                                                               "Single Quote" = "'"),
                                                   selected = '"'),

                                      # Horizontal line ----
                                      tags$hr(),

                                      # Input: Select number of rows to display ----
                                      radioButtons("disp", "Display",
                                                   choices = c(Head = "head",
                                                               All = "all"),
                                                   selected = "head")
                                    ),

                                    mainPanel(
                                      # tags$head(tags$style(HTML('.shiny-html-output table td {width: 400px}'))),
                                      # tags$iframe(HTML('.shiny-html-output table td {width: 400px}'),
                                      #             width="100%",frameBorder="0",height="1000px")
                                      # tableOutput("uploadContents")
                                      
                                      div(dataTableOutput("uploadContents"), style = "font-size: 75%; width: 75%;"),
                                      tags$style(type = "text/css",
                                                 ".shiny-table { width: 75%; }")
                                    )
                                  )
                         )
                ),
                tabPanel("About Numberbank", fluid = TRUE,
                         tabPanel("About Numberbank", fluid = TRUE,
                                  includeHTML("about.html")
                         )
                )
)

# shinyApp(ui=ui, server=server)
# rsconnect::deployApp(appName="shiny_app", account="numberbank")