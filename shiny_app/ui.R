library(shinythemes)
library(shinyWidgets)
library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(bslib)
library(shinybusy)
library(shinycssloaders)
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
                                     href = "css/customtheme.css"),
                           tags$link(rel = "stylesheet",
                                     type = "text/css",
                                     href = "css/site.css"),
                           tags$link(rel = "icon",
                                     type = "image/png",
                                     href = "img/fishwithears.png")#,
                           # tags$style(HTML("
                           #            .dropdown-toggle { color: white; background-color: #A5C581; }
                           #            "))
                         )
                ),
                
                navbarMenu("Give-N",
                  tabPanel("Knower Levels", value = 'tabKL', fluid = TRUE,
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
                               # add_busy_spinner(spin = "bounce", position = "full-page"), #trinity-rings, semipolar, circle, bounce
                               tags$style(type = "text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"),
                               tabsetPanel(selected = "KL by age, language", 
                                           tabPanel("KL by age, language",
                                                    plotOutput("age_boxplot") %>% withSpinner(color="#A5C581")
                                                    ), 
                                           tabPanel("Table", 
                                                    dataTableOutput('table')),
                                           tabPanel("Datasets",
                                                    htmlOutput("citations"))
                               ), 
                               tabsetPanel(selected = "Cumulative probability of KL", 
                                           tabPanel("Cumulative probability of KL",
                                                    plotOutput("cumulative_prob") %>% withSpinner(color="#A5C581")
                                                    )
                               ) 
                             )
                           )
                  ), 
                  
                  tabPanel("Item Visualizations", fluid = TRUE,
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
                                        # add_busy_spinner(spin = "bounce", position = "full-page"),
                                        tags$style(type = "text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"),
                                        tabsetPanel(selected = "Averaged responses", 
                                                    tabPanel("Averaged responses",
                                                             plotOutput("avg_histogram") %>% withSpinner(color="#A5C581")
                                                             ), 
                                                    tabPanel("Table", 
                                                             dataTableOutput('table_item')),
                                                    tabPanel("Datasets",
                                                             htmlOutput("citationsItemAll"))
                                        ), 
                                        tabsetPanel(selected = "Responses by language", 
                                                    tabPanel("Responses by language",
                                                             plotOutput("lang_histogram") %>% withSpinner(color="#A5C581")
                                                             ), 
                                                    tabPanel("Table", 
                                                             dataTableOutput('table_language_item'))
                                        )
                                      )
                                    )
                           ) 
                  )
                ),
                
                tabPanel("Highest Count", value = 'tabHC', fluid = TRUE,
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
                             # add_busy_spinner(spin = "bounce", position = "full-page"),
                             tags$style(type = "text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"),
                             tabsetPanel(selected = "Highest count by age", 
                                         tabPanel("Highest count by age",
                                                  plotOutput("hc_age_language") %>% withSpinner(color="#A5C581")
                                                  ), 
                                         tabPanel("Table", 
                                                  dataTableOutput('table_hc')),
                                         tabPanel("Datasets",
                                                  htmlOutput("citationsHCAll"))
                             ),
                             tabsetPanel(selected = "Highest count by language", 
                                         tabPanel("Highest count by language",
                                                  plotOutput("hc_density") %>% withSpinner(color="#A5C581")
                                                  )
                             )
                           )
                         )
                ), 
                tabPanel("Methods", fluid = TRUE,
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
                                  includeHTML("about.html"),
                                  htmlOutput("citationsAll"),
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css", href = "css/contributors.css"),
                                    tags$script(src = "js/contributorFunctions.js")
                                  )
                         )
                )
                # ,
                # tabPanel("Submit Data", fluid = TRUE,
                #          tabPanel("Submit Data", fluid = TRUE,
                #                   sidebarLayout(
                #                     sidebarPanel(
                #                       fileInput("file1", "Choose File",
                #                                 multiple = TRUE,
                #                                 accept = c("text/csv",
                #                                            "text/comma-separated-values,text/plain",
                #                                            ".csv",
                #                                            ".sav")),
                #                       uiOutput("addDat_header"),
                #                       uiOutput("addDat_separator"),
                #                       uiOutput("addDat_quote"),
                #                       actionButton("convert", "Convert"),
                #                       downloadButton("downloadConvert", "Download")
                #                     ),
                # 
                #                     mainPanel(
                #                       # actionButton("reset", "Reset"),
                #                       DT::dataTableOutput("uploadContents"),
                #                       DT::dataTableOutput("convertContents") #,
                #                       # tags$style(type = "text/css",
                #                       #            ".shiny-table { width: 75%; overflow-x: auto; }")
                #                     )
                #                   )
                #          )
                # )
)

# shinyApp(ui=ui, server=server)
# rsconnect::deployApp(appName="shiny_app", account="numberbank")