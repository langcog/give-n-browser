library(shinyBS)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  # bsCollapse(id = "doc", open = "title",
  #            bsCollapsePanel(title = h3("Numberbank"),
  #                            # includeMarkdown("../docs/peekbank.md"),
  #                            value = "title",
  #                            style = "default")
  # ),
  
  navbarPage("Numberbank", theme = shinytheme("lumen"),
             tabPanel("Knower levels", fluid = TRUE,
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                              uiOutput("age_range_selector"),
                              uiOutput("language_selector"),
                              uiOutput("method_selector") , 
                              uiOutput("kl_range_selector")
                            ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "KL by age, language", 
                  tabPanel("KL by age, language",
                           plotOutput("age_boxplot")), 
                  tabPanel("KL by age, language, method",
                           plotOutput("method_boxplot"))
  
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
                          uiOutput("query_range_selector_item")
                        ),

              mainPanel(
                tags$style(type = "text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabsetPanel(selected = "Averaged responses", 
                            tabPanel("Averaged responses",
                                     plotOutput("avg_histogram")) 
                            ), 
                tabsetPanel(selected = "Responses by language", 
                            tabPanel("Responses by language",
                                     plotOutput("lang_histogram"))
                            )
              )
          )
        )
    )
  )
)