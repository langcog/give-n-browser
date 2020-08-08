library(shinyBS)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("Numberbank"),
                             # includeMarkdown("../docs/peekbank.md"),
                             value = "title",
                             style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("kl_range_selector"), 
      uiOutput("language_selector")
    ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "Boxplot", 
                  tabPanel("Boxplot",
                           plotOutput("boxplot")
                  )
  
      )
    )
  )
)
