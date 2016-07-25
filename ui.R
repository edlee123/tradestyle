library(ggvis)
library(magrittr)
library(dplyr)

list.files(path = "./R", full.names = T) %>% lapply(., FUN = source)

gpdat = readRDS("./data/gpdat.rds")
models = readRDS("./data/models2.rds")

shinyUI(
  navbarPage("TradeStyle",
     tabPanel("Performance Metrics",
        mainPanel(
          h4("Portfolio Returns"),
          selectizeInput(
            'multichart', 'Select Portfolios:', selected = names(models)[1],
            choices = names(models), multiple = TRUE
          ),
          fluidRow(
            column(12, align="center",
                   plotOutput("perf_chart", width = "100%"))
          ),br(),
          h4("Historic Metrics"),
          fluidRow(
            column(12, align="center",
                   DT::dataTableOutput("perf_metrics"))
          ),br(),br(),
          h4("Calendar Returns"),
          selectInput("portfolio", "Select Portfolio:",
                      names(models)),
          DT::dataTableOutput("calret_table"),
          br(), br()
        ) 
     ),
     tabPanel("Style Analysis",
        mainPanel(
          h4("Rolling Coefficients (Carhart Factors)"),
           selectInput("portfoliostyle", "Select Portfolio:",
                       names(models)),
          plotOutput("style_attrib"), br(), br()
         ),
        sidebarPanel(
          p("The rolling regression is based on the Carhart factor model:"),
          uiOutput("factorlist")
        )
     ),
#      tabPanel("Try Your Own!", mainPanel(
#        p("You may analyze your own portfolios. Upload NAVs in csv format, with columns 'Date, NAV'")
#      )
#      ),
#                
    tabPanel("Indicators",
      mainPanel(
        p("Experiment with ggvis and shiny with date slider and clickability."),
         ggvisOutput("plot1"),
         sliderInput("year", "Start / End", gpdat$dt %>% min , 
                     gpdat$dt %>% max, 
                     value = c(gpdat$dt %>% min , gpdat$dt %>% max)))
      ),
  
    tabPanel("About",
            fluidRow(
              column(8,
                     includeMarkdown("include.md")
              )
            )
    )
  )
)