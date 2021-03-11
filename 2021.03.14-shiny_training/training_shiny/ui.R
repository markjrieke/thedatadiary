# libraries ----
library(shiny)

# ui ----
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tabsets"),
  
  # Sidebar controls
  sidebarPanel(
    radioButtons("dist", "Distribution Type:",
                 list("Normal" = "norm",
                      "Uniform" = "unif",
                      "Log-normal" = "lnorm",
                      "Exponential" = "exp")),
    
    br(),
    
    sliderInput("n", "Number of Observations:",
                min = 1, max = 1000, value = 500)
  ),
  
  # Main panel broken up by tabsets
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("table"))
    )
  )
))