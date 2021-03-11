# libraries ----
library(shiny)

# ui ----
shinyUI(pageWithSidebar(
  
  # Application title 
  headerPanel("Miles per Gallon"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput(inputId = "variable",
                label = "Variable:",
                choices = list("Cylinders" = "cyl",
                               "Transmission" = "am",
                               "Gears" = "gear")),
    
    checkboxInput(inputId = "outliers",
                  label = "Show outliers",
                  value = FALSE),
    
    # add in sliders:
    
    # integer
    sliderInput(inputId = "integer",
                label = "Integer:",
                min = 0, max = 1000, value = 500),
    
    # decimal
    sliderInput("decimal", "Decimal:",
                min = 0, max = 1, value = 0.5, step = 0.1),
    
    # range
    sliderInput("range", "Range:",
                min = 1, max = 1000, value = c(200,500)),
    
    # currency format, with basic animation
    sliderInput("format", "Custom Format:",
                min = 0, max = 10000, value = 0, step = 2500,
                format = "$#,##0", locale = "us", animate = TRUE),
    
    # animation with loop
    sliderInput("animation", "Looping Animation:",
                min = 1, max = 2000, value = 1, step = 10,
                animate = animationOptions(interval = 100,
                                           loop = TRUE))
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),
    
    plotOutput("mpgPlot"),
    
    # table output
    tableOutput("values")
  )
))