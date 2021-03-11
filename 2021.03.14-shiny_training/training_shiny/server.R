# libraries ----
library(shiny)
library(datasets)

# setup ----

# Tweaking the "am" field to have nicer factor labels. Doesn't rely on any user
# inputs, so can do this once at startup then use the value throughout the 
# lifetime of the application.

mpgData <- mtcars
mpgData$am <- factor(mpgData$am,
                     labels = c("Automatic", "Manual"))

# server ----
shinyServer(function(input, output) {
  
  # Compute the formula text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers)
  })
  
  # reactive sliders
  sliderValues <- reactive({
    
    # write dataframe
    data.frame(Name = c("Integer", "Decimal", "Range", "Custom Format", "Animation"),
               Value = as.character(c(input$integer,
                                      input$decimal,
                                      paste(input$range, collapse = " "),
                                      input$format,
                                      input$animation)),
               stringsAsFactors = FALSE)
  })
  
  # outputs
  output$values <- renderTable({
    sliderValues()
  })
  
})