# libraries ----
library(shiny)
library(datasets)

# setup ----


# server ----
shinyServer(function(input, output) {
  
  # Change distribution
  data <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Plot of the data
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(),
         main = paste("r", dist, "(", n, ")", sep = ""))
  })
  
  # summary
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate HTML table view
  output$table <- renderTable({
    data.frame(x = data())
  }) 
  
})