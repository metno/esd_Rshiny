library(shiny)
library(esd)
load("dse.aaca.t2m.rcp45.djf.rda")
Y <- as.station(dse.aaca.t2m.rcp45)

shinyServer(function(input, output) {
  
  function(input, output) {
    
    output$map <- renderPlot({ 
      plot(rnorm(10),main=paste("You have selected", input$Element))
    })
    
  }
})
