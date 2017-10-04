# esd-ex2-app map of cyclone trajectories

library(shiny)
library(esd)
source("helpers.R")
load("data/trajectories.atlantic.rda") 

shinyServer(function(input, output) {
  
  selectedParam <- reactive({
    switch.param(input$param)
  })
  
  selectedCyclones <- reactive({ 
    select.cyclones(Z, dates=input$dates, it=input$it)
  })
  
  output$plot1 <- renderPlot({
    map.fancy(selectedCyclones(), param=selectedParam())
   }, height=450, width=500)
})
