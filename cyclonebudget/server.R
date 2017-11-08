# esd-ex2-app map of cyclone trajectories

library(shiny)
library(esd)
source("helpers.R")
load("data/trajectories.atlantic.rda") 

shinyServer(function(input, output) {
  
  #sz <- reactive({
  #  figsize(input$param)
  #})
 
  # hgt <- as.numeric(sz()$height)
  # wdh <- as.numeric(sz()$width)
  nr <- reactive({
    ceiling(length(input$param)/3)
  })
  
  nc <- reactive({
    ceiling(length(input$param)/nr())
  })
  
  selectedCyclones <- reactive({
    calculate.cyclonebudget(select.cyclones(Z, dates=input$dates, it=input$it))
  })
  
  output$plot1 <- renderPlot({
    plot.cyclonebudget(selectedCyclones(),budnames=input$param,
                       xlim=c(-60,60),ylim=c(30,80),new=FALSE)
   }, height=function(){200*as.numeric(nr())}, 
      width=function(){200*as.numeric(nc())})
  
})