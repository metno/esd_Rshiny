# esd-ex2-app map of cyclone trajectories

library(shiny)
#library(esd)
#source("helpers.R")
#load("data/trajectories.hurdat2.rda") 

fluidPage(
  headerPanel('Cyclone trajectories'),
  sidebarPanel(
    selectInput('param', 'Colors', c("Uniform","Maximum wind speed","Pressure",
                                    "ENSO","NAO","Month","Year"),
                selected="Maximum wind speed",width='90%'),
    dateRangeInput('dates', 'Date range', start=as.Date('2016-01-01'), end=as.Date('2016-12-31'), 
                   min=as.Date('1979-01-01'), max=as.Date('2016-12-31'), 
                   format='yyyy-mm-dd', startview='decade', width='90%'),
    checkboxGroupInput('it', 'Season', list("winter","spring","summer","autumn"), 
                       selected="autumn", inline=FALSE)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
