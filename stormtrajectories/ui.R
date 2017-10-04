# esd-ex2-app map of cyclone trajectories

library(shiny)
#library(esd)
#source("helpers.R")
#load("data/trajectories.atlantic.rda") 

fluidPage(
  headerPanel('Cyclone trajectories'),
  sidebarPanel(
    selectInput('param', 'Colors', c("Uniform","Maximum wind speed","Pressure","Radius",
                                    "NAO","AMO","Month","Year"),
                selected="NAO",width='90%'),
    dateRangeInput('dates', 'Date range', start=as.Date('2015-12-01'), end=as.Date('2015-12-31'), 
                   min=as.Date('1979-01-01'), max=as.Date('2015-12-31'), 
                   format='yyyy-mm-dd', startview='decade', width='90%'),
    checkboxGroupInput('it', 'Season', list("winter","spring","summer","fall"), 
                       selected="winter", inline=FALSE)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
