# esd-ex2-app map of cyclone trajectories

library(shiny)
#library(esd)
#source("helpers.R")
#load("data/trajectories.atlantic.rda") 

fluidPage(
  headerPanel('Cyclone budget'),
  sidebarPanel(
    checkboxGroupInput('param', 'Variables to show:', 
                       c("total","system","genesis","lysis",
                         "inN","inE","inS","inW",
                         "outN","outE","outS","outW"),
                       selected=c("total","system","genesis","lysis"),width='85%'),
    dateRangeInput('dates', 'Date range', start=as.Date('2015-01-01'), end=as.Date('2015-12-31'), 
                   min=as.Date('1979-01-01'), max=as.Date('2015-12-31'), 
                   format='yyyy-mm-dd', startview='decade', width='85%'),
    checkboxGroupInput('it', 'Season', list("winter","spring","summer","fall"), 
                       selected=c("winter","spring","summer","fall"), inline=FALSE)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
