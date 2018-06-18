# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(esd)


ci <- c(
  "Precipiation" = "precip",
  "Daily mean temperature" = 't2m',
  "Daily maximum temperature" = "tmax",
  "Daily minimum temperature"="tmin")

sea <- c('All','djf','mam','jja','son',month.name)
sta <- names(meta)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = 'MET / Climate Mapping tool v0.1'),
  dashboardSidebar(
                   selectInput("ci", "Climate Index", choices= ci),
                   selectInput("statistic", "Statistic shown in map", choices= sta),
                   selectInput("location", "Location", choices= locations),
                   selectInput("aspect", "Aspect", choices= c("original","anomaly"),selected = 1),
                   h5("Filter by:"), 
                   dateRangeInput('dateRange',
                                  label = 'Time period',
                                  start = as.Date('1951-01-01'),
                                   end = date(),startview = 'year'),
                   selectInput("season", "Season", choices= sea),
                   selectInput("thresh", "Threshold value in mm", choices = c(0.1,0.5,1,5,10),selected=3)
                   ),
  dashboardBody(
    

    fluidPage( 
      box(title='Station network',status = "primary",collapsed = FALSE, 
          collapsible = TRUE, width="100%", solidHeader = TRUE, 
          leafletOutput("map",height = 500))
    ),
    fluidPage(
              box(title='Time series (past weather)',status = "primary",collapsed = FALSE, 
                  collapsible = TRUE, width="100%", solidHeader = TRUE, 
                  plotOutput("plotstation", height = 360,width = '100%'))
  ),
  fluidPage(
    box(title='Statistical distribution (past climate)',status = "primary",collapsed = TRUE, 
        collapsible = TRUE, width="100%", solidHeader = TRUE, 
        plotOutput("histstation", height = 360,width = '100%'))
  )
))



