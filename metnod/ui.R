## See comments and explanations in global.R
## Rasmus Benestad
# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(esd)


ci <- c(1:length(varids)); names(ci) <- varids

sea <- c('All year'='all','December-February'='DJF',
         'March-May'='MAM','June-August'='JJA','September-November'='SON')
thresholds <- seq(10,50,by=10)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = 'MET Norway / Climate record explorer v0.2'),
  dashboardSidebar(
                   selectInput("ci", "Climate Index", choices= ci),
                   selectInput("statistic", "Statistic shown in map", choices= c(stattype,'Expected # X > x'),selected='mean'),
                   selectInput("location", "Location", choices= Y$location, selected='Oslo - blind'),
                   selectInput("aspect", "Aspect", choices= c("original","anomaly"),selected = 1),
                   h5("Filter by:"), 
                   dateRangeInput('dateRange',
                                  label = 'Time period',
                                  start = as.Date('1951-01-01'),
                                   end = date(),startview = 'year'),
                   selectInput("season", "Season", choices= sea),
                   selectInput("thresh", "Threshold value in mm", choices = thresholds,selected=3)
                   ),
  dashboardBody(
    fluidPage( 
      box(title='Station network',status = "success",collapsed = FALSE, 
          collapsible = TRUE, width="100%", solidHeader = TRUE, 
          leafletOutput("map",height = 500))
    ),
    fluidPage(
              box(title='Time series (past weather)',status = "success",collapsed = FALSE, 
                  collapsible = TRUE, width="100%", solidHeader = TRUE, 
                  plotOutput("plotstation", height = 360,width = '100%'))
  ),
  fluidPage(
    box(title='Statistical distribution (past climate)',status = "success",collapsed = TRUE, 
        collapsible = TRUE, width="100%", solidHeader = TRUE, 
        plotOutput("histstation", height = 360,width = '100%'))
  )
))



