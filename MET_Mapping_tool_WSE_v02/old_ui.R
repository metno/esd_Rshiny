# Load libraries
library(shiny)
library(leaflet)
library(esd)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ci <- c(
  "Mean daily precipiation" = "mdp",
  "Mean wet-day precipitation amount" = 'mwdm',
  "Mean wet-day frequency" = "mwdf",
  "Trend in anual means of wet-day amount" = "tawdm",
  "Trend in anual means of wet-day frequency" = "tawdf")

sea <- c('Annual (All seasons)','DJF (December-Januar-February)', 'MAM - (March-April-May)', 'JJA (June-July-August)','SON (September - Ocober - November)')
ui <- fluidPage(
  leafletOutput("map",height = 800),
  # Shiny versions prior to 0.11 should use class="modal" instead.
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 20, right = 'auto', bottom = "auto",
                width = 330, height = "auto",
                
                h4("ECAD / Climate Mapping tool v0.1"),
                
                selectInput("ci", "Climate Index", ci),
                
                dateRangeInput('dateRange',
                               label = 'Time period',
                               start = as.Date('1981-01-01'), end = as.Date('2010-12-31'),startview = 'year'),
                
                h5("Filter by:"), #,
                selectInput("season", "Season", sea),
                selectInput("location", "Location", location)
                #selectInput("firstyear", "Starting year", levels(factor(locations$start)), selected = "adultpop"),
                #selectInput("endyear", "Ending year", rev(levels(factor(locations$end))), selected = "adultpop")
                
  ),
  
  p(),
  absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, #top = 850, left = 20, right = 20, bottom = 200,
                width = '100%', height = 260,
                
                h5("Explore a station from the sidebar or select one from the map (click) to display the time series",left = 30),
                
                #selectInput("selectLoc", "Location", locations$location),
                #conditionalPanel("Min. Number of recorded years == 'superzip' || input.size == 'superzip'",
                #                 # Only prompt for threshold when coloring or sizing by superzip
                #                 numericInput("nmin", "Min. Number of recorded years", 30)
                #),
                
                plotOutput("plotstation", height = 260,width = '100%')
  ),
  p(),
  absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = '39%', left = '69%', right = 'auto', bottom = 'auto',
                width = '30%', height = '30%',
                
                h5("Number of available stations (Default)",left = 30),
                
                #selectInput("selectLoc", "Location", locations$location),
                #conditionalPanel("Min. Number of recorded years == 'superzip' || input.size == 'superzip'",
                #                 # Only prompt for threshold when coloring or sizing by superzip
                #                 numericInput("nmin", "Min. Number of recorded years", 30)
                #),
                
                plotOutput("plotavailstation", height = 260,width = '100%')
  ),
  
  tags$div(id="cite",
           'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
  ),
  p(),
  actionButton("recalc", "New points")
)

