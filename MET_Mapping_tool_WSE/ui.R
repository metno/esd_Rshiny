# Load libraries
library(shiny)
library(leaflet)
library(esd)

locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

ui <- fluidPage(
  leafletOutput("map",height = 800),
  # Shiny versions prior to 0.11 should use class="modal" instead.
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 20, right = 'auto', bottom = "auto",
                width = 330, height = "auto",
                
                h4("MET-station explorer v0.1"),
                
                selectInput("location", "Location", locations$location),
                selectInput("firstyear", "Starting year", levels(factor(locations$start)), selected = "adultpop"),
                selectInput("endyear", "Ending year", rev(levels(factor(locations$end))), selected = "adultpop"),
                selectInput("cvar", "Climate variable or Index", rev(levels(factor(locations$variable))), selected = "adultpop")
  ),
  
  p(),
  absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, #top = 850, left = 20, right = 20, bottom = 200,
                width = '100%', height = 260,
                
                h5("Explore a station or select one from the map (click) to display the time series",left = 30),
                
                #selectInput("selectLoc", "Location", locations$location),
                #conditionalPanel("Min. Number of recorded years == 'superzip' || input.size == 'superzip'",
                #                 # Only prompt for threshold when coloring or sizing by superzip
                #                 numericInput("nmin", "Min. Number of recorded years", 30)
                #),
                
                plotOutput("plotstation", height = 260,width = '100%'),
                plotOutput("scatterCollegeIncome", height = 250)
  ),
  
  tags$div(id="cite",
           'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
  ),
  p(),
  actionButton("recalc", "New points")
)

