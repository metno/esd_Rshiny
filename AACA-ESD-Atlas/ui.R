library(shiny)
library(esd)

# ui.R

#shinyUI(fluidPage(
#  titlePanel("Downscaled temperature in the Barents region"),
#  
#  fluidRow(
#    column(12,
#      h1("MET Klimasandkassen"),
#      h2("Climate atlas for the Barents region"),
#      p("This page presents results from empirical-statistical downscaling - also referred to as the abbreviation 'ESD' - described in the paper Benestad et al., ERL-102170.R2 (accepted)."),
#      p("The results presented here repersent large multi-model ensembles from CMIP5 experiment, and include the RCP 2.6, 4.5, and 8.5 emission scenarios."),
#      p("The results have been gridded through kriging and with the elveation as a covariate."),
#      br(),
#      div("This R-shiny app lets you look at the statistics of an ensemble, highlight individual model runs, and pick a time period. You can view the actual values or differences between time or scenarios.", style = "color:blue")
#    )),
#   fluidRow(


#    column(1, 
#           checkboxGroupInput("Modelrun", 
#                             label = h4("Simulation"), 
#                             choices = names(Z[[1]])[-c(1,2)],
#                             selected=1:108))
#    ),
#         
#  fluidRow(
#    


#  ),
#  mainPanel(
#    plotOutput("map")
#  ))
#)

# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution


navbarPage("AACA Atlas",
    tabPanel("Ensemble statistics", 
             plotOutput("map"),
             column(3,
                   sliderInput("lon", 
                                    label = "Longitudes:",
                                    min = 0, max = 50, value = c(0, 50))),
             column(3,
                        sliderInput("lat", 
                                    label = "Latitudes:",
                                    min = 65, max = 85, value = c(65, 85))),
             column(3,
                        dateRangeInput("dates", label = h3("time slice"), start='2090-01-01',end='2099-12-31'))
             ),
    tabPanel("Locations", 
             column(2,
                    selectInput(h4("Location"), 
                                label = "location",
                                choices = names(z),
                                selected = "longyearbyen")
             ),
             plotOutput("plot")),
    tabPanel("Settings", 
             column(2,
                    selectInput(h4("Element"), 
                                            label = "Variable",
                                            choices = c("Temperature", "Precipitation"),
                                            selected = "Temperature")
                    ),
                        
             column(2, 
                    selectInput(h4("Season"), 
                                           label = "Season",
                                           choices = c("Winter", "Spring","Summer","Autumn"),
                                           selected = "Winter")
                    ),
                      
            column(2, 
                   selectInput(h4("Scenario"), 
                                            label = "Scenario", 
                                            choices = c("RCP4.5","RCP2.6", "RCP8.5"),
                                            selected = "RCP4.5")),
            column(3,
                       dateRangeInput(h4("Baseline"), label = "baseline", start='1980-01-01',end='2010-21-31')),
            column(2,
                       selectInput(h4("Aspect"), 
                                   label = "Aspect",
                                   choices = c("actual value", "change","scenario difference"),
                                   selected = "Actual value")
            ),
                column(2,
                       selectInput(h4("Statistic"), 
                                   label = "Statistic",
                                   choices = c("Ensemble mean", "95-percentile","5-percentile","Individual model",
                                               "Ensemble spread","Model difference"),
                                   selected = "Ensemble mean")
            ),
            column(4, 
                   checkboxGroupInput(h4("Modelrun"), 
                                      label = "Model", 
                                      choices = gcmnames,
                                      selected=gcmnames)),
  
             
            tableOutput("table"))
)
