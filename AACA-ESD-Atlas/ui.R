library(shiny)
library(esd)

# ui.R

shinyUI(fluidPage(
  titlePanel("Winter temperature in the Barents region"),
  
#  fluidRow(
#    column(12,
#      h1("MET KlimaLab"),
#      h2("Climate atlas for the Barents region"),
#      p("This page presents results from empirical-statistical downscaling - also referred to as the abbreviation 'ESD' - described in the paper Benestad et al., ERL-102170.R2 (accepted)."),
#      p("The results presented here repersent large multi-model ensembles from CMIP5 experiment, and include the RCP 2.6, 4.5, and 8.5 emission scenarios."),
#      p("The results have been gridded through kriging and with the elveation as a covariate."),
#      br(),
#      div("This R-shiny app lets you look at the statistics of an ensemble, highlight individual model runs, and pick a time period. You can view the actual values or differences between time or scenarios.", style = "color:blue")
#    )),
   fluidRow(
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
           checkboxGroupInput("checkGroup", 
                              label = h4("Scenario"), 
                              choices = list("RCP4.5" = 1, 
                                             "RCP2.6" = 2, 
                                             "RCP8.5" = 3),
                              selected = 1)),
    column(2,
           selectInput(h4("Aspect"), 
                       label = "Aspect",
                       choices = c("actual value", "change","scenario difference"),
                       selected = "Actual value")
    ),
    column(2,
           selectInput(h4("Statistic"), 
                       label = "Statistic",
                       choices = c("Ensemble mean", "95-percentile","5-percentile","Individual model"),
                       selected = "Ensemble mean"),
           
    column(1, 
          numericInput("Modelrun", 
                      label = h4("Simulation"), 
                      value = 1))
    )
    ),
         
  fluidRow(
    
    column(3,
           sliderInput("lon", 
                       label = "longitude region:",
                       min = 0, max = 50, value = c(0, 50))),
    column(3,
           sliderInput("lat", 
                       label = "latitude region:",
                       min = 65, max = 85, value = c(65, 85))),
    column(3,
           dateRangeInput("dates", label = h3("time slice"), start='2090-01-01',end='2099-12-31')),
    column(3,
           dateRangeInput("reference", label = h3("baseline", start='1980-01-01',end='2010-21-31')))
  ),
  mainPanel(
    plotOutput("map")
  ))
)


