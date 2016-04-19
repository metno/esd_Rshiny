library(shiny)
library(esd)

# ui.R

shinyUI(fluidPage(
  titlePanel("Winter temperature in the Barents region"),
  
  fluidRow(
    column(12,
      h1("MET KlimaLab"),
      h2("Climate atlas for the Barents region"),
      p("This page presents results from empirical-statistical downscaling - also referred to as the abbreviation 'ESD' - described in the paper Benestad et al., ERL-102170.R2 (accepted)."),
      p("The results presented here repersent large multi-model ensembles from CMIP5 experiment, and include the RCP 2.6, 4.5, and 8.5 emission scenarios."),
      p("The results have been gridded through kriging and with the elveation as a covariate."),
      br(),
      div("This R-shiny app lets you look at the statistics of an ensemble, highlight individual model runs, and pick a time period. You can view the actual values or differences between time or scenarios.", style = "color:blue")
    )),
   fluidRow(
    column(2,
           selectInput(h4("Elements"), 
                        label = "Choose a variable to display",
                        choices = c("Temperature", "Precipitation"),
                        selected = "Temperature")
                              ),
    
    column(2, 
           checkboxGroupInput("checkGroup", 
                              label = h4("Season"), 
                              choices = list("Winter" = 1, 
                                             "Spring" = 2, 
                                             "Summer" = 3,
                                             "Autumn" = 4),
                              selected = 1)),
    
    column(2, 
           checkboxGroupInput("checkGroup", 
                              label = h4("Scenario"), 
                              choices = list("RCP4.5" = 1, 
                                             "RCP2.6" = 2, 
                                             "RCP8.5" = 3),
                              selected = 1)),
    column(2,
           checkboxGroupInput("checkGroup", 
                              label = h4("Show"), 
                              choices = list("Actual value" = 1, 
                                             "Change" = 2, 
                                             "Scenario difference" = 3),
                              selected = 1)),
    column(2,
           checkboxGroupInput("checkGroup", 
                              label = h4("What"), 
                              choices = list("Ensemble mean" = 1, 
                                             "95-percentile" = 2, 
                                             "5-percentile" = 3,
                                             "Realisation" = 4),
                              selected = 1))
  ),
  fluidRow(
    
    column(3,
           sliderInput("range", 
                       label = "Time slice:",
                       min = 1900, max = 2100, value = c(1900, 2100))),
    
    
    column(3, 
           numericInput("Modelrun", 
                        label = h4("Simulation #"), 
                        value = 1))
  ),
  mainPanel(
    plotOutput("map")
  ))
)


