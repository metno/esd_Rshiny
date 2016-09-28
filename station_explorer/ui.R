
library(shiny)
if (!require("DT")) install.packages('DT')
library(DT)
# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(esd)

selsta <- select.station()
var.names <- c('Mean temperature','Minimum temperature','Maximum temperature',
               'Precipitation','Sunshine','Lowest minimum temperature','Day of Tl',
               'Relative humidity','Mean Pressure','Number of days with snow cover 50 covered',
               'Wind speed','Wind direction','Wind Gust','Maximum 1 day precipitation','Cloud cover',
               'Snow depth','Snowfall')

# Define the overall UI
navbarPage(title = 'Weather station explorer (Beta version)', 
           id = 'x0',
           tabPanel('Browse', 
                    h4(textOutput('text')),
                    h3(textOutput('text1')),
                    dataTableOutput('varnames'),
                    h3(textOutput('text2')),
                    DT::dataTableOutput("table")
                    #plotOutput("plot")
           ),
           tabPanel('Plot', 
                    #textInput('x2', 'Row ID'), 
                    plotOutput("plot")
           ),
           tabPanel('Locate', 
                    #textInput('x2', 'Row ID'), 
                    plotOutput("map")#,width = "auto", height = "auto")
           ),
           tabPanel('View data', 
                    #textInput('x2', 'Row ID'), 
                    DT::dataTableOutput("selsta",width = 'auto'),
                    DT::dataTableOutput("rawdata",width = 'auto')
           ),
           tabPanel('Download', 
                    #textInput('x2', 'Row ID'), 
                    downloadLink('downloadMeta', 'Download Meta'),
                    downloadLink('downloadData', 'Download Data') 
           )
)

# shinyUI(
# fluidPage(
#   titlePanel("ESD - Station Explorer"),
#   #sidebarLayout(
#   #sidebarPanel(
  #   #    actionButton("newplot", "New plot")
  #   #),
  #   # Create a new Row in the UI for selectInputs
  #   #mianPanel(
  #     fluidRow(
  #       column(width=4,
  #              downloadLink('downloadMeta', 'Download Meta') 
  #       ),
  #       column(width=4,
  #              downloadLink('downloadData', 'Download Data') 
  #       ),
  #       plotOutput("map"),
  #       # # Copy the line below to make a slider range 
  #       #  column(width=4,
  #       #         sliderInput("period", label = h3("Temporal coverage"), min = as.integer(1950), 
  #       #              max = as.integer(2020), step=1, value = as.integer(c(1970, 2010)))
  #       #  )
  #    # ),
  #     # Create a new row for the table.
  #   #  mainPanel(
  #       #fluidRow(
  #         DT::dataTableOutput("table"),
  #       #)#),
  #     #mainPanel(
  #       DT::dataTableOutput("selectedrow"),
  #       plotOutput("plot")
  #       
  #     #)
  #   )
  # )
  #)
