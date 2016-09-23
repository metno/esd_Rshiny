library(shiny)
library(DT)

# Define the overall UI

navbarPage(title = 'eSACP - Climate and Weather Meta data browser (Beta version)', 
           id = 'x0',
           tabPanel('Browse', 
                    h4(textOutput('text')),
                    h4(textOutput('text1')),
                    dataTableOutput('varnames'),
                    h4(textOutput('text2')),
                    DT::dataTableOutput("table")
                    #plotOutput("plot")
           ),
           # tabPanel('Plot', 
           #          #textInput('x2', 'Row ID'), 
           #          plotOutput("plot")
           # ),
           # tabPanel('Locate', 
           #          #textInput('x2', 'Row ID'), 
           #          plotOutput("map")#,width = "auto", height = "auto")
           # ),
           # tabPanel('View data', 
           #          #textInput('x2', 'Row ID'), 
           #          DT::dataTableOutput("selsta",width = 'auto'),
           #          DT::dataTableOutput("rawdata",width = 'auto')
           # ),
           tabPanel('Download', 
                    #textInput('x2', 'Row ID'), 
                    downloadLink('downloadMeta', 'Download Meta'),
                    downloadLink('downloadData', 'Download Data') 
           )
)