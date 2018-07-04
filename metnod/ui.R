## See comments and explanations in global.R
## Rasmus Benestad
# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(esd)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
###----------------------------------

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = textOutput("maintitle")),
  dashboardSidebar(
    selectInput("ci", "Climate Index", choices= ci),
    selectInput("lingo", "Language", 
                choices= languages,selected='English')),
  dashboardBody(
    fluidPage( 
      box(title=textOutput("maptitle"),status = "success",collapsed = FALSE, 
          collapsible = TRUE, width="100%", solidHeader = TRUE, 
          column(9,leafletOutput("map",height = 700)),
          column(3,selectInput("location", textOutput("location.label"), 
                               choices= Y$location, selected=first.location),
                 selectInput("statistic", textOutput("statistics.label"), 
                             choices= stats,selected='mean'),
                 selectInput("season", textOutput("season.label"), choices= sea),
                 selectInput("higlight", lab.highlight, choices= higlighting),
                 conditionalPanel(condition="input.statistic == 'specific day'",
                                  dateInput("it",textOutput("specific.date"), 0)))
      )
    ),
    fluidPage(
      box(title=textOutput("tstitle"),status = "success",collapsed = TRUE,
          collapsible = TRUE, width="100%", solidHeader = TRUE,
          column(9, plotlyOutput("plotstation", height = 500,width = '100%')),
          column(3,
                 selectInput("tscale", textOutput("timescale.label"), choices= tscales,selected = tscales[1]),
                 selectInput("aspect", textOutput("aspect.label"), choices= aspects,selected = aspects[1]),
                 selectInput("highlightTS", lab.highlight, choices= higlighting),
                 # conditionalPanel(condition=c("input.ci == '1'","input.tscale == 'month'",
                 #                              "input.tscale == 'season'","input.tscale == 'year'"),
                 #                  selectInput("aspect", "Aspect", choices= aspectsP,selected = aspectsP[1])),
                 # conditionalPanel(condition="input.ci == '2'",
                 #                  selectInput("aspect", "Aspect", choices= aspectsT,selected = aspectsT[1])),
                 # conditionalPanel(condition="input.ci == '3'",
                 #                  selectInput("aspect", "Aspect", choices= aspectsT,selected = aspectsT[1])),
                 # conditionalPanel(condition="input.ci == '4'",
                 #                  selectInput("aspect", "Aspect", choices= aspectsT,selected = aspectsT[1])),
                 conditionalPanel(condition="input.aspect == 'number of days'",
                                  numericInput("x0",textOutput("threshold"), 0)) #,
                 #selectInput("seasonTS", textOutput("season.label"), choices= seaTS,selected=seaTS[1]),
                 ## Only show threshold if number of days
                 #selectInput("thresh", "Threshold value in mm", choices = thresholds,selected=3)
          ))
    ),
  fluidPage(
    box(title=textOutput("htitle"),status = "success",collapsed = TRUE,
        collapsible = TRUE, width="100%", solidHeader = TRUE,
        column(9, plotlyOutput("histstation", height = 500,width = '100%')),
        column(3,
               selectInput("timespace", textOutput("timespace.label"), choices= timespace,selected = timespace[1]),
               dateRangeInput('dateRange',
                              label = textOutput("timeperiod.label"),
                              start = as.Date('1951-01-01'),
                              end = date(),startview = 'year')
               ## Only show threshold if number of days
               #selectInput("thresh", "Threshold value in mm", choices = thresholds,selected=3)
        ))
  ),
  fluidPage(
    box(title=textOutput("cftitle"),status = "success",collapsed = TRUE, 
        collapsible = TRUE, width="100%", solidHeader = TRUE, 
        tags$div(class="header", checked=NA,
                 tags$p("Do you have any comments or feedbacks? If so, please respond through this"),
                 tags$a(href="https://goo.gl/forms/GuzqO1GIUFfz5L2K2","link")
        )
    ))
))



