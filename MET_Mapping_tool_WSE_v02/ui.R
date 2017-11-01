# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(esd)


ci <- c(
  "Mean daily precipiation" = "mdp",
  "Mean wet-day precipitation amount" = 'mwdm',
  "Mean wet-day frequency" = "mwdf",
  "Trend in anual means of wet-day amount" = "tawdm",
  "Trend in anual means of wet-day frequency" = "tawdf")

sea <- c('Annual (All seasons)','DJF (December-Januar-February)', 
         'MAM - (March-April-May)', 'JJA (June-July-August)',
         'SON (September - Ocober - November)',month.name)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = 'ECAD / Climate Mapping tool v0.1'),
  dashboardSidebar(selectInput("ci", "Climate Index", ci),
                   selectInput("thresh", "Threshold value in mm", choices = c(0.1,0.5,1,5,10)),
                   #textInput("thresh", "Threshold value in mm", value = 0.1),
                   dateRangeInput('dateRange',
                                  label = 'Time period',
                                  start = as.Date('1981-01-01'),
                                  end = as.Date('2010-12-31'),startview = 'year'),
                   h5("Filter by:"), #,
                   selectInput("season", "Season", sea),
                   selectInput("location", "Location", location)),
  dashboardBody(
    #include google analytics
    tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108282573-1'></script>
            <script>
              window.dataLayer = window.dataLayer || [];
              function gtag(){dataLayer.push(arguments);}
              gtag('js', new Date());
              gtag('config', 'UA-108282573-1');
            </script>"
    )),
    
    #send information to google analytics
    #this includes the event name and the value it is set to
    #omit sending plotting information (i.e., events starting with .client)
    tags$script(HTML(
      "$(document).on('shiny:inputchanged', function(event) {
            if (event.name.substr(1,6) !== 'client') {
              newname = event.name+' set to '+event.value;
              gtag('event', newname, {'event_category': 'User interaction'});
            }
            });"
    )),
    fluidPage(leafletOutput("map",height = 800)),
    fluidPage(h5("Annual mean of Wet-day mean",left = 30),
              plotOutput("plotstation", height = 260,width = '100%')),
    fluidPage(
      column(4,
             h5("Annual mean of Wet-day amount",left = 30),
             plotOutput("plotwdm", height = 260,width = '100%')),
      column(4, h5("Annual mean of Wet-day frequency",left = 30),
             plotOutput("plotwdf", height = 260,width = '100%')),
      column(4, h5("Number of consecutive wet-days",left = 30),
             plotOutput("plotncwd", height = 260,width = '100%')))
    
  )
)



