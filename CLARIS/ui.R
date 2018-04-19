# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(esd)


ci <- c(
  "Mean daily precipiation" = "mdp",
  "Wet-day mean precipitation" = 'mwdm',
  "Wet-day frequency" = "mwdf",
  "Probability of heavy rain" = "rprob",
  "Daily maximum temperature"="tmax",
  "Tmax anomaly"="tmaxa",
  "Daily minimum temperature"="tmin",
  "Tmin anomaly"="tmina")

sea <- c('Annual (All seasons)','Rainy season (October-March)',month.name)

sta <- c("mean","trend","maximum","minimum","range","years")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = 'CLARIS / Climate Mapping tool v0.1'),
  dashboardSidebar(selectInput("ci", "Climate Index", choices= ci),
                   selectInput("thresh", "Threshold value in mm", choices = c(0.1,0.5,1,5,10),selected=1),
                   #textInput("thresh", "Threshold value in mm", value = 0.1),
                   dateRangeInput('dateRange',
                                  label = 'Time period',
                                  start = as.Date('1951-01-01'),
                                  end = as.Date('2006-12-31'),startview = 'year'),
                   h5("Filter by:"), #,
                   selectInput("season", "Season", choices= sea),
                   selectInput("statistic", "Statistic shown in map", choices= sta),
                   selectInput("location", "Location", choices= location)),
  dashboardBody(
#    #include google analytics
#    tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108282573-1'></script>
#            <script>
#              window.dataLayer = window.dataLayer || [];
#              function gtag(){dataLayer.push(arguments);}
#              gtag('js', new Date());
#              gtag('config', 'UA-108282573-1');
#            </script>"
#    )),
#    
#    #send information to google analytics
#    #this includes the event name and the value it is set to
#    #omit sending plotting information (i.e., events starting with .client)
#    tags$script(HTML(
#      "$(document).on('shiny:inputchanged', function(event) {
#            if (event.name.substr(1,6) !== 'client') {
#              newname = event.name+' set to '+event.value;
#              gtag('event', newname, {'event_category': 'User interaction'});
#            }
#            });"
#    )),
    fluidPage( 
      box(title='Station network of stations',status = "primary",collapsed = FALSE, 
          collapsible = TRUE, width="100%", solidHeader = TRUE, 
          leafletOutput("map",height = 500))
    ),
    fluidPage(
              box(title='Time series',status = "primary",collapsed = FALSE, 
                  collapsible = TRUE, width="100%", solidHeader = TRUE, 
                  plotOutput("plotstation", height = 360,width = '100%'))
#    ),
#    fluidPage(p(),
#      box(title='Wet-day amount',status = "primary",collapsed = TRUE, 
#          collapsible = TRUE, width="100%", solidHeader = TRUE, 
#          column(12,
#             plotOutput("plotwdm", height = 360,width = '100%')))
#      ),
#    fluidPage(p(),
#      box(title='Wet-day frequence',status = "primary",collapsed = TRUE, 
#          collapsible = TRUE, width="100%", solidHeader = TRUE, 
#          column(12, 
#             plotOutput("plotwdf", height = 360,width = '100%')))
#      ),
#    fluidPage(p(),
#      box(title='Number of consecutive wet days',status = "primary",
#          collapsed = TRUE, collapsible = TRUE, width="100%", solidHeader = TRUE, 
#          column(12, 
#             plotOutput("plotncwd", height = 360,width = '100%')))
#      ),
#      fluidPage(p(),
#                box(title='Test of the rain equation',status = "primary",
#                    collapsed = TRUE, collapsible = TRUE, width="100%", solidHeader = TRUE, 
#                    column(12,
#                           plotOutput("rainequation", height = 360,width = '100%')))
  )
))



