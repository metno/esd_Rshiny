# Load libraries
library(shiny)
library(leaflet)
library(esd)

yrs <- year(x)
fval <- function(x) apply(x,2,function(x,yrs=yrs) min(yrs[is.finite(x)]),yrs)
lval <- function(x) apply(x,2,function(x,yrs=yrs) max(yrs[is.finite(x)]),yrs)

#locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)
#locations <- select.station(param=c('tmax','t2m.9am','precip'),lon=c(27,37),lat=c(-40,-10),src='ghcnd',nmin=30)
#data("meta"); locations <- meta
load('data/precip.inam.rda')
load('data/tmax.inam.rda')
load('data/t2m.9am.inam.rda')
ns <- dim(precip.inam)[2]+dim(tmax.inam)[2]+dim(t2m.9am.inam)[2]
locations <- data.frame(station_id=c(stid(precip.inam),stid(tmax.inam),stid(t2m.9am.inam)),
                        locations=c(loc(precip.inam),loc(tmax.inam),loc(t2m.9am.inam)),
                        country=rep('Mosambique',ns),
                        longitude=c(lon(precip.inam),lon(tmax.inam),lon(t2m.9am.inam)),
                        latitude=c(lat(precip.inam),lat(tmax.inam),lat(t2m.9am.inam)),
                        altitude=c(alt(precip.inam),alt(tmax.inam),alt(t2m.9am.inam)),
                        element=rep(NA,ns),
                        start=c(fval(precip.inam),fval(tmax.inam),fval(t2m.9am.inam)),
                        end=c(lval(precip.inam),lval(tmax.inam),lval(t2m.9am.inam)),
                        source=c(src(precip.inam),src(tmax.inam),src(t2m.9am.inam)),
                        wmo=rep(NA,ns),
                        variable=c(varid(precip.inam),varid(tmax.inam),varid(t2m.9am.inam)),
                        type=rep('observations',ns))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- fluidPage(
  leafletOutput("map",height = 800),
  # Shiny versions prior to 0.11 should use class="modal" instead.
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 20, right = 'auto', bottom = "auto",
                width = 330, height = "auto",
                
                h4("MIMOZA-station explorer v0.1"),
          
                #selectInput("location", "Location", levels(factor(locations$location)), selected=locations$location[1]),
                #selectInput("firstyear", "Starting year", levels(factor(locations$start)), selected = "1951"),
                #selectInput("endyear", "Ending year", rev(levels(factor(locations$end))), selected = "2000"),
                selectInput("cvar", "Climate variable or Index", rev(levels(factor(locations$variable))), selected = "precip")
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
                #plotOutput("scatterCollegeIncome", height = 250)
  ),
  
  tags$div(id="cite",
           'Data retrieved from ', tags$em('INAM'), '.'
  ),
  p(),
  actionButton("recalc", "New points")
)

