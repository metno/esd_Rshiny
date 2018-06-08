library(shiny)
library(esd)
library(leaflet)

yrs <- year(x)
fval <- function(x) apply(x,2,function(x,yrs=yrs) min(yrs[is.finite(x)]),yrs)
lval <- function(x) apply(x,2,function(x,yrs=yrs) max(yrs[is.finite(x)]),yrs)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

#locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)
#locations <- select.station(param=c('tmax','t2m.9am','precip'),lon=c(27,37),lat=c(-40,-10),src='ghcnd',cntr='Mozambique',nmin=30)
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


# Define a server for the Shiny app
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(locations$longitude, locations$latitude)
  }, ignoreNULL = FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addCircleMarkers(weight = 1,radius = 6,
                       lng = locations$longitude,
                       lat = locations$latitude,
                       layerId = locations$station_id) %>% #
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      #setView(lat=60,lng = 10, zoom = 5)
      setView(lat=-17,lng = 25, zoom = 5)
  })
  
  sellocation <- reactive({
    # If no zipcodes are in view, don't plot
    
    if (!is.null(input$location) == 0)
      return(NULL)
    print()
  })
  
  #observe({
  #  leafletProxy("map", data = locations) %>%
  #    clearShapes() %>%
  #    #addCircleMarkers(locations$longitude, locations$latitude)
  #    addMarkers(locations$longitude, locations$latitude)
  #})
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    selectedLoc <- locations[locations$station_id == stid,]
    print(selectedLoc)
    # content <- as.character(tagList(
    #   tags$h4(paste(toupper(selectedLoc$location),' (',selectedLoc$country,')',sep='')),
    #   tags$strong(HTML(paste('LON: ',selectedLoc$longitude,'W',sep=''), paste('/ LAT: ',selectedLoc$latitude,'N',sep=''), paste('/ ALT: ',selectedLoc$altitude,'m',sep=''))), tags$br(),
    #   sprintf("Station ID: %s", as.numeric(selectedLoc$station_id)), tags$br(),
    #   sprintf("Parameter: %s", toupper(selectedLoc$variable)), tags$br(),
    #   sprintf("Start year: %s", selectedLoc$start), tags$br(),
    #   sprintf("End year: %s", selectedLoc$end), tags$br(),
    #   sprintf("Data provider: %s", selectedLoc$source)))
    # 
    #ftplink <- file.path('ftp://ftp.met.no/projects/chasepl/test/data.METNOD',paste('RR_',sprintf("%05d", as.numeric(stid)),'.dly',sep=''))
    #ftplink <- file.path('ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily')
    #browser()
    selLon <- as.numeric(levels(factor(selectedLoc$longitude)))
    selLat <- as.numeric(levels(factor(selectedLoc$latitude)))
    selAlt <- as.numeric(levels(factor(selectedLoc$altitude)))
    content <- paste(sep = "<br/>",
                     #paste(paste("<b><a href='",ftplink,"'>",sep=''),toupper(selectedLoc$location),"</a></b>",sep=''),
                     paste("<b><a href='",toupper(selectedLoc$location),"</a></b>",sep=''),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), paste(' LAT ',selLat,'N',sep=''), paste(' ALT ',selAlt,'m',sep=''))), tags$br(),
                     sprintf("Station ID: %s", as.numeric(selectedLoc$station_id)),
                     sprintf("Parameter: %s", paste(toupper(selectedLoc$variable),collapse = ',')),
                     sprintf("Start year: %s", paste(selectedLoc$start,collapse = ',')),
                     sprintf("End year: %s", paste(selectedLoc$end,collapse = ',')),
                     sprintf("Data provider: %s", paste(selectedLoc$source,collapse = ',')))
    
    leafletProxy("map") %>% addPopups(lng, lat, content,layerId = stid)
    print(stid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    print(event)
    if (is.null(event))
      return()
    isolate({
      showMetaPopup(stid=event$id,lat=event$lat, lng = event$lng)
    })
    
    #removeMarker("map",layerId = event$id)
    leafletProxy("map",data = event) %>% 
      addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
    
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0, 
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   })
      # If no zipcodes are in view, don't plot
      if (!is.null(event$id) == 0)
        return(NULL)
      #load('Y.rda')
      Y <- switch(input$cvar,'precip'=precip.inam,'tmax'=tmax.inam,'t2m.9am'=t2m.9am.inam)
      y <- subset(Y,is=list(stid=event$id))
      #y <- try(esd::station(stid = event$id,param=c('tmax','tmin','precip'),src='ghcnd'))
      
      if (grepl(pattern = 'error',x = class(y),ignore.case = TRUE))
        return(NULL)
      
      esd::plot.station(y, map.show = FALSE, new=FALSE,plot.type='mult',ylab = varid(y))
    })
    
  })
  
  ## Data Explorer ###########################################
  
  observe({
    #selectedLoc <- input$location
    Loc <- locations[which(tolower(input$location) == tolower(locations$location)),]
    selectedStid <- locations$station_id[which(tolower(input$location) == tolower(locations$location))]
    
    leafletProxy("map",data = selectedStid) %>% clearPopups() %>% 
      addCircles(lng = Loc$longitude, lat = Loc$latitude,color = 'red',layerId = 'selectID', weight = 12)
    isolate({
      showMetaPopup(stid=selectedStid,lat=Loc$latitude, lng = Loc$longitude)
    })
    
    #y <- try(esd::station(stid = selectedStid,param=c('tmax','tmin','precip'),src='ghcnd'))
    
    if (sum(grepl(pattern = 'error',x = class(y),ignore.case = TRUE)) >0)
      return(NULL)
    else
      ysub <- esd::subset.station(y,is = selectedStid, it=as.numeric(c(input$firstyear,input$endyear)))
    
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0, 
                   { for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                   }
                   })
      # If no zipcodes are in view, don't plot
      if (!is.null(selectedStid) == 0)
        return(NULL)
      #y <- try(esd::station(stid = selectedStid,param=c('tmax','tmin','precip'),src='ghcnd'))
      y <- switch(input$cvar,'precip'=precip.inam,'tmax'=tmax.inam,'t2m.9am'=t2m.inam)
      is <- is.element(loc(y),input$location)
      ysub <- esd::subset(y,is=is)

      #if (sum(grepl(pattern = 'error',x = class(y),ignore.case = TRUE)) >0)
      #  return(NULL)
      #else
      #  ysub <- esd::subset.station(y,is = 1:length(c('t2m','precip')), it=as.numeric(c(input$firstyear,input$endyear)))

      if (grepl(pattern = 'error',x = class(ysub),ignore.case = TRUE))
        return(NULL)
      else 
        esd::plot.station(ysub, map.show = FALSE, new=FALSE,plot.type='mult',ylab = varid(y))
    })
  })
  
}
