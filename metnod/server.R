library(shiny)
library(esd)
library(leaflet)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

# Define a server for the Shiny app
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    #cbind(longitude, latitude)
    cbind(lon(y),lat(y))
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    station_id <- stid(y)
    print(station_id)
    ID <- which(station_id == event$id)
    print('observeEvent'); print(ID)
    

    updateSelectInput(session,inputId = 'location',
                      choices = toupper(locatins))  
    #print(paste('Updated ',toupper(loc(y))))
  })
  
  # Computing indices
  vals <- reactive({
    
    Y <- switch(input$ci,
                'mdp'=precip.stats,'mwdm'=precip.stats,'mwdf'=preci.stats,'rprob'=precip.stats,
                'tmax'=tmax.stats,'tmin'=tmin.stats,'tmaxa'=tmax.stats,'tmina'=tmin.stats)
    locations <- Y$loc
    station_id <- Y$stid
    startyr <- Y$firstyear
    endyr <- Y$lastyear
    Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))
    print(paste('Y$',input$statistic,sep=''))
    return(Z) 
    })
  
  
  ## The map panel 
  output$map <- renderLeaflet({
    
    Y <- switch(input$ci,
                'precip'=precip.stats,'tmax'=tmax.stats,'tmin'=tmin.stats,'t2m'=t2m.stats)
    pal <- colorBin(colscal(col = 't2m',n=100,rev = is.precip(y)[1]),
                    vals(),bins = 10,pretty = TRUE)
    locations <- Y$loc
    station_id <- Y$stid
    startyr <- Y$firstyear
    endyr <- Y$lastyear
    
    statistic <- switch(input$statistic,
                        'mean' = paste('mean (',unit(y),')',sep=''),
                        'trend' = paste('trend (',unit(y),'/decade)',sep=''),
                        'year' = 'year')
    #print(statistic)
    
    leaflet() %>% 
      addCircleMarkers(lng = Y$lon, # longitude
                       lat = Y$lat,fill = TRUE, # latitude
                       label = as.character(round(vals(),digits = 2)),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = location,popupOptions(keepInView = TRUE),
                       radius =7,stroke=TRUE,weight = 1, color='black',
                       layerId = station_id,
                       fillOpacity = 0.4,fillColor=pal(vals())) %>% 
      addLegend("bottomleft", pal=pal, values=round(vals(), digits = 2), 
                title=statistic,
                layerId="colorLegend") %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat=median(Y$lat),lng = median(Y$lon), zoom = 6)
  })
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    station_id <- stid(y)
    #print(station_id)
    ID <- which(station_id == stid)
   # print(c(loc(y)[ID],location[ID],firstyear(y)[ID],startyr[ID],endyr[ID],ID,stid))

    selLon <- as.numeric(levels(factor(round(lon(y),digits = 2))))
    selLat <- as.numeric(levels(factor(round(lat(y),digits = 2))))
    selAlt <- as.numeric(levels(factor(alt(y))))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location[ID]))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), paste(' LAT ',selLat,'N',sep=''), paste(' ALT ',selAlt,'m',sep=''))), tags$br(),
                     sprintf("Station ID: %s", as.character(stid(y))),
                     sprintf("Parameter: %s", paste(toupper(varid(y)),collapse = ',')),
                     sprintf("Start year: %s", paste(startyr[ID],collapse = ',')),
                     sprintf("End year: %s", paste(endyr[ID],collapse = ',')),
                     sprintf("Data provider: %s", paste(attr(y,'source'),collapse = ',')))
    
    leafletProxy("map") %>% addPopups(lng, lat, content,layerId = stid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    #browser()
    #print('--->'); print(event); print('<---')
    if (is.null(event))
      return()
    isolate({
      showMetaPopup(stid=event$id,lat=event$lat, lng = event$lng)
    })
    
    #removeMarker("map",layerId = event$id)
    leafletProxy("map",data = event) %>% 
      addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
    
  })
  
  # ## Data Explorer ###########################################
  # 
  observe({
    Y <- switch(input$ci,
                'precip'=precip.stats,'tmax'=tmax.stats,'tmin'=tmin.stats,'t2m'=t2m.stats)
    fname <- switch(input$ci,
                    'precip'='data/precip.metnod.nc','tmax'='data/tmax.metnod.nc',
                    'tmin'='data/tmin.metnod.nc','t2m'='data/t2m.metnod.nc')
    pal <- colorBin(colscal(col = 't2m',n=100,rev = is.precip(y)[1]),
                    vals(),bins = 10,pretty = TRUE)
    locations <- Y$loc
    station_id <- Y$stid
    startyr <- Y$firstyear
    endyr <- Y$lastyear
    
    print(location)
    ID <- which(tolower(input$location) == tolower(location))
    #print(station_id)
    #print('Data Explorer'); print(ID); print(input$location)
    if (is.null(ID)) ID <- 1
    selectedStid <- station_id[which(tolower(input$location) == tolower(location))]
    
    y <- retrieve.station(fname,stid=selectedStid)
    y <- subset(y,it = input$dateRange)
    if (input$aspect =='anomaly') y <- anomaly(y)
    
    if (substr(input$season,1,6) != 'All')
          y <- esd::subset.station(y,it = tolower(substr(input$season,1,3))) 
    
    leafletProxy("map",data = y) %>% clearPopups() %>% 
      addCircles(lng = lon(y), lat = lat(y), color = 'red', layerId = 'selectID', weight = 12)
    isolate({
      showMetaPopup(stid=stid(y),lat=lat(y), lng = lon(y))
    })
    
    timeseries <- switch(input$ci,
    'mean'=y,'stdv'=y,
    'mu'=annual(y, FUN='wetmean', nmin=30, threshold = thresh()),
    'fw'=annual(y, FUN='wetfreq', nmin=30, threshold = thresh()),
    'max'=y,
    'min'=y,
    'rprob'=NULL)
    
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   })
      #browser()
      if (is.null(timeseries)) esd::test.rainequation(y,x0=30) else {
        esd::plot.station(timeseries, map.show = FALSE, new=FALSE,main=loc(timeseries),
                          ylab = paste(varid(timeseries),' (',unit(timeseries),')',sep=''))
        lines(trend(timeseries),lty=2)
        grid()
      }
    })
  
  
  output$histstation <- renderPlot({
    withProgress(message = 'Updating ...',
                 detail = 'This may take a while...', value = 0,
                 { for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
                 })
    if (is.null(timeseries)) esd::test.rainequation(y,x0=30) else {
      hist(timeseries, main=loc(timeseries),col='grey',freq=FALSE,
           xlab = paste(varid(timeseries),' (',unit(timeseries),')',sep=''))
      grid()
    }
  })
  
  })
  
  thresh <- reactive({
    return(as.numeric(input$thresh))
  })
  
}
