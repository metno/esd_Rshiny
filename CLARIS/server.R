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
    
    selectedLoc <- subset.station(y, is = ID) 
   
    updateSelectInput(session,inputId = 'location',
                      choices = toupper(esd::loc(selectedLoc)))  
    #print(paste('Updated ',toupper(loc(y))))
  })
  
  # Computing indices
  vals <- reactive({
    
    y <- switch(input$ci,
                'mdp'=pre,'mwdm'=pre,'mwdf'=pre,'rprob'=pre,
                'tmax'=tmax,'tmin'=tmin,'tmaxa'=tmax,'tmina'=tmin)
    location <- loc(y)
    station_id <- stid(y)
    startyr <- as.numeric(firstyear(y))
    endyr <- as.numeric(lastyear(y))
    y <- esd::subset.station(y, it = input$dateRange)
    ## Allow for plotting anomalies:
    #if (sum(is.element(input$ci,c('tmaxa','tmina')))>0) y <- esd::anomaly(y)
    if (substr(input$season,1,6) != 'Annual')
      y <- esd::subset.station(y, it = tolower(substr(input$season,1,3)))
    
    # mean daily precipitation
    if ( (input$statistic == 'mean') &  (sum(is.element(input$ci,c('mdp','rprob')))>0) )
      return(apply(annual(y,FUN='sum',nmin=30),2,FUN='mean',na.rm=TRUE)) else
    if ( (input$statistic == 'mean') &  (sum(is.element(input$ci,c('tmax','tmin','tmaxa','tmina')))>0) )
          return(apply(annual(y,FUN='mean',nmin=30),2,FUN='mean',na.rm=TRUE)) else
    if ( (input$statistic == 'trend') &  (input$ci == 'mdp') )
      return(apply(annual(y,FUN='sum',nmin=30),2,FUN='trend.coef',na.rm=TRUE)) else
    if ( (input$statistic == 'mean') &  (input$ci == 'mwdm') )
      return(apply(annual(y,FUN='wetmean',nmin=30,threshold = thresh()),2,FUN='mean',na.rm=TRUE)) else
    if ( (input$statistic == 'trend') &  (input$ci == 'mwdm') )
      return(apply(annual(y,FUN='wetmean',nmin=30,threshold = thresh()),2,FUN='tend.coef',na.rm=TRUE)) else
    if ( (input$statistic == 'mean') &  (input$ci == 'mwdf') )
      return(apply(annual(y,FUN='wetfreq',nmin=30,threshold = thresh()),2,FUN='mean',na.rm=TRUE)) else
    if ( (input$statistic == 'trend') &  (input$ci == 'mwdf') )
      return(apply(annual(y,FUN='wetfreq',nmin=30,threshold = thresh()),2,FUN='tend.coef',na.rm=TRUE)) else
    if (input$statistic == 'years') 
      return(lastyear(y) - firstyear(y) + 1) else 
    if (input$statistic == 'maximum') 
          return(apply(coredata(y),2,FUN='max',na.rm=TRUE)) else 
    if (input$statistic == 'minimum') 
          return(apply(coredata(y),2,FUN='min',na.rm=TRUE)) else  
    if (input$statistic == 'range') 
          return(apply(coredata(y),2,FUN='max',na.rm=TRUE) - apply(coredata(y),2,FUN='min',na.rm=TRUE))
    })
  
  
  ## The map panel 
  output$map <- renderLeaflet({
    pal <- colorBin(colscal(col = 't2m',n=100,rev = TRUE),vals(),bins = 10,pretty = TRUE)
    
    ##REB add temperature
    y <- switch(input$ci,
                'mdp'=pre,'mwdm'=pre,'mwdf'=pre,'rprob'=pre,
                'tmax'=tmax,'tmin'=tmin,'tmaxa'=tmax,'tmina'=tmin)
    location <- loc(y)
    station_id <- stid(y)
    startyr <- as.numeric(firstyear(y))
    endyr <- as.numeric(lastyear(y))
    
    ## BER
    statistic <- switch(input$statistic,
                        'mean' = paste('mean (',unit(y)[1],')',sep=''),
                        'trend' = paste('trend (',unit(y)[1],'/decade)',sep=''),
                        'year' = 'year')
    
    leaflet() %>% 
      addCircleMarkers(lng = lon(y), # longitude
                       lat = lat(y),fill = TRUE, # latitude
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
      setView(lat=median(lat(y)),lng = median(lon(y)), zoom = 6)
  })
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    station_id <- stid(y)
    #print(station_id)
    ID <- which(station_id == stid)
    selectedLoc <- subset.station(y, is = ID) 
   # print(c(loc(y)[ID],location[ID],firstyear(y)[ID],startyr[ID],endyr[ID],ID,stid))

    selLon <- as.numeric(levels(factor(round(lon(selectedLoc),digits = 2))))
    selLat <- as.numeric(levels(factor(round(lat(selectedLoc),digits = 2))))
    selAlt <- as.numeric(levels(factor(alt(selectedLoc))))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location[ID]))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), paste(' LAT ',selLat,'N',sep=''), paste(' ALT ',selAlt,'m',sep=''))), tags$br(),
                     sprintf("Station ID: %s", as.character(stid(selectedLoc))),
                     sprintf("Parameter: %s", paste(toupper(varid(selectedLoc)),collapse = ',')),
                     sprintf("Start year: %s", paste(startyr[ID],collapse = ',')),
                     sprintf("End year: %s", paste(endyr[ID],collapse = ',')),
                     sprintf("Data provider: %s", paste(attr(selectedLoc,'source'),collapse = ',')))
    
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
    #selectedLoc <- input$location
    y <- switch(input$ci,
                'mdp'=pre,'mwdm'=pre,'mwdf'=pre,'rprob'=pre,
                'tmax'=tmax,'tmin'=tmin,'tmaxa'=tmax,'tmina'=tmin)
    location <- loc(y)
    station_id <- stid(y)
    startyr <- as.numeric(firstyear(y))
    endyr <- as.numeric(lastyear(y))
    
    print(location)
    ID <- which(tolower(input$location) == tolower(location))
    #print(station_id)
    #print('Data Explorer'); print(ID); print(input$location)
    if (is.null(ID)) ID <- 1
    #selectedStid <- station_id[which(tolower(input$location) == tolower(location))]
    
    ## Allow for plotting anomalies:
    #if (sum(is.element(input$ci,c('tmaxa','tmina')))>0) y <- esd::anomaly(y)
    
    ## Select one station
    selectedLoc <- subset(y,is = ID)   
    selectedLoc <- subset(selectedLoc,it = input$dateRange)
    
    if (substr(input$season,1,6) != 'Annual')
      if (substr(input$season,1,5) != 'rainy')
          selectedLoc <- esd::subset.station(selectedLoc,it = tolower(substr(input$season,1,3))) else
          selectedLoc <- esd::subset.station(selectedLoc,it = c('Oct','Nov','Dec','Jan','Feb','Mar'))
    
    leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
      addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
    isolate({
      showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
    })
    
    timeseries <- switch(input$ci,
    'mdp'=selectedLoc,
    'mwdm'=annual(selectedLoc, FUN='wetmean', nmin=30, threshold = thresh()),
    'mwdf'=annual(selectedLoc, FUN='wetfreq', nmin=30, threshold = thresh()),
    'ywo'=selectedLoc,
    'tmax'=selectedLoc,
    'tmin'=selectedLoc,
    'tmaxa'=anomaly(selectedLoc),
    'tmina'=anomaly(selectedLoc),
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
      if (is.null(timeseries)) esd::test.rainequation(selectedLoc,x0=30) else {
        esd::plot.station(timeseries, map.show = FALSE, new=FALSE,main=loc(timeseries),
                          ylab = paste(varid(timeseries),' (',unit(timeseries),')',sep=''))
        lines(trend(timeseries),lty=2)
        grid()
      }
    })
  })
  
  thresh <- reactive({
    return(as.numeric(input$thresh))
  })
  

#    output$plotncwd <- renderPlot({
#      
#      leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
#        addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
#      isolate({
#        showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
#      })
#      esd::plot.station(ncwd,new=FALSE,map.show = FALSE,errorbar = TRUE,col = 'green')
#      lines(trend(ncwd),lty = 5,col = 'darkgreen')
#      title('Annual means of wet spell length')
#      })
#    
#    output$rainequation <- renderPlot({
#      
#      leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
#        addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
#      isolate({
#        showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
#      })
#      esd::test.rainequation(ncwd,threshold=30)
#      title('Probability of heavy rainfall')
#    })
#    
#  }) 
}
