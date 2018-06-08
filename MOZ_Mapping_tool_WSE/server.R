library(shiny)
library(esd)
library(leaflet)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

# Define a server for the Shiny app
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(longitude, latitude)
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    ID <- which(station_id == event$id)
    selectedLoc <- subset.station(y, is = ID) 
   
    updateSelectInput(session,inputId = 'location',
                      choices = toupper(loc(selectedLoc)))  
    print(paste('Updated ',toupper(loc(selectedLoc))))
  })
  
  # Computing indices
  vals <- reactive({
    
    y <- esd::subset.station(y, it = input$dateRange)
    
    if (substr(input$season,1,6) != 'Annual')
      y <- esd::subset.station(y, it = tolower(substr(input$season,1,3)))
    
    # mean daily precipitation
    if (input$ci == 'mdp') 
      return(apply(annual(y,FUN='sum'),2,FUN='mean',na.rm=TRUE)) #REB2018-03-18
    
    # wet-day mean
    if (input$ci == 'mwdm') 
      return(apply(y,2,FUN='wetmean', threshold = thresh() ))
    
    # wet-day frequency
    if (input$ci == 'mwdf') 
      return(apply(y,2,FUN='wetfreq',threshold = thresh() ))
    
    # wet-day frequency
    if (input$ci == 'ywo') 
      return(endyr - startyr)
    
    withProgress(message = 'Please Wait',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   
                   # trend in annual means of precipitation
                   if (input$ci == 'tawdm') {
                     ya <- annual(y,FUN = 'wetmean', threshold = thresh() ,na.rm=TRUE)
                     #return(apply(ya,2,FUN='trend',na.rm=TRUE))
                     return(apply(annual(ya),2,FUN='trend.coef',na.rm=TRUE) / apply(annual(ya),2,FUN='mean',na.rm=TRUE) * 100)
                   }              
                   
                   # trend in annual means of temperauture
                   if (input$ci == 'tawdf') { 
                     ya <- annual(y,FUN = 'wetfreq', threshold = thresh() ,na.rm=TRUE)
                     #return(apply(ya,2,FUN='trend',na.rm=TRUE))
                     return(apply(annual(ya),2,FUN='trend.coef',na.rm=TRUE) / apply(annual(ya),2,FUN='mean',na.rm=TRUE) * 100)
                   }
                 })
    })
  
  output$map <- renderLeaflet({
    pal <- colorBin(colscal(col = 't2m',n=100,rev = TRUE),vals(),bins = 10,pretty = TRUE)
    
    leaflet() %>% 
      addCircleMarkers(lng = longitude,
                       lat = latitude,fill = TRUE,
                       label = as.character(round(vals(),digits = 2)),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = location,popupOptions(keepInView = TRUE),
                       radius =7,stroke=TRUE,weight = 1, color='black',
                       layerId = station_id,
                       fillOpacity = 0.4,fillColor=pal(vals())) %>% 
      addLegend("bottomleft", pal=pal, values=round(vals(), digits = 2), title='Color Scale',
                layerId="colorLegend") %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      #setView(lat=60,lng = 10, zoom = 5)
      setView(lat=-19,lng = 34, zoom = 5)
  })
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    ID <- which(station_id == stid)
    selectedLoc <- subset.station(y, is = ID) 
    print(c(loc(y)[ID],startyr[ID],endyr[ID]))
    
    #ftplink <- file.path('ftp://ftp.met.no/projects/chasepl/test/data.METNOD',paste('RR_',sprintf("%05d", as.numeric(stid)),'.dly',sep=''))
    
    selLon <- as.numeric(levels(factor(round(lon(selectedLoc),digits = 2))))
    selLat <- as.numeric(levels(factor(round(lat(selectedLoc),digits = 2))))
    selAlt <- as.numeric(levels(factor(alt(selectedLoc))))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location[ID]))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), paste(' LAT ',selLat,'N',sep=''), paste(' ALT ',selAlt,'m',sep=''))), tags$br(),
                     sprintf("Station ID: %s", as.character(stid(selectedLoc))),
                     sprintf("Parameter: %s", paste(toupper(varid(selectedLoc)),collapse = ',')),
                     #sprintf("Start year: %s", paste(start(selectedLoc),collapse = ',')),
                     #sprintf("End year: %s", paste(end(selectedLoc),collapse = ',')),
                     sprintf("Start year: %s", paste(startyr[ID],collapse = ',')),
                     sprintf("End year: %s", paste(endyr[ID],collapse = ',')),
                     sprintf("Data provider: %s", paste(attr(selectedLoc,'source'),collapse = ',')))
    
    leafletProxy("map") %>% addPopups(lng, lat, content,layerId = stid)
    print(stid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    #browser()
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
      #y <- try(esd::station(stid = event$id,param=c('t2m','precip'),src='metnod',user = 'external'))
      
      if (sum(grepl(pattern = 'error',x = class(y),ignore.case = TRUE)) > 0)
        return(NULL)
      
      esd::plot.station(esd::subset.station(y,is= event$id == station_id), 
                        map.show = FALSE, new=FALSE,plot.type='mult',
                        ylab = varid(y))
      
    })
    
  })
  
  # ## Data Explorer ###########################################
  # 
  observe({
    #selectedLoc <- input$location
    ID <- which(tolower(input$location) == tolower(location))
    #selectedStid <- station_id[which(tolower(input$location) == tolower(location))]
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
    
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   })
      #browser()
      esd::plot.station(selectedLoc, map.show = FALSE, new=FALSE,plot.type='mult',ylab = varid(y))
    })
  })
  
  thresh <- reactive({
    return(as.numeric(input$thresh))
  })
  
  observe({
    # Annual plots
    
    ID <- which(tolower(input$location) == tolower(location))
    #selectedStid <- station_id[which(tolower(input$location) == tolower(location))]
    selectedLoc <- subset(y,is = ID)   
    selectedLoc <- subset(selectedLoc,it = input$dateRange)
    
    if (substr(input$season,1,6) != 'Annual')
      selectedLoc <- esd::subset.station(selectedLoc,it = tolower(substr(input$season,1,3)))
    
    X <- combine.stations(selectedLoc,selectedLoc)
    Y <- allgood(X)
    selectedLoc <- subset(Y,is=1)
    
    wdm <- annual(selectedLoc, FUN='wetmean', threshold = thresh())
    wdf <- annual(selectedLoc, FUN='wetfreq', threshold = thresh())
    
    ncwd <- annual(subset(esd::spell(selectedLoc, threshold = thresh()),is = 1),FUN='max',nmin = 0)
    ncdd <- subset(spell(selectedLoc, threshold = thresh()),is = 2)
    
    output$plotwdm <- renderPlot({
      leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
        addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
      isolate({
        showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
      })
      esd::plot.station(wdm,new=FALSE,map.show = FALSE,errorbar = TRUE,col = 'red')
      lines(trend(wdm),lty = 5,col = 'darkred')
      title('Annual means of Wet-day Amount [mm]')
      })
    output$plotwdf <- renderPlot({
      
      leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
        addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
      isolate({
        showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
      })
      esd::plot.station(wdf,new=FALSE,map.show = FALSE,errorbar = TRUE,col = 'orange')
      lines(trend(wdf),lty = 5,col = 'orange')
      title('Annual means of Wet-day frequency')
      })
    output$plotncwd <- renderPlot({
      
      leafletProxy("map",data = selectedLoc) %>% clearPopups() %>% 
        addCircles(lng = lon(selectedLoc), lat = lat(selectedLoc), color = 'red', layerId = 'selectID', weight = 12)
      isolate({
        showMetaPopup(stid=stid(selectedLoc),lat=lat(selectedLoc), lng = lon(selectedLoc))
      })
      esd::plot.station(ncwd,new=FALSE,map.show = FALSE,errorbar = TRUE,col = 'green')
      lines(trend(ncwd),lty = 5,col = 'darkgreen')
      title('Annual means of wet spell length')
      })
  }) 
}
