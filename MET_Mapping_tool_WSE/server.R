library(shiny)
library(esd)
library(leaflet)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)

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
      setView(lat=65,lng = 10, zoom = 5)
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
    ftplink <- file.path('ftp://ftp.met.no/projects/chasepl/test/data.METNOD',paste('RR_',sprintf("%05d", as.numeric(stid)),'.dly',sep=''))
    #browser()
    selLon <- as.numeric(levels(factor(selectedLoc$longitude)))
    selLat <- as.numeric(levels(factor(selectedLoc$latitude)))
    selAlt <- as.numeric(levels(factor(selectedLoc$altitude)))
    content <- paste(sep = "<br/>",
                     paste(paste("<b><a href='",ftplink,"'>",sep=''),toupper(selectedLoc$location),"</a></b>",sep=''),
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
      y <- try(esd::station(stid = event$id,param=c('t2m','precip'),src='metnod',user = 'external'))
      
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
    
    y <- try(esd::station(stid = selectedStid,param=c('t2m','precip'),src='metnod',user = 'external'))
    
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
      y <- try(esd::station(stid = selectedStid,param=c('t2m','precip'),src='metnod',user = 'external'))

      if (sum(grepl(pattern = 'error',x = class(y),ignore.case = TRUE)) >0)
        return(NULL)
      else
        ysub <- esd::subset.station(y,is = 1:length(c('t2m','precip')), it=as.numeric(c(input$firstyear,input$endyear)))

      if (grepl(pattern = 'error',x = class(ysub),ignore.case = TRUE))
        return(NULL)
      else 
        esd::plot.station(ysub, map.show = FALSE, new=FALSE,plot.type='mult',ylab = varid(y))
    })
  })
  
}
