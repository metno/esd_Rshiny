## See comments and explanations in global.R
## Rasmus Benestad

library(shiny)
library(esd)
library(leaflet)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

# Define a server for the Shiny app
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(lon(y),lat(y))
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$map_marker_click,{
    #print("observeEvent() - click")
    event <- input$map_marker_click
    print(paste('Updated ',input$location)); print(event$id)
    updateSelectInput(session,inputId = 'location',
                      choices = toupper(Y$location))  
  })
  
  # Computing indices
  vals <- reactive({
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))
    print(paste('Y$',input$statistic,sep=''))
    return(Z) 
    })
  
  
  ## The map panel 
  output$map <- renderLeaflet({
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[1],verbose=verbose)
    ## Get the statistics to display
    statistic <- eval(parse(text=paste('Y$',input$statistic,sep='')))
    pal <- colorBin(colscal(col = 't2m',n=100,rev = is.precip(y)[1]),
                    vals(),bins = 10,pretty = TRUE)
    
    leaflet() %>% 
      addCircleMarkers(lng = Y$longitude, # longitude
                       lat = Y$latitude,fill = TRUE, # latitude
                       label = as.character(round(vals(),digits = 2)),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = Y$location,popupOptions(keepInView = TRUE),
                       radius =7,stroke=TRUE,weight = 1, color='black',
                       layerId = Y$station.id,
                       fillOpacity = 0.4,fillColor=pal(vals())) %>% 
      addLegend("bottomleft", pal=pal, values=round(vals(), digits = 2), 
                title=input$statistic,
                layerId="colorLegend") %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat=mean(Y$latitude),lng = mean(Y$longitude), zoom = 5)
  })
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    print(paste('showMetaPopup() ===',stid,round(lat,2),round(lng,2)))
    selLon <- round(Y$longitude[Y$station.id == stid],2)
    selLat <- round(Y$latitude[Y$station.id == stid],2)
    selAlt <- round(Y$altitude[Y$station.id == stid])
    location <- Y$location[Y$station.id == stid]
    print(c(stid,selLon,selLat,location))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), 
                                      paste(' LAT ',selLat,'N',sep=''), 
                                      paste(' ALT ',selAlt,'m',sep=''))), 
                     sprintf("Period: %s",paste(attr(Y,'period')[1],'-',attr(Y,'period')[2])),
                     sprintf("Station ID: %s", as.character(stid)),
                     sprintf("Parameter: %s", paste(toupper(varid(y)),collapse = ',')),
                     sprintf("Start year: %s", paste(Y$first.year[Y$station.id == stid],collapse = ',')),
                     sprintf("End year: %s", paste(Y$last.year[Y$station.id == stid],collapse = ',')),
                     sprintf("Data provider: Meteorologisk institutt"))
    
    leafletProxy("map") %>% addPopups(lng, lat, content,layerId = stid)
  }
  
  # When map is clicked, show a popup with location info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    #print('Click --->'); print(event); print('<--- Click')
    if (is.null(event))
      return()
    isolate({
      showMetaPopup(stid=event$id,lat=event$lat, lng = event$lng)
    })
    
    #removeMarker("map",layerId = event$id)
    leafletProxy("map",data = event) %>% 
      addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
  })
  
  ## Data Explorer ###########################################
  observe({
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    print('Data Explorer'); print(input$location)
    ##selectedStid <- Y$station.id[which(tolower(input$location) == tolower(Y$location))]
    selectedStid <- event$id
    ## Read single time series from the netCDF file
    print('selectedID'); print(selectedStid)
    if (is.null(selectedStid) | length(selectedStid)!=1) {print('Something is wrong!');browser()}
    y <- retrieve.station(fnames[as.numeric(input$ci)],stid=selectedStid,verbose=verbose)
    pal <- colorBin(colscal(col = 't2m',n=100,rev = is.precip(y)[1]),
                    vals(),bins = 10,pretty = TRUE)
    
    print('subset y'); print(loc(y)); print(stid(y))
    y <- subset(y,it = input$dateRange)
  
    leafletProxy("map",data = y) %>% clearPopups() %>% 
      addCircles(lng = lon(y), lat = lat(y), color = 'red', layerId = 'selectID', weight = 12)
    isolate({
      showMetaPopup(stid=stid(y),lat=lat(y), lng = lon(y))
    })
    

    timeseries <- y
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   })
     
      esd::plot.station(timeseries, map.show = FALSE, new=FALSE,main=loc(timeseries),
                        ylab = paste(varid(timeseries),' (',unit(timeseries),')',sep=''))
      lines(trend(timeseries),lty=2)
      grid()
    
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
