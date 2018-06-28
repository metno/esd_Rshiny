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
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    event <- input$map_marker_click
    #print(paste('Updated ',input$location)); print(event$id)
    selected <- which(Y$station.id == event$id)
    updateSelectInput(session,inputId = 'location',label='Sted',choices=Y$location,
                      selected = Y$location[selected])  
  })
  
  # Computing indices
  vals <- reactive({
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    print(paste('Y$',input$statistic,sep=''))
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    print(paste('Y$',input$statistic,showseason,sep=''))
    if (tolower(substr(input$statistic,1,8)!='expected')) 
      Z <- eval(parse(text=paste('Y$',input$statistic,showseason,sep=''))) else {
      #Z <- eval(parse(text=paste('Y$',input$statistic,sep=''))) else {
      if (is.precip(y)) Z <- switch(input$season,
                                     'all'=3.6525*Y$wetfreq*exp(-input$thresh/Y$wetmean),
                                     'DJF'=0.90*Y$wetfreq_DJF*exp(-input$thresh/Y$wetmean_DJF),
                                     'MAM'=0.90*Y$wetfreq_MAM*exp(-input$thresh/Y$wetmean_MAM),
                                     'JJA'=0.90*Y$wetfreq_JJA*exp(-input$thresh/Y$wetmean_JJA),
                                     'SON'=0.90*Y$wetfreq_SON*exp(-input$thresh/Y$wetmean_SON)) else
                       Z <- switch(input$season,
                                      'all'=365.25*(1-pnorm(input$thresh,mean=Y$mean,sd=Y$sd)),
                                      'DJF'=90*(1-pnorm(input$thresh,mean=Y$mean_DJF,sd=Y$sd_DJF)),
                                      'MAM'=90*(1-pnorm(input$thresh,mean=Y$mean_MAM,sd=Y$sd_MAM)),
                                      'JJA'=90*(1-pnorm(input$thresh,mean=Y$mean_JJA,sd=Y$sd_JJA)),
                                      'SON'=90*(1-pnorm(input$thresh,mean=Y$mean_SON,sd=Y$sd_SON)))                
      }
    if (input$statistic=='number.valid') Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))/attr(Y,'length')
    print(length(Z)); print(summary(Z))
    return(Z) 
    })
  
  
  ## The map panel 
  output$map <- renderLeaflet({
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    ## Get the statistics to display
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    statistic <- eval(parse(text=paste('Y$',input$statistic,showseason,sep='')))
    pal <- colorBin(colscal(col = 't2m',n=100),
                    vals(),bins = 10,pretty = TRUE,reverse=is.precip(y)[1])
    print('palette (map) - reversed?'); print(is.precip(y)[1]); print(summary(statistic))
    
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
                layerId="colorLegend",labFormat = labelFormat(big.mark = "")) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat=mean(Y$latitude),lng = mean(Y$longitude), zoom = 5)
  })
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    #print(paste('showMetaPopup() ===',stid,round(lat,2),round(lng,2)))
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    selLon <- round(Y$longitude[Y$station.id == stid],2)
    selLat <- round(Y$latitude[Y$station.id == stid],2)
    selAlt <- round(Y$altitude[Y$station.id == stid])
    location <- Y$location[Y$station.id == stid]
    #print(c(stid,selLon,selLat,location))
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
    #print('Data Explorer from map'); print(event$id)
    selectedStid <- Y$station.id[which(tolower(input$location) == tolower(Y$location))]
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
  
  ## Data Explorer roll down menu
  observe({
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    #print('Data Explorer from roll-down menu'); print(input$location)
    selectedStid <- Y$station.id[which(tolower(input$location) == tolower(Y$location))]
    ## Read single time series from the netCDF file
    print('selectedID'); print(selectedStid)
    if (is.null(selectedStid) | length(selectedStid)!=1) 
    {
      print(input$location);print('Something is wrong!'); 
      if (is.null(selectedStid)) selectedStid <- Y$station.id[1] else
      if (length(selectedStid)>1) selectedStid <- selectedStid[1] else
        browser()
      }
    y <- retrieve.station(fnames[as.numeric(input$ci)],stid=selectedStid,verbose=verbose)
    pal <- colorBin(colscal(col = 't2m',n=100),
                    vals(),bins = 10,pretty = TRUE,reverse = is.precip(y)[1])
    print('palette - reversed?'); print(is.precip(y)[1]); print(loc(y)); print(stid(y))
    if (is.precip(y)) thresholds <- seq(10,50,by=10) else thresholds <- seq(-30,30,by=5)
    y <- subset(y,it = input$dateRange)
  
    print(input$aspect); print(input$season); print(input$statistic)
    if (is.precip(y)) FUN='sum' else FUN='mean'
    if (input$aspect=='anomaly') y <- anomaly(y) 
    if (input$season != 'all') {
      y <- subset(y,it=tolower(input$season)); nmin <-75
      print(length(y)); print(nv(y))
    } else nmin <- 300
    timeseries <- y
    if (sum(is.element(input$statistic,c('trend','mean')))>0) timeseries <- annual(y,FUN=FUN,nmin=nmin) else 
      if (length(grep('wetfreq',input$statistic))>0) {
        timeseries <- 100*annual(y,FUN='wetfreq',nmin=nmin)
        attr(timeseries,'unit') <- '%'
      } else if (length(grep('wetmean',input$statistic))>0) timeseries <- annual(y,FUN='wetmean',nmin=nmin)
    
    leafletProxy("map",data = y) %>% clearPopups() %>% 
      addCircles(lng = lon(y), lat = lat(y), color = 'red', layerId = 'selectID', weight = 12)
    isolate({
      showMetaPopup(stid=stid(y),lat=lat(y), lng = lon(y))
    })
    
    output$plotstation <- renderPlot({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                   })
     
      if (tolower(substr(input$statistic,1,8)=='expected')) {
        if (is.precip(y)) {
          timeseries <- test.rainequation(y,x0=input$thresh,plot=FALSE) 
          plot(merge(ndays*timeseries[,1],timeseries[,1]),lwd=c(3,2),col=c('black','red'),
               'Main= Expected and counted number of days',ylab='count',xlab='',
               sub=paste('Days with more than',input$thresh,unit(y)))
          grid()
          } else {
          ave <- annual(y,FUN='mean',nmin=nmim); std <- annual(anomaly(y,FUN='sd',nmin=nmin))
          nv <- annual(y,FUN='nv')
          obs <- annual(y,FUN='count',x0=input$thresh)
          est <- zoo(x=nv*pnorm(input$thresh,mean=coredata(ave),sd=coredata(std)),order.by=index(ave))
          plot(merge(obs,est,all=TRUE),plot.type='single',col=c('red','black'))
        }
          
      } else {
        plot.station(timeseries, map.show = FALSE, new=FALSE,main=loc(timeseries),
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
    mx <- ceiling(1.1*max(abs(y),na.rm=TRUE))
    if (is.precip(y)) breaks = seq(0,mx,by=1) else
                               breaks = seq(-mx,mx,by=0.25)
    col <- rainbow(length(breaks)-1)
    hist(y, breaks=breaks,col=col,main=loc(y),freq=FALSE,
         xlab = paste(varid(y),' (',unit(y),')',sep=''))
    grid()
  })
  
  })
  
  thresh <- reactive({
    return(as.numeric(input$thresh))
  })
  
}
