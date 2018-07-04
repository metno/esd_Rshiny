## See comments and explanations in global.R
## Rasmus Benestad


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
  
  # observe(input$aspectTS, {
  #   if (is.precip(y)) choices <- aspectsP else choices <- aspectsT
  #   updateSelectInput(session,inputId = 'aspectTS',label='aspect',choices=choices)})
  
  # Computing indices
  vals <- reactive({
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    print(paste('Y$',input$statistic,sep=''))
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    print(paste('Y$',input$statistic,showseason,sep=''))
    if ( (tolower(input$statistic)!='number_of_days') &
         (tolower(input$statistic)!='specific_date') )
      Z <- eval(parse(text=paste('Y$',input$statistic,showseason,sep=''))) else {
        if (tolower(substr(input$statistic,1,8)!='expected')) {
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
        } else {
          ## Get a specific date
          Z <- retrieve.station(fnames[as.numeric(input$ci)],it=input$it)
        }
      }
    if (input$statistic=='number.valid') Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))/attr(Y,'length')
    print(length(Z)); print(summary(Z))
    return(Z) 
  })
  
  ## The map panel 
  output$map <- renderLeaflet({
    ## Get summary data from the netCDF file
    print(input$ci)
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    ## Get the statistics to display
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    statistic <- eval(parse(text=paste('Y$',input$statistic,showseason,sep='')))
    if (tolower(input$higlight) == "top 10") highlight <- order(statistic,decreasing=TRUE)[1:10] else 
    if (tolower(input$higlight) == "low 10") highlight <- order(statistic,decreasing=FALSE)[1:10] else 
      highlight <- NULL
    lon.highlight <- Y$longitude[highlight]
    lat.highlight <- Y$latitude[highlight]
    #print(statistic[highlight]); print(lon.highlight); print(lat.highlight)
    pal <- colorBin(colscal(col = 't2m',n=100),
                    vals(),bins = 10,pretty = TRUE,reverse=is.precip(y)[1])
    #print('palette (map) - reversed?'); print(is.precip(y)[1]); print(summary(statistic))
    
    leaflet() %>% 
      addCircleMarkers(lng = Y$longitude, # longitude
                       lat = Y$latitude,fill = TRUE, # latitude
                       label = as.character(round(vals(),digits = 2)),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = Y$location,popupOptions(keepInView = TRUE),
                       radius =7,stroke=TRUE,weight = 1, color='black',
                       layerId = Y$station.id,
                       fillOpacity = 0.4,fillColor=pal(vals())) %>% 
      addCircleMarkers(lng = lon.highlight, lat = lat.highlight,fill=TRUE,
                       label=as.character(1:10),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       radius=8,stroke=TRUE, weight=5, color='black',
                       layerId = Y$station.id[highlight],
                       fillOpacity = 0.6,fillColor=rep("black",10)) %>%
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
    print(paste('showMetaPopup() ===',stid,round(lat,2),round(lng,2)))
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
  
  observeEvent(input$lingo, {
    tscales <- c("day","month","season","year")
    names(tscales) <- timescales[as.numeric(input$lingo),]
    updateSelectInput(session=session,inputId="tscale",choices=tscales,selected=tscales[1])
  })

  observeEvent(input$lingo, {
    varids <- 1:length(varids)
    names(varids) <- varnames[as.numeric(input$lingo),]
    print(varids)
    updateSelectInput(session=session,inputId="ci",choices=varids)
  })
  
  observeEvent(input$ci, {
    #print('Change aspect according to climate indicator'); print(input$ci); print(varids)
    aspects <- switch(tolower(varids[as.numeric(input$ci)]),
                      'precip'=aspectsP,'t2m'=aspectsT,'tmax'=aspectsT,'tmin'=aspectsT)
    names(aspects) <- switch(tolower(input$ci),
                             'precip'=aspectnameP,'t2m'=aspectnameT,'tmax'=aspectnameT,'tmin'=aspectnameT)
    #print(aspects)
    updateSelectInput(session=session,inputId="aspect",choices=aspects,selected=aspects[1])
  })
  
  observeEvent(input$ci,{
    updateSelectInput(session=session,inputId="statistic",
                      choices=getstattype(fnames[as.numeric(input$ci)]),selected="mean")
  })
  
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
    #print('selectedID'); print(selectedStid)
    if (is.null(selectedStid) | length(selectedStid)!=1) 
    {
      print(input$location);print('Something is wrong!'); 
      if (is.null(selectedStid)) selectedStid <- Y$station.id[1] else
        if (length(selectedStid)>1) selectedStid <- selectedStid[1] else
          selectedStid <- Y$station.id[1]
    }
    y <- retrieve.station(fnames[as.numeric(input$ci)],stid=selectedStid,verbose=verbose)
    pal <- colorBin(colscal(col = 't2m',n=100),
                    vals(),bins = 10,pretty = TRUE,reverse = is.precip(y)[1])
    
    #print('palette - reversed?'); print(is.precip(y)[1]); print(loc(y)); print(stid(y))
    if (is.precip(y)) thresholds <- seq(10,50,by=10) else thresholds <- seq(-30,30,by=5)
    y <- subset(y,it = input$dateRange)
    
    #print(input$season); print(input$tscale)
    if (is.precip(y)) {
      if (input$aspect=='wetmean') FUN<-'wetmean' else
        if (input$aspect=='wetfreq') FUN<-'wetfreq' else
          if (input$aspect=="number of days") FUN<-'count' else FUN<-'sum'
    } else if (input$aspect=="number of days") FUN<-'count' else FUN<-'mean'
    FUN='mean'
    
    
    ## Time series
    x0 <- as.numeric(input$x0)
    y <- switch(input$tscale,
                'day'=y,'month'=as.monthly(y,FUN=FUN),
                'season'=as.4seasons(y,FUN=FUN),'year'=as.annual(y,FUN=FUN))
    #if (is.T(y)) browser()
    #print(c(aspects,input$aspect)); print(input$ci)
    if (input$aspect=='anomaly') y <- anomaly(y)
    if (input$seasonTS != 'all') y <- subset(y,it=tolower(input$seasonTS))
    
    ## Marking the top and low 10 poits
    if (tolower(input$highlightTS)=='top 10') highlight10 <- y[order(coredata(y),decreasing=TRUE)[1:10]] else
      if (tolower(input$highlightTS)=='low 10') highlight10 <- y[order(coredata(y),decreasing=FALSE)[1:10]] else
        highlight10 <- y[1:10]+NA
    #print(highlight10)
    
    if (input$timespace == timespace[1]) yH <- y else yH <- vals()
    
    leafletProxy("map",data = y) %>% clearPopups() %>% 
      addCircles(lng = lon(y), lat = lat(y), color = 'red', layerId = 'selectID', weight = 12)
    
    isolate({
      showMetaPopup(stid=stid(y),lat=lat(y), lng = lon(y))
    })
    
    
    output$plotstation <- renderPlotly({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.05)}
                   })
      
      timeseries <- data.frame(date=index(y),y=coredata(y),trend=coredata(trend(y)))
      TS <- plot_ly(timeseries,x=~date,y=~y,type = 'scatter',mode='lines',name='data')
      TS = TS %>% add_trace(y=~trend,name='trend') %>% 
        add_markers(x=index(highlight10),y=coredata(highlight10),label=input$highlightTS) %>% 
        layout(title=loc(y),yaxis = list(title=unit(y)))
      #TS$elementID <- NULL
    })
    
    output$histstation <- renderPlotly({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.05)
                   }
                   })
      mx <- ceiling(1.1*max(abs(y),na.rm=TRUE))
      if (is.precip(yH)) {
        bin_size <- 1
        breaks = seq(1,mx,by=bin_size)
        cY <- coredata(yH); cY[cY<1] <- NA
        h <- hist(cY,breaks=breaks)
        pdf <- wetfreq(yH)*exp(-h$mids/wetmean(yH))
      } else if (is.T(yH)) {
        bin_size=0.25
        breaks = seq(floor(min(yH)),ceiling(max(yH)),by=bin_size)
        h <- hist(coredata(yH),breaks=breaks)
        pdf <- dnorm(h$mids,mean=mean(yH,na.rm=TRUE),sd=sd(yH,na.rm=TRUE))
      } else {
        h <- hist(coredata(yH))
        pdf <- dnorm(h$mids,mean=mean(yH,na.rm=TRUE),sd=sd(yH,na.rm=TRUE))
      }
      
      dist <- data.frame(y=h$density,x=h$mids,pdf=pdf)
      H <- plot_ly(dist,x=~x,y=~y,name='data',type='scatter')
      H = H %>% add_trace(x=dist$x,y=dist$pdf,name='pdf',mode='lines') %>% layout(title=loc(y))
    })
    
    output$maintitle <- renderText({
      maintitle[as.numeric(input$lingo)]})
    output$maptitle <- renderText({
      maptitle[as.numeric(input$lingo)]})
    output$tstitle <- renderText({
      tstitle[as.numeric(input$lingo)]})
    output$htitle <- renderText({
      htitle[as.numeric(input$lingo)]})
    output$cftitle <- renderText({
      cftitle[as.numeric(input$lingo)]})
    output$timespace.label <- renderText({
      lab.timespace[as.numeric(input$lingo)]})
    output$timeperiod.label <- renderText({
      lab.timeperiod[as.numeric(input$lingo)]})
    output$timescale.label <- renderText({
      lab.timescale[as.numeric(input$lingo)]})
    output$season.label <- renderText({
      lab.season[as.numeric(input$lingo)]})
    output$aspect.label <- renderText({
      lab.aspect[as.numeric(input$lingo)]})
    output$location.label <- renderText({
      lab.location[as.numeric(input$lingo)]})
    output$statistics.label <- renderText({
      lab.statitics[as.numeric(input$lingo)]})
    output$threshold <- renderText({
      lab.threshold[as.numeric(input$lingo)]})
    output$specificdate <- renderText({
      lab.date[as.numeric(input$lingo)]})
    output$highlight.label <- renderText({
      lab.highlight[as.numeric(input$lingo)]})
    output$highlightTS.label <- renderText({
      lab.highlight[as.numeric(input$lingo)]})
    
    thresh <- reactive({
      return(as.numeric(input$thresh))
    })
    
  })
}