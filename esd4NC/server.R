# define the back-end

shinyServer(function(input, output, session) {
  
  #countview <- reactiveValues(i = 1)
  
  ## Try to get the location names to depend on whether temperature of precipitation stations
  # update GCM names regarding which RCP
  # observe(priority = 10, {
  #               gcmnames <- switch(tolower(as.character(input$rcp7)),
  #                              'rcp4.5'=gcmnames.45,'rcp2.6'=gcmnames.26,'rcp8.5'=gcmnames.85)
  #               updateSelectInput(session = session, inputId = "im7",choices = gcmnames, selected = gcmnames[1])
  #              }
  # )
  # 
  # ## compute values to display on the map
  # getval <- function(season1,param1,rcp1,dates1,lon1,lat1,stats,aspect,im) { 
  #   print('In getval function')
  #   it <- range(as.numeric(dates1))
  #   it0 <- range(as.numeric(dates1))
  #   is <- list(lon=as.numeric(lon1),lat=as.numeric(lat1))
  #   season <- switch(tolower(as.character(season1)),
  #                    'winter'='djf','spring'='mam','summer'='jja','autumn'='son')
  #   rcp <- switch(tolower(as.character(rcp1)),
  #                 'rcp4.5'='45','rcp2.6'='26','rcp8.5'='85')
  #   param <- switch(tolower(as.character(param1)),
  #                   'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
  #                   'precip. sum'='ptot')
  #   FUN <- switch(tolower(as.character(aspect)),
  #                 "mean value"="mean", "change"="change","trend"="trend","variability"="sd")
  #   FUNX <- switch(tolower(as.character(stats)),
  #                  "ensemble mean"="mean", "ensemble spread"="sd")
  #   eval(parse(text = paste('z <- Z4$',paste(param,season,rcp,sep='.'),sep='')))
  #   #if (param>=0) li <- (rcp-1)*4+season + param else
  #   #  li <- (rcp-1)*4+season + 12
  #   gcmnames <- names(z)[grep('_',names(z))]
  #   im <- is.element(gcmnames,im)
  #   #
  #   #main <- paste('Downscaled',FUN,tolower(input$season1),tolower(input$param1),'for',it[1],'-',it[2],
  #   #              'following',toupper(input$rcp1),'based on',sum(im),'model runs')
  #   
  #   ## Check if the total precipitation needs to be estimated:
  #   if (param != 'ptot' ) {
  #     ##browser()
  #     #zmap <- Z4[[li]]
  #     zmap <- map(z,FUN="mean",FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  #     if (FUN=="change") {
  #       zmap0 <- map(z,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
  #       coredata(dzmap) <- coredata(zmap) - coredata(zmap0)
  #       zmap <- dzmap; rm('dzmap','dzmap0'); it <- NULL; im <- NULL; is <- NULL
  #       #str(zmap)
  #       FUN <- "mean"
  #     }
  #   } else {
  #     ## The total precipitation = 90 * fw * mu which requires a transform from EOF to
  #     ## a field object
  #     eval(parse(text = paste('mu <- Z4$',paste('mu',season,rcp,sep='.'),sep='')))
  #     eval(parse(text = paste('fw <- Z4$',paste('fw',season,rcp,sep='.'),sep='')))
  #     
  #     mumap <-  map(mu,FUN="mean",FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  #     fwmap <-  map(fw,FUN="mean",FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  #     
  #     #zmap1 <- mu # Z4[[(rcp-1)*4+season + 12]]
  #     #zmap2 <- fw # Z4[[(rcp-1)*4+season + 24]]
  #     if (FUN=="change") {
  #       if (it[1]==it[2]) it <- c(it[1],it[1]+(it0[2]-it0[1]))
  #       print('Estimate change for total precipitation')
  #       print(exists('zmap1'))
  #       print(c(it,it0))
  #       mumap0 <- map(mu,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
  #       fwmap0 <- map(fw,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
  #       zmap <- mumap
  #       coredata(zmap) <- 90*(coredata(mumap)*coredata(fwmap) - coredata(mumap0)*coredata(fwmap0))
  #       it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
  #       rm('fwmap0','mumap0')
  #     }
  #     if (FUN=="trend") { 
  #       itx <- c(it[1],max(c(it[2],it[1]+30)))
  #       mumap <-  map(mu,FUN="mean",FUNX=FUNX,it=itx,is=is,im=im,plot=FALSE)
  #       fwmap <-  map(fw,FUN="mean",FUNX=FUNX,it=itx,is=is,im=im,plot=FALSE)
  #       zmap <- mumap
  #       coredata(zmap) <- 90*coredata(mumap)*coredata(fwmap)
  #       it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
  #       rm('mumap','fwmap')
  #     } 
  #     else {
  #       zmap1 <- map(fw,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  #       zmap2 <- map(mu,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  #       zmap <- zmap1
  #       coredata(zmap) <- 90*coredata(zmap1)*coredata(zmap2) 
  #       it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
  #       rm('zmap1','zmap2')
  #     }
  #     attr(zmap,'variable') <- 'precip'
  #     attr(zmap,'unit') <- 'mm/season'
  #     #str(zmap)
  #     # val <- coredata(zmap)
  #     # print('val contain')
  #     # print(str(zmap))
  #     
  #   }
  #   # browser()
  #   return(zmap)
  #   
  # }
  # 
  # observe(priority = -10,{
  #   vals1 <- getval(season1 = 'Winter',input$param1,input$rcp1,input$dates1,input$lon1,input$lat1,input$stats,input$aspect,input$im)
  #   
  #   vals2 <- getval(season1 = 'Spring',input$param1,input$rcp1,input$dates1,input$lon1,input$lat1,input$stats,input$aspect,input$im)
  #   
  #   vals3 <- getval(season1 = 'Summer',input$param1,input$rcp1,input$dates1,input$lon1,input$lat1,input$stats,input$aspect,input$im)
  #   
  #   vals4 <- getval(season1 = 'Autumn',input$param1,input$rcp1,input$dates1,input$lon1,input$lat1,input$stats,input$aspect,input$im)
  #  
  #   output$map0 <- renderLeaflet({
  #     #browser()
  #     zmap <- vals1()
  #     coredata(zmap) <- (coredata(vals1()) + coredata(vals2()) + coredata(vals3()) + coredata(vals4())) / 4
  #     #browser()
  #     print('zmap contains ...')
  #     #str(zmap)
  #     cat('PARAM',input$param1,'SEASON', input$season1,'RCP',input$rcp1)
  #     cat(sep = '\n')
  #     x <- attr(zmap,'longitude')
  #     y <- attr(zmap,'latitude')
  #     z <- coredata(zmap)
  #     #browser()
  #     #Create raster object
  #     dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
  #     dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     r <- raster(dat1)
  #     print(r)
  #     #
  #     if ((input$param1 == 'Temperature')) {
  #       rev <- FALSE
  #       col <- 't2m'
  #       breaks <- seq(-20,20,1)
  #     } else if (input$param1 == 'Precip. sum') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,1200,50)
  #     } else if (input$param1 == 'Wet-day freq.') {
  #       rev <- TRUE
  #       col <- 't2m'
  #       breaks <- seq(0,1,0.05)
  #     } else if (input$param1 == 'Precip. intensity') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,20,0.05)
  #     }
  #     
  #     pal <- colorBin(colscal(col = col,n=100,rev=rev),breaks,bins = 20,pretty = TRUE,na.color = NA)
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$Esri.WorldStreetMap,
  #                        #addProviderTiles(providers$Stamen.TonerLite,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       setView(lat=64,lng = 16, zoom = 5) %>%
  #       addRasterImage(x = r,colors = pal, opacity = 0.65) %>%
  #       addLegend("bottomleft", pal=pal, values=round(r@data@values, digits = 2), title='Legend',
  #                 layerId="colorLegend") %>% 
  #       addMiniMap()
  #   })
  #   
  #   output$map1 <- renderLeaflet({
  #     
  #     zmap <- vals1()
  #     print('zmap contains ...')
  #     #str(zmap)
  #     cat('PARAM',input$param1,'SEASON', input$season1,'RCP',input$rcp1)
  #     cat(sep = '\n')
  #     x <- attr(zmap,'longitude')
  #     y <- attr(zmap,'latitude')
  #     z <- coredata(zmap)
  #     #browser()
  #     #Create raster object
  #     dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
  #     dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     r <- raster(dat1)
  #     print(r)
  #     #
  #     if ((input$param1 == 'Temperature')) {
  #       rev <- FALSE
  #       col <- 't2m'
  #       breaks <- seq(-20,20,1)
  #     } else if (input$param1 == 'Precip. sum') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,1200,50)
  #     } else if (input$param1 == 'Wet-day freq.') {
  #       rev <- TRUE
  #       col <- 't2m'
  #       breaks <- seq(0,1,0.05)
  #     } else if (input$param1 == 'Precip. intensity') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,20,0.05)
  #     }
  #     
  #     pal <- colorBin(colscal(col = col,n=100,rev=rev),breaks,bins = 20,pretty = TRUE,na.color = NA)
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$Esri.WorldStreetMap,
  #                        #addProviderTiles(providers$Stamen.TonerLite,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       setView(lat=65,lng = 20, zoom = 4) %>%
  #       addRasterImage(x = r,colors = pal, opacity = 0.65) # %>%
  #     # addLegend("bottomleft", pal=pal, values=round(r@data@values, digits = 2), title='Winter',
  #     #          layerId="colorLegend") # %>% 
  #     #addMiniMap()
  #   })
  #   
  #   output$map2 <- renderLeaflet({
  #     
  #     zmap <- vals2()
  #     print('zmap contains ...')
  #     #str(zmap)
  #     cat('PARAM',input$param1,'SEASON', input$season1,'RCP',input$rcp1)
  #     cat(sep = '\n')
  #     x <- attr(zmap,'longitude')
  #     y <- attr(zmap,'latitude')
  #     z <- coredata(zmap)
  #     #browser()
  #     #Create raster object
  #     dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
  #     dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     r <- raster(dat1)
  #     print(r)
  #     #
  #     if ((input$param1 == 'Temperature')) {
  #       rev <- FALSE
  #       col <- 't2m'
  #       breaks <- seq(-20,20,1)
  #     } else if (input$param1 == 'Precip. sum') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,1200,50)
  #     } else if (input$param1 == 'Wet-day freq.') {
  #       rev <- TRUE
  #       col <- 't2m'
  #       breaks <- seq(0,1,0.05)
  #     } else if (input$param1 == 'Precip. intensity') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,20,0.05)
  #     }
  #     
  #     pal <- colorBin(colscal(col = col,n=100,rev=rev),breaks,bins = 20,pretty = TRUE,na.color = NA)
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$Esri.WorldStreetMap,
  #                        #addProviderTiles(providers$Stamen.TonerLite,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       setView(lat=65,lng = 20, zoom = 4) %>%
  #       addRasterImage(x = r,colors = pal, opacity = 0.65) # %>%
  #     # addLegend("bottomleft", pal=pal, values=round(r@data@values, digits = 2), title='Spring',
  #     #         layerId="colorLegend")# %>% 
  #     #addMiniMap()
  #   })
  #   
  #   output$map3 <- renderLeaflet({
  #     
  #     zmap <- vals3()
  #     print('zmap contains ...')
  #     #str(zmap)
  #     cat('PARAM',input$param1,'SEASON', input$season1,'RCP',input$rcp1)
  #     cat(sep = '\n')
  #     x <- attr(zmap,'longitude')
  #     y <- attr(zmap,'latitude')
  #     z <- coredata(zmap)
  #     #browser()
  #     #Create raster object
  #     dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
  #     dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     r <- raster(dat1)
  #     print(r)
  #     #
  #     if ((input$param1 == 'Temperature')) {
  #       rev <- FALSE
  #       col <- 't2m'
  #       breaks <- seq(-20,20,1)
  #     } else if (input$param1 == 'Precip. sum') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,1200,50)
  #     } else if (input$param1 == 'Wet-day freq.') {
  #       rev <- TRUE
  #       col <- 't2m'
  #       breaks <- seq(0,1,0.05)
  #     } else if (input$param1 == 'Precip. intensity') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,20,0.05)
  #     }
  #     
  #     pal <- colorBin(colscal(col = col,n=100,rev=rev),breaks,bins = 20,pretty = TRUE,na.color = NA)
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$Esri.WorldStreetMap,
  #                        #addProviderTiles(providers$Stamen.TonerLite,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       setView(lat=65,lng = 20, zoom = 4) %>%
  #       addRasterImage(x = r,colors = pal, opacity = 0.65) # %>%
  #     # addLegend("bottomleft", pal=pal, values=round(r@data@values, digits = 2), title='Summer',
  #     #           layerId="colorLegend") # %>% 
  #     # addMiniMap()
  #   })
  #   
  #   output$map4 <- renderLeaflet({
  #     
  #     zmap <- vals4()
  #     print('zmap contains ...')
  #     #str(zmap)
  #     cat('PARAM',input$param1,'SEASON', input$season1,'RCP',input$rcp1)
  #     cat(sep = '\n')
  #     x <- attr(zmap,'longitude')
  #     y <- attr(zmap,'latitude')
  #     z <- coredata(zmap)
  #     #browser()
  #     #Create raster object
  #     dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
  #     dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     r <- raster(dat1)
  #     print(r)
  #     #
  #     if ((input$param1 == 'Temperature')) {
  #       rev <- FALSE
  #       col <- 't2m'
  #       breaks <- seq(-20,20,1)
  #     } else if (input$param1 == 'Precip. sum') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,1200,50)
  #     } else if (input$param1 == 'Wet-day freq.') {
  #       rev <- TRUE
  #       col <- 't2m'
  #       breaks <- seq(0,1,0.05)
  #     } else if (input$param1 == 'Precip. intensity') {
  #       rev <- FALSE
  #       col <- 'precip'
  #       breaks <- seq(0,20,0.05)
  #     }
  #     
  #     pal <- colorBin(colscal(col = col,n=100,rev=rev),breaks,bins = 20,pretty = TRUE,na.color = NA)
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$Esri.WorldStreetMap,
  #                        #addProviderTiles(providers$Stamen.TonerLite,
  #                        options = providerTileOptions(noWrap = TRUE)) %>%
  #       setView(lat=65,lng = 20, zoom = 4) %>%
  #       addRasterImage(x = r,colors = pal, opacity = 0.65) # %>%
  #     # addLegend("bottomleft", pal=pal, values= round(r@data@values, digits = 2), title='Autumn', # 
  #     #           layerId="colorLegend") # %>% 
  #     # addMiniMap()
  #   })
  # })
  #y <- map(zmap,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
  
  #map(y,main=main,FUN="mean",new=FALSE)
  
  ## Plot individual station
  # output$plot <- renderPlot({
  #   if(!is.null(input$location2)) {
  #     season <- switch(tolower(as.character(input$season2)),
  #                      'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
  #     rcp <- switch(tolower(as.character(input$rcp2)),
  #                   'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
  #     param <- switch(tolower(as.character(input$param2)),
  #                     'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
  #                     'precip. sum'=-1)
  #     #if (param==0) obs <- t2m else obs <- rr
  #     fun <- switch(tolower(as.character(input$param2)),
  #                   'temperature'='mean','wet-day freq.'='wetfreq','precip. intensity'='wetmean')
  #     if (param>=0) {
  #       li <- (rcp-1)*4+season + param
  #     } else {
  #       li <- (rcp-1)*4+season + 12
  #     }
  #     
  #     locs2 <- switch(tolower(as.character(input$param2)),
  #                     "temperature" = t2m.locs,
  #                     "wet-day freq." = pre.locs,
  #                     "precip. intensity" = pre.locs,
  #                     "precip. sum"=pre.locs)
  #     
  #     if (param==0) {
  #       is <- srt.t2m[is.element(locs2,as.character(input$location2))]
  #     } else {
  #       is <- srt.pre[is.element(locs2,as.character(input$location2))]
  #     }
  #     
  #     gcmnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
  #     
  #     zz <- Z4[[li]]; zz$eof <- NULL;
  #     ## Reduce the matrix size and pick one station before the recovery of the original format
  #     zz$pca <- subset(zz$pca,is=is)
  #     im <- is.element(gcmnames,input$im)
  #     z <- as.station(zz,im=im,verbose=FALSE)
  #     if (param<0) {
  #       z1 <- z
  #       zz2 <- Z4[[li+12]]; zz2$eof <- NULL;
  #       
  #       ## Reduce the matrix size and pick one station before the recovery of the original format
  #       zz2$pca <- subset(zz2$pca,is=is)
  #       z2 <- as.station(zz2,im=im)
  #       z <- 90*z1*z2
  #       z <- attrcp(z2,z)
  #       attr(z,'station') <- 90*attr(z1,'station')*attr(z2,'station')
  #       attr(z,'station') <- attrcp(attr(z1,'station'),attr(z,'station'))
  #       class(attr(z,'station')) <- class(attr(z1,'station'))
  #       attr(z,'varible') <- 'precip'
  #       attr(z,'unit') <- 'mm/season'
  #       attr(attr(z,'station'),'varible') <- 'precip'
  #       attr(attr(z,'station'),'unit') <- 'mm/season'
  #       class(z) <- class(z2)
  #       rm('z1','z2')
  #     }
  #     #y <- subset(obs,is=is)
  #     #y <- subset(as.4seasons(y,FUN=fun),it=c('djf','mam','jja','son')[season])
  #     #main <- paste(is,li,sum(im),sum(is.finite(coredata(z))),index(z)[1],paste(class(z),collapse='-'))
  #     main <- paste(is,li,sum(im),index(z)[1],paste(class(z),collapse='-'))
  #     #plot(z,main=main,obs.show=FALSE,target.show=FALSE,legend.show=FALSE,new=FALSE)
  #     plot(z,main=main,target.show=FALSE,legend.show=FALSE,new=FALSE,
  #          xrange=c(2,35),yrange=c(53,75),
  #          map.show=TRUE,usegooglemap=TRUE,verbose=FALSE)
  #   }
  #   #index(y) <- year(y)
  #   #lines(y,type='b',lwd=3,cex=1.2)
  #   #plot(rnorm(100),main=main)
  # }, height=function(){0.9*session$clientData$output_plot_width},width = 'auto') #600}) 
  # 

# observe({
#    #if (session$isClosed()) {
#    sheet <- googlesheets::gs_title('esd_Rshiny_usage')
#    df <- data.frame(cbind(date(), session$clientData$url_hostname, input$rcp7,
#                           input$param7,input$im,input$season7,paste(input$date#s7,collapse = '/'),paste(input$datesref,collapse = '/'),paste(input$lon7,collap#se = '/'),paste(input$lat7,collapse = '/')))
#    gs_add_row(sheet,ws = 1, input = df)
#    #}
#  })

## Plot ensemble statistics for multiple-stations
  output$plot.multi <- renderPlot({ 
    it <- range(as.numeric(input$dates3))
    season <- switch(tolower(as.character(input$season3)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp3)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    param <- switch(tolower(as.character(input$param3)),
                    'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
                    'precip. sum'=-1)
    li <- (rcp-1)*4+season+param
    gcmnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    im <- is.element(gcmnames,input$im)
    zz <- Z4[[li]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','season','list')
    lons <- lon(zz$pca); lats <- lat(zz$pca); alts <- alt(zz$pca)
    keep <- lons >= min(input$lon3) & lons <= max(input$lon3) &
      lats >= min(input$lat3) & lats <= max(input$lat3) &
      alts >= min(input$alt3) & alts <= max(input$alt3)
    is <- (1:length(lons))[keep]
    zz <- subset(zz,it=it,im=im,is=is)
    z <- as.station(zz)
    main <- paste(input$param3,season,input$rcp3,li,it[1],it[2],length(is),sum(im))
    plot(z,main=main,new=FALSE,fig=c(0,1,0,0.8),mar=c(4.5,4.8,0.75,0.5),
         usegooglemap=FALSE,verbose=FALSE)
    grid()
  },height=function(){0.65*session$clientData$output_plot.multi_width} )#600})
  
  ## Show a map of evaluation scores
  ## Central parts of circle shows trend similarity
  ## Outer rim shows the number outside the 90% confidence interval
  #browser()
  
  #observe({
  #  #if (session$isClosed()) {
  #  sheet <- googlesheets::gs_title('esd_Rshiny_usage')
  #  df <- data.frame(cbind(date(), session$clientData$url_hostname, input$rcp7,
  #                         input$param7,input$im,input$season7,paste(input$date#s7,collapse = '/'),paste(input$datesref,collapse = '/'),paste(input$lon7,collap#se = '/'),paste(input$lat7,collapse = '/')))
#    gs_add_row(sheet,ws = 1, input = df)
#    #}
#  })
  
  output$plot.prob <- renderPlot({
    if(!is.null(input$location4)) {
      it <- range(as.numeric(input$dates4))
      season <- switch(tolower(as.character(input$season4)),
                       'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
      rcp <- switch(tolower(as.character(input$rcp4)),
                    'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
      param <- switch(tolower(as.character(input$param4)),
                      'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
                      'precip. sum'=-1)
      if (param>=0) li <- (rcp-1)*4+season + param else
        li <- (rcp-1)*4+season + 12 
      gcmnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
      im <- is.element(gcmnames,input$im)
      locs4 <- switch(input$param4,
                      "Temperature" = t2m.locs,
                      "Wet-day freq." = pre.locs,
                      "Precip. intensity" = pre.locs,
                      "Precip. sum"=pre.locs)
      if (param==0) is <- srt.t2m[is.element(locs4,as.character(input$location4))] else
        is <- srt.pre[is.element(locs4,as.character(input$location4))]
      
      zz <- Z4[[li]]; zz$eof <- NULL;
      class(zz) <- c('dsensemble','pca','season','list')
      lons <- lon(zz$pca); lats <- lat(zz$pca); alts <- alt(zz$pca)
      zz <- subset(zz,im=im,is=is)
      z <- as.station(zz)
      ## Total precipitation
      if (param<0) {
        z1 <- z
        zz2 <- Z4[[li+12]]; zz2$eof <- NULL;
        
        ## Reduce the matrix size and pick one station before the recovery of the original format
        zz2$pca <- subset(zz2$pca,is=is)
        z2 <- as.station(zz2,im=im)
        z <- 90*z1*z2
        z <- attrcp(z2,z)
        attr(z,'station') <- 90*attr(z1,'station')*attr(z2,'station')
        attr(z,'station') <- attrcp(attr(z1,'station'),attr(z,'station'))
        class(attr(z,'station')) <- class(attr(z1,'station'))
        attr(z,'varible') <- 'precip'
        attr(z,'unit') <- 'mm/season'
        attr(attr(z,'station'),'varible') <- 'precip'
        attr(attr(z,'station'),'unit') <- 'mm/season'
        class(z) <- class(z2)
        rm('z1','z2')
      }
      z <- c(as.numeric(subset(z,it=it)))
      zx <- ceiling(max(c(abs(z),abs(input$threshold4)),na.rm=TRUE))+1
      breaks <- switch(input$param4,
                       "Temperature" = seq(-zx,zx,by=0.5),
                       "Wet-day freq." = seq(0,1,by=0.02),
                       "Precip. intensity" = seq(0,zx,by=0.2),
                       "Precip. sum"=seq(0,zx,by=10))
      if (as.character(input$direction4) == "Lower") {
        prob <- pnorm(input$threshold4,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)) 
        probability <- paste('Pr(X < ',round(input$threshold4,2),')=',sep='')
      } else {
        prob <- 1 - pnorm(input$threshold4,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))
        probability <- paste('Pr(X >= ',round(input$threshold4,2),')=',sep='')
      }
      main <- paste(input$location4,probability,round(100*prob,2),'%')
      hist(z,breaks=breaks,main=main,new=FALSE,freq=FALSE,col='grey')
      X <- seq(-zx,zx,by=0.05)
      lines(X,dnorm(X,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),
            lwd=5,col=rgb(0.5,0,0,0.3))
      if (as.character(input$direction4) == "Lower") {
        Xless <- X[X <= input$threshold4]
        polygon(c(Xless,max(Xless),min(Xless)),
                c(dnorm(Xless,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),0,0),
                col=rgb(1,0,0,0.2))
      } else {
        Xmore <- X[X >= input$threshold4]
        polygon(c(max(Xmore),min(Xmore),Xmore),
                c(0,0,dnorm(X[X >= input$threshold4],mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))),
                col=rgb(1,0,0,0.2))
      }
    }
  },height=function(){0.8*session$clientData$output_plot.prob_width} )#600})
  
  ## Unfinished!  
  output$map.quality <- renderPlot({ 
    season <- switch(tolower(as.character(input$season6)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp6)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    param <- switch(tolower(as.character(input$param6)),
                    'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
                    'precip. sum'=-1)
    li <- (rcp-1)*4+season+param
    is <- list(lon=as.numeric(input$lon6),lat=as.numeric(input$lat6))
    im <- is.element(gcmnames,input$im)
    
    ## If all models are selected, use pre-calculated quality scores else calculate for 
    ## selected models
    if (sum(im) < length(im)) {
      zz <- Z4[[li]]
      class(zz) <- c('dsensemble','pca','list')
      ## Reduce the matrix size and pick one station before the recovery of the original format
      im <- is.element(gcmnames,input$im)
      zz <- subset(zz,is=is,im=im)
      z <- as.station(zz)
      if (as.character(input$quality6)=="trend") {
        score <- unlist(lapply(z,trendscore)) 
      } else if (as.character(input$quality6)=="spread") {
        score<- unlist(lapply(z,varscore))
      }
      dim(score) <- c(3,length(z))
      lons <- lon(zz$pca); lats <- lat(zz$pca)
    } else {
      ## Pre-calculated scores for whole ensemble
      if (as.character(input$quality6)=="trend") {
        score <- quality$trend[[li]]
      } else {
        score <- quality$range[[li]]
      }
      if (param==0) {
        lons <- lon(quality$t2m); lats <- lat(quality$t2m)
      } else {
        lons <- lon(quality$pre); lats <- lat(quality$pre)
      }
      ixy <- (lons >= is$lon[1]) & (lons <= is$lon[2]) &
        (lats >= is$lat[1]) & (lats <= is$lat[2])
      score <- score[,ixy]; lons <- lons[ixy]; lats <- lats[ixy]
    }
    main <- paste('Quality:',input$quality6,' for ',
                  input$season6,input$param6,' (',sum(im),input$rcp6,' runs)',sep='')
    plot(lons,lats,xlab='',ylab='',main=main,lwd=2,cex=2,pch=19,col='grey80')
    grid()
    
    data(geoborders)
    lines(geoborders,col='grey')
    
    col <- rep(rgb(0,0.5,0.5),length(lons)); pch <- rep(19,length(lons))
    q2 <- score[1,] < 0.1 | score[1,] > 0.9
    q3 <- score[1,] < 0.05 | score[1,] > 0.95
    col[q2] <- rgb(0.5,0.5,0); pch[q2] <- 15
    col[q3] <- rgb(1,0,0);   pch[q3] <- 17
    points(score[2,],score[3,],pch=pch,cex=1.5,col=col)
    par0 <- par()
    par(new=TRUE,fig=c(0.05,0.25,0.9,1),mar=rep(1,4),xaxt='n',yaxt='n')
    image(cbind(1:3,1:3),col=c(rgb(0,0.5,0.5),rgb(0.5,0.5,0),rgb(1,0,0)))
    text(0,1,'[10,90]',col='white',cex=0.8,pos=1)
    text(0.50,1,'[05,95]',col='white',cex=0.8,pos=1)
    text(1,1,'outside',col='white',cex=0.8,pos=1)
    print("almost done")
    par(par0)
  },height=function(){0.8*session$clientData$output_map.quality_width} )#600})
  
  ## Show the difference between one selected model and the mean of the rest of the ensemble.
  
  ### Function to read seasonal values
  gety1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref)({
    print('in Y1 function')
    #browser()
    # SET PARAMETERS
    season <- switch(tolower(as.character(season7)),
                     'Annual (All seasons)'='ann','winter'='djf','spring'='mam','summer'='jja','autumn'='son')
    rcp <- switch(tolower(as.character(rcp7)),
                  'intermediate emissions (rcp4.5)'='45','low emissions (rcp2.6)'='26','high emissions (rcp8.5)'='85')
    param <- switch(tolower(as.character(param7)),
                    'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                    'precip. sum'='ptot')
    
    it <- range(as.numeric(dates7))
    it.ref <- range(as.numeric(datesref))
    is <- list(lon=as.numeric(lon7),lat=as.numeric(lat7))
    
    # load the data 
    eval(parse(text = paste('z <- Z4$',paste(param,season,rcp,sep='.'),sep='')))
    # browser()
    
    if (input$im == 'Ens. Mean') {
      z1 <- subset.dsensemble(z,it=it,is=is)
      zz <- subset.dsensemble(z,it=it.ref,is=is)
      y1 <- map(z1,FUN="mean",FUNX='mean',plot=FALSE)
      yy <- map(zz,FUN="mean",FUNX='mean',plot=FALSE)
    } else {
      gcmnames <- names(z)[grep('_',names(z))]
      im1 <- is.element(gcmnames,im)
      z1 <- esd::subset.dsensemble.multi(z,im=im1,it=it,is=is)
      zz <- esd::subset.dsensemble.multi(z,im=im1,it=it.ref,is=is)
      #zz <- subset(z,im=!im1,it=it.ref,is=is)
      y1 <- map(z1,plot=FALSE)
      yy <- map(zz,plot=FALSE)
    }
    if (input$param7 == 'Temperature')
      coredata(y1) <- coredata(y1) - coredata(yy)
    else 
      coredata(y1) <- (coredata(y1) - coredata(yy)) / coredata(yy) * 100
    # main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
    #               season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
    invisible(y1)
  }) 
  
  observe({
    if (tolower(input$rcp7) == 'intermediate emissions (rcp4.5)') 
      choices <- c('Ens. Mean','------',gcmnames.45)
    else if (tolower(input$rcp7) == 'high emissions (rcp8.5)')
      choices <- c('Ens. Mean','------',gcmnames.85)
    else if (tolower(input$rcp7) == 'low emissions (rcp2.6)')
      choices <- c('Ens. Mean','------',gcmnames.26)
    updateSelectInput(session,inputId = "im", choices = choices, selected = choices[1])
  })
  
  # reactive expressions
  ysm1 <- reactive({
    return(gety1('Winter',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  ysm2 <- reactive({
    return(gety1('Spring',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  ysm3 <- reactive({
    return(gety1('Summer',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  ysm4 <- reactive({
    return(gety1('Autumn',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  
 
  
  # REnder the map
  zmap.reactive <- reactive({
    
    ## browser()
    season <- switch(tolower(as.character(input$season7)),
                     'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
    
    if (season == 'ann')  {
      
      zmap1 <- ysm1()
      zmap2 <- ysm2()
      zmap3 <- ysm3()
      zmap4 <- ysm4()
      
      zmap <- zmap1
      coredata(zmap) <- (coredata(zmap1) + coredata(zmap2) + coredata(zmap3) + coredata(zmap4)) / 4
      rm('zmap1','zmap2','zmap3','zmap4')
      
    } else {
      if (season == 'djf')
        zmap <- ysm1()
      else if (season == 'mam')
        zmap <- ysm2()
      else if (season == 'jja')
        zmap <- ysm3()
      else if (season == 'son')
        zmap <- ysm4()
    }
    return(zmap)
  })
  
  observe(priority = 0, {
    zmap <- zmap.reactive()
    output$map0sm <- renderLeaflet({
    ## browser()
    print('zmap contains ...')
      # str(zmap)
      cat('ObserveEvent','PARAM',input$param7,'SEASON', input$season7,'RCP',input$rcp7,'SM',input$im)
      cat(sep = '\n')
      x <- attr(zmap,'longitude')
      y <- attr(zmap,'latitude')
      z <- coredata(zmap)
      ## browser()
      #Create raster object
      dat1 <- list(x=attr(zmap,'longitude'),y = attr(zmap,'latitude'), z = coredata(zmap))
      dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
      r <- raster(dat1)
      print(print(object.size(r),units = 'Mb'))
      
      if ((input$param7 == 'Temperature')) {
        rev <- FALSE
        col <- 'warm'
        rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
        breaks <- c(0,max(rng))
        #breaks <- seq(-5,5,0.5)
        leg.title <- "Change [C]"
      } else if (input$param7 == 'Precip. sum') {
        rev <- TRUE
        col <- 't2m'
        breaks <- seq(-50,50,5)
        leg.title <- 'Change [%]'
      } else if (input$param7 == 'Wet-day freq.') {
        rev <- FALSE
        col <- 't2m'
        rng <- round(range(r@data@values,na.rm=TRUE),digits = 0)
        breaks <- c(-max(abs(rng)),max(abs(rng)))
        #breaks <- seq(0,1,0.05)
        leg.title <- 'Change [%]'
      } else if (input$param7 == 'Precip. intensity') {
        rev <- TRUE
        col <- 't2m'
        rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
        breaks <- c(-max(abs(rng)),max(abs(rng))) #seq(0,20,0.05)
        leg.title <- 'Change [%]'
      }
      
      pal <- colorBin(colscal(col = col,rev=rev),breaks, bins = 10, pretty = TRUE,na.color = NA)
      
      ## custom label format function
      myLabelFormat = function(..., reverse_order = FALSE){
        if(reverse_order){
          function(type = "numeric", cuts){
            cuts <- sort(cuts, decreasing = T)
          }
        } else{
          labelFormat(...)
        }
      }
      # browser()
      m <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldStreetMap,
                         #addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lat=64,lng = 16, zoom = 5) %>%
        addRasterImage(x = r,colors = pal, opacity = 0.65)
      ## browser()  
      if (input$legend == 'Display')
        m <- m %>% addLegend("bottomleft", values=round(r@data@values, digits = 2), 
                             title=leg.title, colors = rev(colscal(col= col, rev = rev, n=length(pretty(breaks,n = 10)))),
                             labels = rev(pretty(breaks,n = 10)),#pal=pal, 
                             labFormat = myLabelFormat(reverse_order = F),layerId="colorLegend") 
        # m <- m %>% addLegend("topleft", values=round(r@data@values, digits = 2), title=leg.title, colors = pal(round(r@data@values, digits = 2)),labels = seq(1,10,1),#pal=pal, 
        #                      labFormat = myLabelFormat(reverse_order = T),layerId="colorLegend") # labFormat = myLabelFormat(reverse_order = T),
       if (input$minimap == 'Display')
        m <- m %>% addMiniMap()
      m
    })
  })
  
  ### Render the plot
  ### Function to read seasonal values
  getz1 <- function(season7,dates7,lon7,lat7,rcp7,param7,im,datesref)({
    print('in Z1 function')
    # browser()
    # SET PARAMETERS
    season <- switch(tolower(as.character(season7)),
                     'Annual (All seasons)'='ann','winter'='djf','spring'='mam','summer'='jja','autumn'='son')
    rcp <- switch(tolower(as.character(rcp7)),
                  'intermediate emissions (rcp4.5)'='45','low emissions (rcp2.6)'='26','high emissions (rcp8.5)'='85')
    param <- switch(tolower(as.character(param7)),
                    'temperature'='t2m','wet-day freq.'='fw','precip. intensity'='mu',
                    'precip. sum'='ptot')
    
    it <- range(as.numeric(dates7))
    it.ref <- range(as.numeric(datesref))
    is <- list(lon=as.numeric(lon7),lat=as.numeric(lat7))
    
    # load the data 
    eval(parse(text = paste('z <- Z4$',paste(param,season,rcp,sep='.'),sep='')))
   
    if (input$im == 'Ens. Mean') {
      z1 <- subset.dsensemble(z, is=is)
      y1 <- plot.dsensemble(z1,plot = FALSE, new = FALSE)
    } else {
      gcmnames <- names(z)[grep('_',names(z))]
      im1 <- is.element(gcmnames,im)
      z1 <- subset(z,im=im1,is=is)
      y1 <- plot.dsensemble(z1,plot=FALSE,new = FALSE)
    }
    invisible(y1)
  }) 
  
  # reactive expressions
  zsm1 <- reactive({
    return(getz1('Winter',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  zsm2 <- reactive({
    return(getz1('Spring',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  zsm3 <- reactive({
    return(getz1('Summer',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  zsm4 <- reactive({
    return(getz1('Autumn',input$dates7,input$lon7,input$lat7,input$rcp7,input$param7,input$im,input$datesref))
  })
  
  zplot.reactive <- reactive({
    
    ##browser()
    season <- switch(tolower(as.character(input$season7)),
                     'annual (all seasons)'='ann','winter (djf)'='djf','spring (mam)'='mam','summer (jja)'='jja','autumn (son)'='son')
    
    if (season == 'ann')  {
      
      zmap1 <- zsm1()
      zmap2 <- zsm2()
      zmap3 <- zsm3()
      zmap4 <- zsm4()
      
      zmap <- zmap1
      coredata(zmap) <- (coredata(zmap1) + coredata(zmap2) + coredata(zmap3) + coredata(zmap4)) / 4
      rm('zmap1','zmap2','zmap3','zmap4')
      
    } else {
      if (season == 'djf')
        zmap <- zsm1()
      else if (season == 'mam')
        zmap <- zsm2()
      else if (season == 'jja')
        zmap <- zsm3()
      else if (season == 'son')
        zmap <- zsm4()
    }
    return(zmap)
  })
  
  # observe(priority = 0, {
  #   zplot <- zplot.reactive()
  #   output$plot <- renderPlot({
  #     
  #     ## browser()
  #     print('zplot contains ...')
  #     str(zplot)
  #     # df <- data.frame(cbind(as.numeric(year(zplot)),rowMeans(coredata(zplot),na.rm=TRUE)))
  #     # colnames(df) <- c('Years','Value')
  #     # gvisLineChart(data = df, xvar = 'Years', yvar = 'Value') #, options=list(width=400, height=450)
  #     z.sta <- as.station(zoo(x = zoo(coredata(rowMeans(zplot,na.rm=TRUE)),order.by = index(zplot))))
  #     plot.station(z.sta, new = FALSE, ylab = input$param7,xlab = 'Years')
  #     # cat('ObserveEvent','PARAM',input$param7,'SEASON', input$season7,'RCP',input$rcp7,'SM',input$im)
  #     # cat(sep = '\n')
  #     # x <- attr(zplot,'longitude')
  #     # y <- attr(zplot,'latitude')
  #     # z <- coredata(zplot)
  #     # ## browser()
  #     # #Create raster object
  #     # dat1 <- list(x=attr(zplot,'longitude'),y = attr(zplot,'latitude'), z = coredata(zplot))
  #     # dim(dat1$z) <- c(length(dat1$x),length(dat1$y))
  #     # r <- raster(dat1)
  #     # print(r)
  #     # #
  #     # if ((input$param7 == 'Temperature')) {
  #     #   rev <- FALSE
  #     #   col <- 'warm'
  #     #   rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
  #     #   breaks <- c(0,max(rng))
  #     #   #breaks <- seq(-5,5,0.5)
  #     #   leg.title <- "Absolute change [C]"
  #     # } else if (input$param7 == 'Precip. sum') {
  #     #   rev <- FALSE
  #     #   col <- 'precip'
  #     #   breaks <- seq(-50,50,5)
  #     #   leg.title <- 'Relative change [%]'
  #     # } else if (input$param7 == 'Wet-day freq.') {
  #     #   rev <- TRUE
  #     #   col <- 't2m'
  #     #   rng <- round(range(r@data@values,na.rm=TRUE),digits = 0)
  #     #   breaks <- c(-max(abs(rng)),max(abs(rng)))
  #     #   #breaks <- seq(0,1,0.05)
  #     #   leg.title <- 'Relative change  [%]'
  #     # } else if (input$param7 == 'Precip. intensity') {
  #     #   rev <- FALSE
  #     #   col <- 'precip'
  #     #   rng <- round(range(r@data@values,na.rm=TRUE),digits = 1)
  #     #   breaks <- c(-max(abs(rng)),max(abs(rng))) #seq(0,20,0.05)
  #     #   leg.title <- 'Relative change [%]'
  #     # }
  #     
  #   })
  # })
  
  # output$title1 <- renderText({
  #   if (input$im == 'Ens. Mean')
  #     txt <- 'Ensemble Mean'
  #   else 
  #     txt <- unlist(strsplit(input$im,split = '_'))[2]
  #   paste(toupper(txt), ' of projected changes in ',input$season7, ' mean ', input$param7,'.',sep= '')
  # })
  
  output$subtitle1 <- renderText({
    if (input$im == 'Ens. Mean')
      txt <- 'Ensemble Mean'
    else 
      txt <- unlist(strsplit(input$im,split = '_'))[2]
    paste(toupper(txt), ' of projected changes in mean ', 
          tolower(input$season7), tolower(input$param7), 'by',
          paste(input$dates7,collapse = '--'), 'with regards to the base period ',
          paste(input$datesref, collapse = '--'),' assuming ',input$rcp7, 'scenarios.')
  })
  
  output$subtitle2 <- renderText({
    if (input$im == 'Ens. Mean')
      txt <- 'Ensemble Mean'
    else 
      txt <- unlist(strsplit(input$im,split = '_'))[2]
    paste(toupper(txt), ' of area averaged projected changes in mean ', 
          tolower(input$season7), tolower(input$param7), 'by',
          paste(input$dates7,collapse = '--'), 'with regards to the base period ',
          paste(input$datesref, collapse = '--'),' assuming ',input$rcp7, 'scenarios.')
  })
  
  output$use.stats <- renderText({
    txt <- paste(countview$i,"actions")
  })
  
  observe({
    if (!input$im == 'Ens. Mean') {
      ngcms <- 1
      subtitle <- tags$h5(paste('Global Climate Model (',
                                toupper(unlist(strsplit(input$im,split = '_'))[2]),')',sep =''))
    } else {
      if (input$rcp7 == 'Low emissions (RCP2.6)')
        ngcms <- length(gcmnames.26)
      else if (input$rcp7 == 'Intermediate emissions (RCP4.5)')
        ngcms <- length(gcmnames.45)
      else if (input$rcp7 == 'High emissions (RCP8.5)')
        ngcms <- length(gcmnames.85)
      subtitle <- tags$h5('Global Climate Models')
    }
    output$ngcmbox <- renderValueBox({
      valueBox(value = ngcms, subtitle = subtitle,
               icon = NULL, color = "aqua", width = 4,
               href = NULL)})
    
    output$rcpbox <- renderValueBox({
      # browser()
      text <- strsplit(input$rcp7,split = '\\(')[[1]][1]
      maintext <-strsplit(text,split = ' ')[[1]][1]
      rcp <- strsplit(input$rcp7,split = '\\(')[[1]][2]
      subtext <- strsplit(rcp,split = '\\)')[[1]][] 
      valueBox(value = maintext, subtitle = tags$h5(paste('Emission Scenario (',subtext,')',sep='')),
               icon = NULL, color = "green", width = 4,
               href = NULL)
    })
    
    output$changebox <- renderValueBox({
      
      if (input$param7 == 'Temperature') unit <- ' C' else unit <- '%'
      val <- round(mean(coredata(zmap.reactive()),na.rm=TRUE),digits = 1)
      if (val >0)
        txt <- paste('+',as.character(val), unit,sep = ' ')
      else 
 	txt <- paste(as.character(val), unit,sep = ' ')
      valueBox(as.character(HTML(txt)), 
               subtitle = paste('Averaged change in ', input$param7,sep=''),
               icon = NULL, color = "orange", width = 4)
    })
    
    
  })
  
})
