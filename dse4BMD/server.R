## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)
#if ('RgoogleMaps' %in% installed.packages()) install.packages('RgoogleMaps')
#library(RgoogleMaps)

## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.

Z4 <- list()
load('data/dse.BMD.tmax.rcp45.djf.eof.rda')
Z4$tmax.djf.45 <- Z
#load('data/dse.BMD.tmax.rcp45.mam.eof.rda')
Z4$tmax.mam.45 <- Z
load('data/dse.BMD.tmax.rcp45.jja.eof.rda')
Z4$tmax.jja.45 <- Z
load('data/dse.BMD.tmax.rcp45.son.eof.rda')
Z4$tmax.son.45 <- Z

load('data/dse.BMD.tmax.rcp26.djf.eof.rda')
Z4$tmax.djf.26 <- Z
#load('data/dse.BMD.tmax.rcp26.mam.eof.rda')
Z4$tmax.mam.26 <- Z
load('data/dse.BMD.tmax.rcp26.jja.eof.rda')
Z4$tmax.jja.26 <- Z
load('data/dse.BMD.tmax.rcp26.son.eof.rda')
Z4$tmax.son.26 <- Z

load('data/dse.BMD.tmax.rcp85.djf.eof.rda')
Z4$tmax.djf.85 <- Z
#load('data/dse.BMD.tmax.rcp85.mam.eof.rda')
Z4$tmax.mam.85 <- Z
load('data/dse.BMD.tmax.rcp85.jja.eof.rda')
Z4$tmax.jja.85 <- Z
load('data/dse.BMD.tmax.rcp85.son.eof.rda')
Z4$tmax.son.85 <- Z

## Wet-day frequency
load('data/dse.BMD.fw.rcp45.djf.eof.rda')
Z4$fw.djf.45 <- Z
load('data/dse.BMD.fw.rcp45.mam.eof.rda')
Z4$fw.mam.45 <- Z
load('data/dse.BMD.fw.rcp45.jja.eof.rda')
Z4$fw.jja.45 <- Z
load('data/dse.BMD.fw.rcp45.son.eof.rda')
Z4$fw.son.45 <- Z

load('data/dse.BMD.fw.rcp26.djf.eof.rda')
Z4$fw.djf.26 <- Z
load('data/dse.BMD.fw.rcp26.mam.eof.rda')
Z4$fw.mam.26 <- Z
load('data/dse.BMD.fw.rcp26.jja.eof.rda')
Z4$fw.jja.26 <- Z
load('data/dse.BMD.fw.rcp26.son.eof.rda')
Z4$fw.son.26 <- Z

load('data/dse.BMD.fw.rcp85.djf.eof.rda')
Z4$fw.djf.85 <- Z
load('data/dse.BMD.fw.rcp85.mam.eof.rda')
Z4$fw.mam.85 <- Z
load('data/dse.BMD.fw.rcp85.jja.eof.rda')
Z4$fw.jja.85 <- Z
load('data/dse.BMD.fw.rcp85.son.eof.rda')
Z4$fw.son.85 <- Z

## Wet-day mean precipitation
#load('data/dse.BMD.mu.rcp45.djf.eof.rda')
Z4$mu.djf.45 <- Z
load('data/dse.BMD.mu.rcp45.mam.eof.rda')
Z4$mu.mam.45 <- Z
load('data/dse.BMD.mu.rcp45.jja.eof.rda')
Z4$mu.jja.45 <- Z
load('data/dse.BMD.mu.rcp45.son.eof.rda')
Z4$mu.son.45 <- Z

load('data/dse.BMD.mu.rcp26.mam.eof.rda')
Z4$mu.djf.26 <- Z
load('data/dse.BMD.mu.rcp26.mam.eof.rda')
Z4$mu.mam.26 <- Z
load('data/dse.BMD.mu.rcp26.jja.eof.rda')
Z4$mu.jja.26 <- Z
load('data/dse.BMD.mu.rcp26.son.eof.rda')
Z4$mu.son.26 <- Z

load('data/dse.BMD.mu.rcp85.mam.eof.rda')
Z4$mu.djf.85 <- Z
load('data/dse.BMD.mu.rcp85.mam.eof.rda')
Z4$mu.mam.85 <- Z
load('data/dse.BMD.mu.rcp85.jja.eof.rda')
Z4$mu.jja.85 <- Z
load('data/dse.BMD.mu.rcp85.son.eof.rda')
Z4$mu.son.85 <- Z

load('data/bmd.rda')
t2m <- Y
load('data/bmd.rda')
rr <- Y
rm('Y')
#load('data/quality.rda')
srt.t2m <- order(loc(Z4[[1]]$pca))
srt.pre <- order(loc(Z4[[13]]$pca))                 
tmax.locs <- sort(loc(Z4[[1]]$pca))
pre.locs <- sort(loc(Z4[[13]]$pca))
iview <- 0

## Estimate the probabilities for trend in observation is within the population trends based on of downscaled results
## zoo objects are slow so extract the core data
trendscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  ty <- trend.coef(y)
  tX <- apply(X,2,FUN='trend.coef')
  score <- pnorm(ty,mean=mean(tX),sd=sd(tX))
  return(c(score,lon(x),lat(x)))
}

## Estimate the probabilities for observed values are within the 90% conf. int. of population of downscaled results
## zoo objects are slow so extract the core data
varscore <- function(x) {
  it.X <- year(x) 
  X <- coredata(x)
  it.y <- year(attr(x,'station'))
  y <- coredata(attr(x,'station'))
  X <- X[is.element(it.X,it.y),]
  nX <- sum(apply(cbind(y,X),1,FUN=function(x) x[1] < quantile(x[-1],probs=0.05) | x[1] > quantile(x[-1],probs=0.95)))
  score <- pbinom(nX,size=length(y),prob=0.1)
  return(c(score,lon(x),lat(x)))
}


shinyServer(function(input, output, session) {
  #countview <- reactiveValues(i = 1)
  
  ## Try to get the location names to depend on whether temperature of precipitation stations
  output$locations2 <- renderUI({
    Locs2 <- switch(input$param2,
                    "Temperature" = tmax.locs,
                    "Wet-day freq." = pre.locs,
                    "Precip. intensity" = pre.locs,
                    "Precip. sum"=pre.locs)
    selectInput("location2", 
                label = "Location",
                choices = Locs2,
                selected = "OSLO BLINDERN")
  })
  output$locations4 <- renderUI({
    Locs4 <- switch(input$param4,
                    "Temperature" = tmax.locs,
                    "Wet-day freq." = pre.locs,
                    "Precip. intensity" = pre.locs,
                    "Precip. sum"=pre.locs)
    selectInput("location4", 
                label = "Location",
                choices = Locs4,
                selected = "OSLO BLINDERN")
  })
  output$thresholds8 <- renderUI({
    Thresh8 <- switch(input$direction8,
                     "Hot summer days"=20,
                     "Cold winter days"=0)
    numericInput("threshold8",
                 label = "Threshold",
                 value = Thresh8,
                 min = -50.0,
                 max = 50.0)
  })
    
  ## Show map of gridded temperature
  output$maps <- renderPlot({ 
    it <- range(as.numeric(input$dates1))
    it0 <- range(as.numeric(input$baseline))
    is <- list(lon=as.numeric(input$lon1),lat=as.numeric(input$lat1))
    season <- switch(tolower(as.character(input$season1)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp1)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    param <- switch(tolower(as.character(input$param1)),
                    'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
                    'precip. sum'=-1)
    FUN <- switch(tolower(as.character(input$aspect)),
                  "mean value"="mean", "change"="change","trend"="trend","variability"="sd")
    FUNX <- switch(tolower(as.character(input$stats)),
                   "ensemble mean"="mean", "ensemble spread"="sd")
    
    if (param>=0) li <- (rcp-1)*4+season + param else
                  li <- (rcp-1)*4+season + 12
    im <- is.element(gcmnames,input$im)
    gcnames <<- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    main <- paste('Downscaled',FUN,tolower(input$season1),tolower(input$param1),'for',it[1],'-',it[2],
                  'following',toupper(input$rcp1),'based on',sum(im),'model runs')
    
    ## Check if thetotal precipitation needs to be estimated:
    if (param>=0) {
      zmap <- Z4[[li]]
      if (FUN=="change") {
        dzmap <- map(zmap,FUN="mean",FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
        dzmap0 <- map(zmap,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
        coredata(dzmap) <- coredata(dzmap) - coredata(dzmap0)
        zmap <- dzmap; rm('dzmap','dzmap0'); it <- NULL; im <- NULL; is <- NULL
        str(zmap)
        FUN <- "mean"
      }
    } else {
      ## The total precipitation = 90 * fw * mu which requires a transform from EOF to
      ## a field object
      zmap1 <- Z4[[(rcp-1)*4+season + 12]]
      zmap2 <- Z4[[(rcp-1)*4+season + 24]]
      if (FUN=="change") {
        if (it[1]==it[2]) it <- c(it[1],it[1]+(it0[2]-it0[1]))
        print('Estimate change for total precipitation'); print(exists('zmap1')); print(c(it,it0))
        dzmap1 <-  map(zmap1,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
        dzmap2 <-  map(zmap2,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
        dzmap01 <- map(zmap1,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
        dzmap02 <- map(zmap2,FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
        zmap <- dzmap1
        coredata(zmap) <- 90*(coredata(dzmap1)*coredata(dzmap2) - coredata(dzmap01)*coredata(dzmap02))
        it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
        rm('dzmap1','dzmap2','dzmap01','dzmap02')
      }
      if (FUN=="trend") { 
        itx <- c(it[1],max(c(it[2],it[1]+30)))
        zmap1 <- map(zmap1,FUN=FUN,FUNX=FUNX,it=itx,is=is,im=im,plot=FALSE)
        zmap2 <- map(zmap2,FUN=FUN,FUNX=FUNX,it=itx,is=is,im=im,plot=FALSE)
        zmap <- zmap1
        coredata(zmap) <- 90*coredata(zmap1)*coredata(zmap2) 
        it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
        rm('zmap1','zmap2')
      } else {
        zmap1 <- map(zmap1,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
        zmap2 <- map(zmap2,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
        zmap <- zmap1
        coredata(zmap) <- 90*coredata(zmap1)*coredata(zmap2) 
        it <- NULL; im <- NULL; is <- NULL; FUN='mean'; FUNX='mean'
        rm('zmap1','zmap2')
      }
      attr(zmap,'variable') <- 'precip'
      attr(zmap,'unit') <- 'mm/season'
      str(zmap)
    }
    
    y <- map(zmap,FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
    
    map(y,main=main,FUN="mean",new=FALSE)
    }, height=function(){0.8*session$clientData$output_maps_width})
   
  ## Plot individual station
  output$plot <- renderPlot({
    if(!is.null(input$location2)) {
      season <- switch(tolower(as.character(input$season2)),
                       'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
      rcp <- switch(tolower(as.character(input$rcp2)),
                    'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
      param <- switch(tolower(as.character(input$param2)),
                      'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24,
                      'precip. sum'=-1)
      #if (param==0) obs <- t2m else obs <- rr
      fun <- switch(tolower(as.character(input$param2)),
                    'temperature'='mean','wet-day freq.'='wetfreq','precip. intensity'='wetmean')
      if (param>=0) {
        li <- (rcp-1)*4+season + param
      } else {
        li <- (rcp-1)*4+season + 12
      }
    
      locs2 <- switch(tolower(as.character(input$param2)),
               "temperature" = tmax.locs,
               "wet-day freq." = pre.locs,
               "precip. intensity" = pre.locs,
               "precip. sum"=pre.locs)
      
      if (param==0) {
        is <- srt.t2m[is.element(locs2,as.character(input$location2))]
      } else {
        is <- srt.pre[is.element(locs2,as.character(input$location2))]
      }
    
      gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    
      zz <- Z4[[li]]; zz$eof <- NULL;
      ## Reduce the matrix size and pick one station before the recovery of the original format
      zz$pca <- subset(zz$pca,is=is)
      im <- is.element(gcmnames,input$im)
      z <- as.station(zz,im=im,verbose=FALSE)
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
      #y <- subset(obs,is=is)
      #y <- subset(as.4seasons(y,FUN=fun),it=c('djf','mam','jja','son')[season])
      #main <- paste(is,li,sum(im),sum(is.finite(coredata(z))),index(z)[1],paste(class(z),collapse='-'))
      main <- paste(is,li,sum(im),index(z)[1],paste(class(z),collapse='-'))
      #plot(z,main=main,obs.show=FALSE,target.show=FALSE,legend.show=FALSE,new=FALSE)
      plot(z,main=main,target.show=FALSE,legend.show=FALSE,new=FALSE,
           xrange=range(lon(zz$pca)),yrange=range(lat(zz$pca)),
           map.show=TRUE,usegooglemap=FALSE,verbose=FALSE)
    }
    #index(y) <- year(y)
    #lines(y,type='b',lwd=3,cex=1.2)
    #plot(rnorm(100),main=main)
  }, height=function(){0.65*session$clientData$output_plot_width}) #600})
  
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
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
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
      gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
      im <- is.element(gcmnames,input$im)
      locs4 <- switch(input$param4,
                      "Temperature" = tmax.locs,
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
      zn <- floor(min(c(abs(z),input$threshold4),na.rm=TRUE))-1
      breaks <- switch(input$param4,
                       "Temperature" = seq(zn,zx,by=0.1),
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
  
  ## Show thedifference between one selected model and the mean of the rest of the ensemble.
  output$plot1model <- renderPlot({ 
    it <- range(as.numeric(input$dates7))
    is <- list(lon=as.numeric(input$lon7),lat=as.numeric(input$lat7))
    season <- switch(tolower(as.character(input$season7)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp7)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    param <- switch(tolower(as.character(input$param7)),
                    'temperature'=0,'wet-day freq.'=12,'precip. intensity'=24)
    li <- (rcp-1)*4+season+param
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    im1 <- is.element(gcmnames,input$im7)
    im <-  !is.element(gcmnames,input$im7) & is.element(gcmnames,input$im) 
    
    z1 <- subset(Z4[[li]],im=im1,it=it,is=is)
    zz <- subset(Z4[[li]],im=im,it=it,is=is)
    
    y1 <- map(z1,plot=FALSE)
    yy <- map(zz,plot=FALSE)
    coredata(y1) <- coredata(y1) - coredata(yy)
    main <- paste(gcmnames[im1],' - ensemble mean (number of runs=',sum(im),') ',
                  season,'/',input$rcp1,': ',it[1],'-',it[2],sep='')
    map(y1,main=main,new=FALSE)
    #plot(rnorm(100),main=main)
   },height=function(){0.8*session$clientData$output_plot1model_width} )#600})
  
  output$plotndays <- renderPlot({
    if(!is.null(input$threshold8)) {
      season <- switch(tolower(as.character(input$direction8)),
                       "cold winter days"=1,"hot summer days"=3)
      rcp <- switch(tolower(as.character(input$rcp8)),
                    'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
      li <- (rcp-1)*4+season
      gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
      im <- is.element(gcmnames,input$im)
      is <- srt.t2m[is.element(tmax.locs,as.character(input$location8))]
      is0 <- (1:length(loc(t2m)))[is.element(loc(t2m),as.character(input$location8))]
      zz <- Z4[[li]]; zz$eof <- NULL;
      class(zz) <- c('dsensemble','pca','season','list')
      lons <- lon(zz$pca); lats <- lat(zz$pca); alts <- alt(zz$pca)
      zz <- subset(zz,im=im,is=is)
      z <- as.station(zz)
      if (as.character(input$direction8) == "Hot summer days") {
        nds <- hotsummerdays(subset(t2m,is=is0),dse=z,
                             it=c('djf','mam','jja','son')[season],
                             threshold=input$threshold8,plot=FALSE)
      } else {
        nds <- coldwinterdays(subset(t2m,is=is0),dse=z,
                              it=c('djf','mam','jja','son')[season],
                              threshold=input$threshold8,plot=FALSE)
      }
      plot(nds)
    }
  },height=function(){0.5*session$clientData$output_plotndays_width} )#600})
  
  output$use.stats <- renderText({
    txt <- paste(countview$i,"actions")
  })
  
})
