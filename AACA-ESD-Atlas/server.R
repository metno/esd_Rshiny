## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)
if ('RgoogleMaps' %in% installed.packages()) library(RgoogleMaps)

## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.

Z4 <- list()
load('data/dse.aaca.t2m.rcp45.djf.eof.rda')
Z4$djf.45 <- Z
load('data/dse.aaca.t2m.rcp45.mam.eof.rda')
Z4$mam.45 <- Z
load('data/dse.aaca.t2m.rcp45.jja.eof.rda')
Z4$jja.45 <- Z
load('data/dse.aaca.t2m.rcp45.son.eof.rda')
Z4$son.45 <- Z

load('data/dse.aaca.t2m.rcp26.djf.eof.rda')
Z4$djf.26 <- Z
load('data/dse.aaca.t2m.rcp26.mam.eof.rda')
Z4$mam.26 <- Z
load('data/dse.aaca.t2m.rcp26.jja.eof.rda')
Z4$jja.26 <- Z
load('data/dse.aaca.t2m.rcp26.son.eof.rda')
Z4$son.26 <- Z

load('data/dse.aaca.t2m.rcp85.djf.eof.rda')
Z4$djf.85 <- Z
load('data/dse.aaca.t2m.rcp85.mam.eof.rda')
Z4$mam.85 <- Z
load('data/dse.aaca.t2m.rcp85.jja.eof.rda')
Z4$jja.85 <- Z
load('data/dse.aaca.t2m.rcp85.son.eof.rda')
Z4$son.85 <- Z


shinyServer(function(input, output) {
  
  ## Reactive settings for the menu
#  Li <- reactive({((1:3)[is.element(c('rcp4.5','rcp2.6','rcp8.5'),tolower(input$rcp))-1]*4) +
#                   (1:4)[is.element(c('winter','spring','summer','autumn'),tolower(input$season))]})
#  Gcmnames <<- reactive({names(Z4[[Li()]])[-c(1,2,length(Z4[[Li()]]))]})
#  Locs <<- reactive({loc(Z4[[Li()]]$pca)})
  
  ## Show map of gridded temperature
  output$maps <- renderPlot({ 
    it <- range(as.numeric(input$dates1))
    is <- list(lon=as.numeric(input$lon1),lat=as.numeric(input$lat1))
    season <- switch(tolower(as.character(input$season1)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp1)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    gcnames <- names(Z4[[li]])
    im <- is.element(input$im,gcmnames)
    main <- paste(input$param1,season,input$rcp1,li,it[1],it[2],is$lon1[1],is$lon1[2],is$lat1[1],is$lat1[2],sum(im))
    map(Z4[[li]],it=it,is=is,main=main,new=FALSE)
    }, height=function(){600})
   
  ## Plot individual station
  output$plot <- renderPlot({
    season <- switch(tolower(as.character(input$season2)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp2)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    is <- (1:length(locs))[is.element(locs,as.character(input$location2))]
    gcnames <- names(Z4[[li]])
    zz <- Z4[[li]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','list')
    ## Reduce the matrix size and pick one station before the recovery of the original format
    zz$pca <- subset(zz$pca,is=is)
    im <- is.element(input$im,gcmnames)
    z <- as.station(zz,im=im)
    main <- paste(is,li,sum(im),sum(is.finite(coredata(z))),index(z)[1],paste(class(z),collapse='-'))
    plot(z,main=main,new=FALSE)
    #plot(rnorm(100),main=main)
  }, height=function(){600})
  
  ## Plot ensemble statistics for multiple-stations
  output$plot.multi <- renderPlot({ 
    it <- range(as.numeric(input$dates3))
    season <- switch(tolower(as.character(input$season3)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp3)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    gcnames <- names(Z4[[li]])
    im <- is.element(input$im,gcmnames)
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
    plot(z,main=main,new=FALSE)
    },height=function(){600})
  
  ## Show a map of evaluation scores
  ## Central parts of circle shows trend similarity
  ## Outer rim shows the number outside the 90% confidence interval
  
  output$plot.prob <- renderPlot({
    it <- range(as.numeric(input$dates4))
    season <- switch(tolower(as.character(input$season4)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp4)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    gcnames <- names(Z4[[li]])
    im <- is.element(input$im,gcmnames)
    is <- (1:length(locs))[is.element(locs,as.character(input$location4))]
    zz <- Z4[[li]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','season','list')
    lons <- lon(zz$pca); lats <- lat(zz$pca); alts <- alt(zz$pca)
    zz <- subset(zz,im=im,is=is)
    z <- as.station(zz)
    z <- c(as.numeric(subset(z,it=it)))
    zx <- ceiling(max(c(abs(z),abs(input$threshold4)),na.rm=TRUE))+1
    breaks <- seq(-zx,zx,by=0.5)
    if (as.character(input$direction4) == "Colder") 
      prob <- pnorm(input$threshold4,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)) else
      prob <- 1 - pnorm(input$threshold4,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE))
    main <- paste(input$location4,is,li,sum(im),round(input$threshold4,2),round(100*prob,2))
    hist(z,breaks=breaks,main=main,new=FALSE,freq=FALSE,col='grey')
    X <- seq(-zx,zx,by=0.05)
    lines(X,dnorm(X,mean(mean(z,na.rm=TRUE)),sd=sd(z,na.rm=TRUE)),
          lwd=5,col=rgb(0.5,0,0,0.3))
    if (as.character(input$direction4) == "Colder") {
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
    })
  
  ## Unfinished!
  output$plot.warmcolddays <- renderPlot({plot(rnorm(100))})
  
  ## Unfinished!  
  output$map.quality <- renderPlot({ 
    season <- switch(tolower(as.character(input$season6)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp6)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    is <- list(lon=as.numeric(input$lon6),lat=as.numeric(input$lat6))
    y <- Z4[[li]]
    map(subset(y$pca,is=is),new=FALSE)},
    height=function(){600})
  
  output$idtext <- renderText({
    txt <- paste(input$param,input$rcp,input$season,input$scenario,input$dates)
  })
  
})
