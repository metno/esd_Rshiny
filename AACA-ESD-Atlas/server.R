## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)
#if ('RgoogleMaps' %in% installed.packages()) install.packages('RgoogleMaps')
library(RgoogleMaps)

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

load('data/t2m.aaca.rda')

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

iview <- 0

shinyServer(function(input, output) {
  countview <- reactiveValues(i = 1)
  
  ## Show map of gridded temperature
  output$maps <- renderPlot({ 
    it <- range(as.numeric(input$dates1))
    is <- list(lon=as.numeric(input$lon1),lat=as.numeric(input$lat1))
    season <- switch(tolower(as.character(input$season1)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp1)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    FUN <- switch(tolower(as.character(input$aspect)),
                  "mean value"="mean", "change"="change","trend"="trend","variability"="sd")
    FUNX <- switch(tolower(as.character(input$stats)),
                   "ensemble mean"="mean", "ensemble spread"="sd")
    
    li <- (rcp-1)*4+season
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    im <- is.element(gcmnames,input$im)
    
    if (FUN=="trend") it[2] <- max(c(it[2],it[1]+30))
    main <- paste('Map of',FUN,tolower(input$param1),'for',season,'and scenario',
                  toupper(input$rcp1),'and period',it[1],'-',it[2],
                  'based on',sum(im),'model runs')
    
    y <- map(Z4[[li]],it=it,is=is,im=im,plot=FALSE)
    
    if (FUN=="change") {
      y <- map(Z4[[li]],FUN="mean",FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
      it0 <- range(as.numeric(input$baseline))
      it0 <- c(1961,1990)
      y0 <- map(Z4[[li]],FUN="mean",FUNX=FUNX,it=it0,is=is,im=im,plot=FALSE)
      coredata(y) <- t(coredata(t(y)) - apply(coredata(t(y0)),1,FUN='mean'))
    } else y <- map(Z4[[li]],FUN=FUN,FUNX=FUNX,it=it,is=is,im=im,plot=FALSE)
    
    map(y,main=main,FUN="mean",new=FALSE)
    
    }, height=function(){600})
   
  ## Plot individual station
  output$plot <- renderPlot({
    season <- switch(tolower(as.character(input$season2)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp2)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    is <- (1:length(locs))[is.element(locs,as.character(input$location2))]
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    zz <- Z4[[li]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','list')
    ## Reduce the matrix size and pick one station before the recovery of the original format
    zz$pca <- subset(zz$pca,is=is)
    im <- is.element(gcmnames,input$im)
    z <- as.station(zz,im=im)
    main <- paste('ensemble of',sum(im),'model runs with',
                  sum(is.finite(coredata(z))),index(z)[1],'valid data')
    plot(z,main=main,target.show=FALSE,new=FALSE)
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
    main <- paste('Ensemble mean of',input$param3,'for',season,'and',
                  toupper(input$rcp3),'scenario and',sum(im),'model runs')
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
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    im <- is.element(gcmnames,input$im)
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
    main <- paste(input$location4,'using',sum(im),'model runs: the probability of',
                  tolower(input$direction4),'than',round(input$threshold4,2),
                  'is',round(100*prob,2))
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
    im <- is.element(gcmnames,input$im)
    zz <- Z4[[li]]
    class(zz) <- c('dsensemble','pca','list')
    ## Reduce the matrix size and pick one station before the recovery of the original format
    im <- is.element(gcmnames,input$im)
    zz <- subset(zz,is=is,im=im)
    z <- as.station(zz)
    main <- paste('Quality of',input$quality6,'for',input$season6,input$param6,'and',sum(im),input$rcp6,'model runs')
    plot(lon(zz$pca),lat(zz$pca),xlab='',ylab='',main=main,lwd=2,cex=2,pch=19,col='grey80')
    grid()
    data(geoborders)
    lines(geoborders,col='grey')
    if (as.character(input$quality6)=="trend") 
      score<- unlist(lapply(z,trendscore)) else
    if (as.character(input$quality6)=="inter-annual range") 
      score<- unlist(lapply(z,varscore))  
    dim(score) <- c(3,length(z))
    col <- rep(rgb(0,0.5,0.5),length(z)); pch <- rep(19,length(z))
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
    par(par0)
    },height=function(){600})
  
  ## Show thedifference between one selected model and the mean of the rest of the ensemble.
  output$plot1model <- renderPlot({ 
    it <- range(as.numeric(input$dates7))
    is <- list(lon=as.numeric(input$lon7),lat=as.numeric(input$lat7))
    season <- switch(tolower(as.character(input$season7)),
                     'winter'=1,'spring'=2,'summer'=3,'autumn'=4)
    rcp <- switch(tolower(as.character(input$rcp7)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
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
  }, height=function(){600})
  
  output$plotndays <- renderPlot({ 
    season <- switch(tolower(as.character(input$direction8)),
                     "cold winter days"=1,"hot summer days"=3)
    rcp <- switch(tolower(as.character(input$rcp8)),
                  'rcp4.5'=1,'rcp2.6'=2,'rcp8.5'=3)
    li <- (rcp-1)*4+season
    gcnames <- names(Z4[[li]])[-c(1,2,length(Z4[[1]]))]
    im <- is.element(gcmnames,input$im)
    is <- (1:length(locs))[is.element(locs,as.character(input$location8))]
    zz <- Z4[[li]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','season','list')
    lons <- lon(zz$pca); lats <- lat(zz$pca); alts <- alt(zz$pca)
    zz <- subset(zz,im=im,is=is)
    z <- as.station(zz)
    if (as.character(input$direction8) == "Hot summer days") {
      nds <- hotsummerdays(subset(t2m,is=is),dse=z,it=c('djf','mam','jja','son')[season],threshold=input$threshold8,plot=FALSE)
    } else {
      nds <- coldwinterdays(subset(t2m,is=is),dse=z,it=c('djf','mam','jja','son')[season],threshold=input$threshold8,plot=FALSE)
    }
    plot(nds)
  })
  
  output$use.stats <- renderText({
    txt <- paste(countview$i,"actions")
  })
  
})
