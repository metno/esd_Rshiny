## Rasmus Benestad, Met Norway, 2016-11-02
## R-shiny app that presents empirical-statistical downscaled results. The results include the CMIP5 ensemble simulations
## for RCP4.5, RCP2.6, and RCP 8.5 for a number of stations and for the four different seasons. PCAs and EOFs have been used
## to minimise the needed data volume, and this app expand information embedded in the PCAs/EOFs to corresponding information
## in the form of station series or gridded maps.

library(shiny)
library(esd)
library(leaflet)
library(raster)
library(rgdal)
library(shinydashboard)

#if ('RgoogleMaps' %in% installed.packages()) install.packages('RgoogleMaps')
#library(RgoogleMaps)

initialExp <- TRUE

## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.

## for Input data
Z4 <- list()
load('data/dse.kss.t2m.rcp45.djf.eof.rda')
Z4$tas <- Z
load('data/dse.kss.mu.rcp45.djf.eof.rda')
Z4$pre <- Z
t2m.locs <- sort(loc(Z4[[1]]$pca))
pre.locs <- sort(loc(Z4[[2]]$pca))
#gcmnames <<- names(Z4[[1]])[-c(1,2,length(Z4[[1]]))]
locs2 <- t2m.locs

## For the server
Z4 <- list()
load('data/dse.kss.t2m.rcp45.djf.eof.rda')
Z4$t2m.djf.45 <- Z
load('data/dse.kss.t2m.rcp45.mam.eof.rda')
Z4$t2m.mam.45 <- Z
load('data/dse.kss.t2m.rcp45.jja.eof.rda')
Z4$t2m.jja.45 <- Z
load('data/dse.kss.t2m.rcp45.son.eof.rda')
Z4$t2m.son.45 <- Z
gcmnames.45 <- names(Z)[grep('_',names(Z))]

load('data/dse.kss.t2m.rcp26.djf.eof.rda')
Z4$t2m.djf.26 <- Z
load('data/dse.kss.t2m.rcp26.mam.eof.rda')
Z4$t2m.mam.26 <- Z
load('data/dse.kss.t2m.rcp26.jja.eof.rda')
Z4$t2m.jja.26 <- Z
load('data/dse.kss.t2m.rcp26.son.eof.rda')
Z4$t2m.son.26 <- Z
gcmnames.26 <- names(Z)[grep('_',names(Z))]

load('data/dse.kss.t2m.rcp85.djf.eof.rda')
Z4$t2m.djf.85 <- Z
load('data/dse.kss.t2m.rcp85.mam.eof.rda')
Z4$t2m.mam.85 <- Z
load('data/dse.kss.t2m.rcp85.jja.eof.rda')
Z4$t2m.jja.85 <- Z
load('data/dse.kss.t2m.rcp85.son.eof.rda')
Z4$t2m.son.85 <- Z
gcmnames.85 <- names(Z)[grep('_',names(Z))]

## Wet-day frequency
load('data/dse.kss.fw.rcp45.djf.eof.rda')
Z4$fw.djf.45 <- Z
load('data/dse.kss.fw.rcp45.mam.eof.rda')
Z4$fw.mam.45 <- Z
load('data/dse.kss.fw.rcp45.jja.eof.rda')
Z4$fw.jja.45 <- Z
load('data/dse.kss.fw.rcp45.son.eof.rda')
Z4$fw.son.45 <- Z

load('data/dse.kss.fw.rcp26.djf.eof.rda')
Z4$fw.djf.26 <- Z
load('data/dse.kss.fw.rcp26.mam.eof.rda')
Z4$fw.mam.26 <- Z
load('data/dse.kss.fw.rcp26.jja.eof.rda')
Z4$fw.jja.26 <- Z
load('data/dse.kss.fw.rcp26.son.eof.rda')
Z4$fw.son.26 <- Z

load('data/dse.kss.fw.rcp85.djf.eof.rda')
Z4$fw.djf.85 <- Z
load('data/dse.kss.fw.rcp85.mam.eof.rda')
Z4$fw.mam.85 <- Z
load('data/dse.kss.fw.rcp85.jja.eof.rda')
Z4$fw.jja.85 <- Z
load('data/dse.kss.fw.rcp85.son.eof.rda')
Z4$fw.son.85 <- Z

## Wet-day mean precipitation
load('data/dse.kss.mu.rcp45.djf.eof.rda')
Z4$mu.djf.45 <- Z
load('data/dse.kss.mu.rcp45.mam.eof.rda')
Z4$mu.mam.45 <- Z
load('data/dse.kss.mu.rcp45.jja.eof.rda')
Z4$mu.jja.45 <- Z
load('data/dse.kss.mu.rcp45.son.eof.rda')
Z4$mu.son.45 <- Z

load('data/dse.kss.mu.rcp26.djf.eof.rda')
Z4$mu.djf.26 <- Z
load('data/dse.kss.mu.rcp26.mam.eof.rda')
Z4$mu.mam.26 <- Z
load('data/dse.kss.mu.rcp26.jja.eof.rda')
Z4$mu.jja.26 <- Z
load('data/dse.kss.mu.rcp26.son.eof.rda')
Z4$mu.son.26 <- Z

load('data/dse.kss.mu.rcp85.djf.eof.rda')
Z4$mu.djf.85 <- Z
load('data/dse.kss.mu.rcp85.mam.eof.rda')
Z4$mu.mam.85 <- Z
load('data/dse.kss.mu.rcp85.jja.eof.rda')
Z4$mu.jja.85 <- Z
load('data/dse.kss.mu.rcp85.son.eof.rda')
Z4$mu.son.85 <- Z

load('data/t2m.nordic.rda')
t2m <- Y
load('data/rr.nordic.rda')
rr <- Y
rm('Y')
load('data/quality.rda')
srt.t2m <- order(loc(Z4[[1]]$pca))
srt.pre <- order(loc(Z4[[13]]$pca))                 
t2m.locs <- sort(loc(Z4[[1]]$pca))
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

