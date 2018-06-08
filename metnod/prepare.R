## Prepare statistics

library(esd)

lastrains <- function(x,x0=1,uptodate=TRUE,verbose=FALSE) {
  y <- cumsum(rev(coredata(x)))
  z <- sum(y < x0)
  if (uptodate) if (Sys.Date() - end(x) > 1) z <- NA 
  return(z)
}

## Maximim temperature
Tx <- retrieve.station('~/git/esd_Rshiny/metnod/data/tmax.metnod.nc')
ave <- apply(Tx,2,'mean',na.rm=TRUE)
std <- apply(anomaly(Tx),2,'sd',na.rm=TRUE)
mx <- apply(Tx,2,'min',na.rm=TRUE)
mn <- apply(Tx,2,'max',na.rm=TRUE)
td <- apply(annual(Tx),2,'trend.coef')
nv <- apply(Tx,2,'nv')
ly <- lastyear(Tx)
fy <- firstyear(Tx)
nrh <- apply(anomaly(Tx),2,function(x) length(records(x))/log(sum(is.finite(x))))
nrl <- apply(-anomaly(Tx),2,function(x) length(records(x))/log(sum(is.finite(x))))

tmax.stats <- data.frame(mean=ave,std=std,max=mx,min=mn,trend=td,
                         lon=lon(Tx),lat=lat(Tx),alt=alt(Tx),
                         loc=loc(Tx),stid=stid(Tx),lastyear=ly,
                         lastyear=ly,nv=nv,n.warm.records=nrh,
                         n.cold.records=nrl)
attr(tmax.stats,'period') <- paste(start(Tx),end(Tx),sep=' - ')

## Fix illigal characters:
tmax.stats$loc <- sub('\xc3','',tmax.stats$loc)

## Order alphabetically
locs <- tolower(tmax.stats$loc); locs <- sub('å','zzzå',locs)
locs <- sub('æ','zzæ',locs); locs <- sub('ø','zzzø',locs)
srt <- order(locs)
tmax.stats <- tmax.stats[srt,]

save(tmax.stats,file='~/git/esd_Rshiny/metnod/data/tmax.stats.rda')
rm('Tx'); gc(reset=TRUE)

## Tmin

Tn <- retrieve.station('~/git/esd_Rshiny/metnod/data/tmin.metnod.nc')
ave <- apply(Tn,2,'mean',na.rm=TRUE)
std <- apply(anomaly(Tn),2,'sd',na.rm=TRUE)
mx <- apply(Tn,2,'min',na.rm=TRUE)
mn <- apply(Tn,2,'max',na.rm=TRUE)
td <- apply(annual(Tn),2,'trend.coef')
nv <- apply(Tn,2,'nv')
ly <- lastyear(Tn)
fy <- firstyear(Tn)
nrh <- apply(anomaly(Tn),2,function(x) {x <- x[x>0]; length(records(x))/log(sum(is.finite(x)))})
nrl <- apply(-anomaly(Tn),2,function(x) {x <- x[x>0]; length(records(x))/log(sum(is.finite(x)))})

tmin.stats <- data.frame(mean=ave,std=std,max=mx,min=mn,trend=td,
                         lon=lon(Tn),lat=lat(Tn),alt=alt(Tn),
                         loc=loc(Tn),stid=stid(Tn),lastyear=ly,
                         lastyear=ly,nv=nv,n.warm.records=nrh,
                         n.cold.records=nrl)
attr(tmin.stats,'period') <- paste(start(Tn),end(Tn),sep=' - ')

## Fix illigal characters:
tmin.stats$loc <- sub('\xc3','',tmin.stats$loc)

## Order alphabetically
locs <- tolower(tmin.stats$loc); locs <- sub('å','zzzå',locs)
locs <- sub('æ','zzæ',locs); locs <- sub('ø','zzzø',locs)
srt <- order(locs)
tmin.stats <- tmin.stats[srt,]
save(tmin.stats,file='~/git/esd_Rshiny/metnod/data/tmin.stats.rda')
rm('Tn'); gc(reset=TRUE)
## T2m

T2m <- retrieve.station('~/git/esd_Rshiny/metnod/data/t2m.metnod.nc')
ave <- apply(T2m,2,'mean',na.rm=TRUE)
std <- apply(anomaly(T2m),2,'sd',na.rm=TRUE)
mx <- apply(T2m,2,'min',na.rm=TRUE)
mn <- apply(T2m,2,'max',na.rm=TRUE)
td <- apply(annual(T2m),2,'trend.coef')
nv <- apply(T2m,2,'nv')
ly <- lastyear(T2m)
fy <- firstyear(T2m)
nrh <- apply(anomaly(T2m),2,function(x) length(records(x))/log(sum(is.finite(x))))
nrl <- apply(-anomaly(T2m),2,function(x) length(records(x))/log(sum(is.finite(x))))

t2m.stats <- data.frame(mean=ave,std=std,max=mx,min=mn,trend=td,
                         lon=lon(T2m),lat=lat(T2m),alt=alt(T2m),
                         loc=loc(T2m),stid=stid(T2m),lastyear=ly,
                         lastyear=ly,nv=nv,n.warm.records=nrh,
                         n.cold.records=nrl)
attr(t2m.stats,'period') <- paste(start(T2m),end(T2m),sep=' - ')

## Fix illigal characters:
t2m.stats$loc <- sub('\xc3','',t2m.stats$loc)

## Order alphabetically
locs <- tolower(t2m.stats$loc); locs <- sub('å','zzzå',locs)
locs <- sub('æ','zzæ',locs); locs <- sub('ø','zzzø',locs)
srt <- order(locs)
t2m.stats <- t2m.stats[srt,]
save(t2m.stats,file='~/git/esd_Rshiny/metnod/data/t2m.stats.rda')
rm('T2m'); gc(reset=TRUE)

## Precipitation

Pr <- retrieve.station('~/git/esd_Rshiny/metnod/data/precip.metnod.nc',
                       lat=c(58,62),lon=c(5,15))
ave <- apply(Pr,2,'mean',na.rm=TRUE)
std <- apply(anomaly(Pr),2,'sd',na.rm=TRUE)
mx <- apply(Pr,2,'min',na.rm=TRUE)
mn <- apply(Pr,2,'max',na.rm=TRUE)
td <- apply(annual(Pr,FUN='sum'),2,'trend.coef')
tdwf <- apply(annual(Pr,FUN='wetfreq'),2,'trend.coef')
tdmu <- apply(annual(Pr,FUN='wetmean'),2,'trend.coef')
nv <- apply(Pr,2,'nv')
ly <- lastyear(Pr)
fy <- firstyear(Pr)
nr <- apply(Pr,2,function(x) {x <- x[x > 0]; length(records(x))/log(sum(is.finite(x)))})
lr <- apply(Pr,2,'lastrains')

precip.stats <- data.frame(mean=ave,std=std,max=mx,min=mn,
                           trend = td,fw.trend=tdwf,mu.trend=tdmu,
                         lon=lon(Pr),lat=lat(Pr),alt=alt(Pr),
                         loc=loc(Pr),stid=stid(Pr),lastyear=ly,
                         lastyear=ly,nv=nv,n.records=nr,
                         lastrain=lr)
attr(precip.stats,'period') <- paste(start(Pr),end(Pr),sep=' - ')

## Fix illigal characters:
precip.stats$loc <- sub('\xc3','',precip.stats$loc)

## Order alphabetically
locs <- tolower(precip.stats$loc); locs <- sub('å','zzzå',locs)
locs <- sub('æ','zzæ',locs); locs <- sub('ø','zzzø',locs)
srt <- order(locs)
precip.stats <- precip.stats[srt,]
save(precip.stats,file='~/git/esd_Rshiny/metnod/data/precip.stats.rda')
rm('Pr'); gc(reset=TRUE)