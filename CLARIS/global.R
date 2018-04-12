## Theesd-package is available from github.com/metno/esd
library(esd)

## Read the station data ...
load('data/claris.Pr.rda')
load('data/claris.Tx.rda')
load('data/claris.Tn.rda')

## Daily precipitation
pre <- Pr
## Filter out unrealistic values
coredata(pre)[coredata(pre) > 600] <- NA 
## Order alphabetically
srt <- order(esd::loc(pre))
coredata(pre) <- coredata(pre[,srt])
attr(pre,'location') <- loc(pre)[srt]; attr(pre,'station_id') <- stid(pre)[srt]
attr(pre,'longitude') <- lon(pre)[srt]; attr(pre,'latitude') <- lat(pre)[srt]; 
attr(pre,'altitude') <- alt(pre)[srt]; 

## Daily maximum temperature
srt <- order(esd::loc(Tx))
tmax <- Tx; coredata(tmax) <- coredata(tmax)[,srt] 
attr(tmax,'location') <- loc(tmax)[srt]; attr(tmax,'station_id') <- stid(tmax)[srt]
attr(tmax,'longitude') <- lon(tmax)[srt]; attr(tmax,'latitude') <- lat(tmax)[srt]
attr(tmax,'altitude') <- alt(tmax)[srt]; 

## Daily minimum temperature
srt <- order(esd::loc(Tn))
tmin <- Tn; coredata(tmin) <- coredata(tmin)[,srt] 
attr(tmin,'location') <- loc(tmin)[srt]; attr(tmin,'station_id') <- stid(tmin)[srt]
attr(tmin,'longitude') <- lon(tmin)[srt]; attr(tmin,'latitude') <- lat(tmin)[srt]
attr(tmin,'altitude') <- alt(tmin)[srt]; 
attr(tmin,'unit') <- 'degC'

## Initial values
y <- pre
location <- loc(y)
station_id <- stid(y)
startyr <- as.numeric(firstyear(y))
endyr <- as.numeric(lastyear(y))



