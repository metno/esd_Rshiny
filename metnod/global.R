## Theesd-package is available from github.com/metno/esd
library(esd)

## Read the station data ...
#load('data/claris.Pr.rda')
#load('data/claris.Tx.rda')
#load('data/claris.Tn.rda')

load('data/tmax.stats.rda')
load('data/tmin.stats.rda')
load('data/t2m.stats.rda')
load('data/precip.stats.rda')

## Daily precipitation
pre <- Pr
meta <- tmax.stats
## Filter out unrealistic values
coredata(pre)[coredata(pre) > 600] <- NA 

## Initial values
y <- pre
location <- meta$loc
location <- tolower(sub('\xc3','',location))
station_id <- meta$stid
startyr <- meta$firstyear
endyr <- meta$lastyear



