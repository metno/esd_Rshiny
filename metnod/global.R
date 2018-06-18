## Theesd-package is available from github.com/metno/esd
library(esd)

## Read summary station statistics ...
load('data/tmax.stats.rda')
load('data/tmin.stats.rda')
load('data/t2m.stats.rda')
load('data/precip.stats.rda')

## Daily precipitation
meta <- precip.stats

## Initial values
print(meta$stid)
print('retrieve.station:')
y <- retrieve.station('data/precip.metnod.nc',stid=meta$stid[1])

print(loc(y)); print(nv(y))

## Filter out unrealistic values
#coredata(pre)[coredata(pre) > 600] <- NA 

## Get the names of locations, etc.
locations <- meta$loc
locations <- tolower(sub('\xc3','',locations))
station_id <- meta$stid
startyr <- meta$firstyear
endyr <- meta$lastyear
sta <- names(meta)



