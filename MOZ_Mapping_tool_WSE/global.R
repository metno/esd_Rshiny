library(esd)
#locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)

#Read precipitation data ...
#load('data/precip.norway.ecad.rda')
#load('data/pre.nordic.rda')
load('data/precip.inam.rda')
pre <- precip.inam
coredata(pre)[coredata(pre) > 600] <- NA 
locations <- pre
#y <- subset(locations,is=which(duplicated(stid(locations))))
#y <- subset(y,is=which(!duplicated(loc(y))))
y <- pre

vals <- as.numeric(colMeans(coredata(esd::annual(y,FUN='sum')),na.rm=TRUE))
#vals <- apply(y,2,FUN='wetfreq',threshold = 0.1)
vals <- round(vals, digits = 2)
names(vals) <- lon(y)

longitude <- esd::lon(y)
latitude <- esd::lat(y)
altitude <- esd::alt(y)
station_id <- esd::stid(y)
location <- esd::loc(y)

yrs <- year(y)
fval <- function(x) apply(x,2,function(x,yrs=yrs) min(yrs[is.finite(x)]),yrs)
lval <- function(x) apply(x,2,function(x,yrs=yrs) max(yrs[is.finite(x)]),yrs)
startyr <- fval(y)
endyr <- lval(y)

