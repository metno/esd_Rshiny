#locations <- select.station(param=c('t2m','precip'),src='metnod')#,cntr='norway',nmin=100)

#Read precipitation data ...
#load('data/precip.norway.ecad.rda')
load('data/pre.nordic.rda')
locations <- precip
coredata(pre)[coredata(pre) > 500] <- NA 
locations <- pre
y <- subset(locations,is=which(!duplicated(stid(locations))))
y <- subset(y,is=which(!duplicated(loc(y))))

vals <- as.numeric(colMeans(coredata(y),na.rm=TRUE))
vals <- apply(y,2,FUN='wetfreq',threshold = 0.1)
vals <- round(vals, digits = 2)

longitude <- attr(y,'longitude')
latitude <- attr(y,'latitude')
altitude <- attr(y,'altitude')
station_id <- attr(y,'station_id')
location <- attr(y,'location')
