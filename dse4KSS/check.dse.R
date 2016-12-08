## Test the downscaling for wet-day frequency
library(esd)

param <- 't2m'
i <- 13
ip <- 1


files <- list.files(pattern='dse.kss',path='~/R/Rshiny/dse4KSS/data',full.names=TRUE)
files <- files[grep(param,files)]
print(files[i])

load(files[i])
pca <- zoo(Z$pca[,ip])
print(Z$info); print(attr(Z,'predictor_file')); print(attr(Z,'predictor_lon')); print(attr(Z,'predictor_lat'))
Z$info <- NULL; Z$pca <- NULL; Z$eof <- NULL
x <- unlist(lapply(Z,function(x) coredata(x)[,ip]))
dim(x) <- c(length(Z[[1]][,1]),length(Z))
x <- zoo(x,order.by=index(Z[[1]]))
plot(x,plot.type='single',col=rgb(1,0.3,0.3,0.1))
lines(pca)
