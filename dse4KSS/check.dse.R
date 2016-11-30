## Test the downscaling for wet-day frequency
library(esd)

param <- 'mu'
i <- 7
ip <- 2

files <- list.files(pattern='dse.kss',path='data',full.names=TRUE)
files <- files[grep(param,files)]
print(files[i])

load(files[i])
pca <- zoo(Z$pca[,ip])
print(Z$info)
Z$info <- NULL; Z$pca <- NULL; Z$eof <- NULL
x <- unlist(lapply(Z,function(x) coredata(x)[,ip]))
dim(x) <- c(length(Z[[1]][,1]),length(Z))
x <- zoo(x,order.by=index(Z[[1]]))
plot(x,plot.type='single',col=rgb(1,0.3,0.3,0.1))
lines(pca)
