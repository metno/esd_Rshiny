## Test script:

setwd('~/R/Rshiny/AACA-ESD-Atlas/')
projection <- list()
print('djf')
load('dse.aaca.t2m.rcp45.djf.eof.rda')
projection$djf <- Z
print('mam')
load('dse.aaca.t2m.rcp45.mam.eof.rda')
projection$mam <- Z
print('jja')
load('dse.aaca.t2m.rcp45.jja.eof.rda')
projection$jja <- Z
print('son')
load('dse.aaca.t2m.rcp45.son.eof.rda')
projection$son <- Z

print('map')
map(projection[[1]])