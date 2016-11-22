## Take the dse-results for AACA and grid them into maps

library(esd)
demo(pca2eof,ask=FALSE)

setwd('data')
files <- list.files(pattern='dse.kss')
print(files)

n <- length(files)

for (i in 1:n) {
  print(files[i])
  load(files[i])
  if (nchar(files[i])==27) nl <- 11 else nl <- 8
  #eval(parse(text=paste('z <- dse.aaca.',
  #               substr(files[i],9,nchar(files[i])-nl),sep='')))
  z <- dse.aaca.t2m.rcp45
  print('z')
  z <- subset(z,ip=1:4)
  print('subset')
  Z <- as.eof.dsensemble.pca(z)
  print('as eof')
  eoffile <- paste(substr(files[i],1,nchar(files[i])-4),'.eof.rda',sep='')
  print(eoffile)
  save(Z,file=eoffile)
}
setwd('..')
