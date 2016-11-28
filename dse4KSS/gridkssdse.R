## Take the dse-results for AACA and grid them into maps

library(esd)
demo(pca2eof,ask=FALSE)

files <- list.files(pattern='dse.kss',path='data',full.names = TRUE)
files <- files[nchar(files)==29]
print(files)

n <- length(files)

for (i in 1:n) {
  print(files[i])
  load(files[i])
  if (dim(Z$pca)[2] > 4) z <- subset(Z,ip=1:4) else z <- Z
  print('subset')
  Z <- as.eof.dsensemble.pca(z)
  print('as eof')
  eoffile <- paste(substr(files[i],1,nchar(files[i])-4),'.eof.rda',sep='')
  print(eoffile)
  save(Z,file=eoffile)
  file.remove(files[i])
}
