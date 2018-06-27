## This R-shiny app searches the data directory for netCDF files with station data. These
## netCDF files have been generated with the esd-package using write2nc4.station and contain
## daily data and some summary statistics. The precalculated summary statistics are used for
## plotting the maps. The intention is that this app is flexible and adjusts the output according
## to the netCDf files and teir contents. To make the app as speedy as possible, it only reads the
## actual data presented, such as pre-calculated summary statistics and selected time series or 
## date. The netCDF files allow for direct access, which means that only the required data are 
## extracted. Some numbers may be estimated, such as expected number of events per year: 365*Pr(X) -
## For temperature use the normal distribution, for precipitation use the "rain equation" - together 
## with the pre-computed summary statistics: (mean, sd) or (fw, mu)
## Rasmus Benestad, 2018-06-21

## The esd-package is available from github.com/metno/esd
library(esd)

getstattype <- function(fname) {
  meta <- retrieve.stationsummary(fname)
  doubleuscrs <- unlist(lapply(gregexpr('_',names(meta)),length)) > 1
  names(meta)[doubleuscrs] <- sub('_','-',names(meta)[doubleuscrs])
  names(meta) <- paste(names(meta),'_',sep='')
  stattype <- rownames(table(substr(names(meta),1,regexpr('_',names(meta))-1)[sapply(meta,is.numeric)]))
  stattype <- stattype[!is.element(stattype,c('station.id'))]
  stattype <- sub('-','_',stattype)
  return(stattype)
}

print('---')
verbose <-FALSE

## Get the file names of the data
fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]

## Extract variables
varids <- list.files(path='data',pattern='.nc',full.names = FALSE)
varids <- varids[grep('.nc',varids,fixed=TRUE)]
varids <- substr(varids,1,regexpr('.',varids,fixed=TRUE)-1)

## Extract information about summary statistics from the netCDF-files
stattype <- getstattype(fnames[1])
print(stattype); print(varids)

## Get the names of locations, etc.
Y <- retrieve.stationsummary(fnames[1])

#print('---')
y <- retrieve.station(fnames[1],stid=Y$station.id[Y$location=="Oslo - blind"],verbose=verbose)





