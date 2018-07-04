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
## Rasmus Benestad & Abdelkader Mezghani, 2018-06-21

## The esd-package is available from github.com/metno/esd
library(esd)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)


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

vari2name <- function(x,vars=c('pre','t2m','tmax','tmin'),
                      names=c('Precipitation','Daily mean temperature',
                                'Daily max temperature','Daily min temperature'),nc=3) {
  y <- x
  if (length(vars) != length(names)) stop("vars have different length to names in 'variname'")
  for (i in 1:length(x)) {
    ii <- grep(tolower(substr(x[i],1,nc)),substr(vars,1,nc))
    if (length(ii)>0) y[i] <- names[ii]
  }
  return(y)
}

print('---')
verbose <-FALSE
maintitle <- c('Meteorologisk institutt klimadata','','MET Norway / Climate record explorer')
## Setting for menues etc. 
ci <- c(1:length(varids)); names(ci) <- varids

sea <- c('All year'='all','Dec-Feb'='DJF',
         'Mar-May'='MAM','Jun-Aug'='JJA','Sep-Nov'='SON')
seaTS <- c('All year'='all','Dec-Feb'='DJF',
           'Mar-May'='MAM','Jun-Aug'='JJA','Sep-Nov'='SON',month.abb)
thresholds <- seq(10,50,by=10)
timespace <- c('timeseries','map statistics')

languages <- 1:3; language.names <- c('Bokmål','Nynorsk','English')
maintitle <- c('Meteorologisk institutt klimadata','','MET Norway Climate records')
maptitle <- c('Velg sted','Vel stad','Location selection')
tstitle <- c('Tidsutvikling (historisk vær)','Tidsutvikling (historisk vêr)','Time series (past weather)')
htitle <- c('Statistikk (tidligere klima)','Statistikk (tidligere klima)','Statistical distribution (past climate)')
cftitle <- c('Om portalen & Tilbakemeldinger','Om portalen & Tilbakemeldingar','About & feedback')
lab.timescale <- c("Tidsskala","Tidsskala","Time scale")
lab.timespace <- c("Tid/rom","Tid/rom","Time or Space")
lab.season <- c("Årstid","Årstid","Season")
lab.highlight <- c("Uthev","Uthev","Higlight")
lab.aspect <- c("Perspektiv","Perspektiv","Aspect")
lab.timeperiod <- c("Tidsperiode","Tidsperiode","Time period")
lab.theshold <- c("Terskelverdi","Treskelverdi","Threshold")
lab.location <- c("Sted","Stad","Location")
lab.statitics <- c("Statistikk vist på kartet","Statistikk vist på kartet","Statistics shown in map")
lab.date <- c("Utvalgt dato","Utvald dato","A specific day")
lab.highlight <- c('Uthev','Uthev','Highlight')
aspectsP <- c("sum","wetfreq","wetmean","number of days")
aspectnameP <- rbind(c("Nedbørsmengde","Nedbørsfrekvens","Nedbørsintensitet","Antall dager med mye nedbør"),
                     c("Nedbørsmengde","Nedbørsfrekvens","Nedbørsintensitet","Antall dager med mye nedbør"),
                     c("Total amount","Rain frequency","Mean rain intensity","Days with heavy rain"))
aspectsT <- c("mean","anomaly","number of days")
aspectnameT <- rbind(c("Gjennomsnitt","Avvik fra normalen","Antall dager"),
                     c("Gjennomsnitt","Avvik fra normalen","Antall dager"),
                     c("Mean","Anomaly","Number of days"))
varnames=rbind(c('Nedbør','Middeltemperatur','Maksimumstemperatur','Minimumstemperatur'),
               c('Nedbør','Middeltemperatur','Maksimumstemperatur','Minimumstemperatur'),
               c('Precipitation','Daily mean temperature','Daily max temperature','Daily min temperature'))
timescales <- rbind(c('Dag','Måned','Sesong','År'),
                    c('Dag','Måned','Sesong','År'),
                    c('Day','Month','Season','Year'))
aspects <- aspectsP
tscales <- c("day","month","season","year"); names(tscales) <- tscales
stats <- c(stattype,'Number_of_days','Specific_day')
higlighting <- c('None','Top 10','Low 10')
lingo <- 1
first.location <- 'Oslo - blind'

## Get the file names of the data
fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]

## Extract variables
varids <- list.files(path='data',pattern='.nc',full.names = FALSE)
varids <- varids[grep('.nc',varids,fixed=TRUE)]
varids <- substr(varids,1,regexpr('.',varids,fixed=TRUE)-1)
names(varids) <- vari2name(varids)
names(languages) <- language.names

## Extract information about summary statistics from the netCDF-files
stattype <- getstattype(fnames[1])
print(stattype); print(varids)

## Get the names of locations, etc.
Y <- retrieve.stationsummary(fnames[1])

#print('---')
y <- retrieve.station(fnames[1],stid=Y$station.id[Y$location=="Oslo - blind"],verbose=verbose)





