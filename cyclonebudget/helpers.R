
prep.imilast(path="~/git/esd_Rshiny/cyclonebudget/data") {
  files <- list.files(path=path,pattern="ERAinterim_1.5_NH_M")
  for(f in files) {
    print(f)
    x <- read.imilast(path=path,f)
    m <- substr(f,regexpr("_M",f)+1,regexpr("_M",f)+3)
    fname.m <- paste("trajectories.",m,".rda",sep="")
    if(!file.exists(file.path(path,fname.m))) {
      print(fname.m)
      eval(parse(text=paste(m," <- as.trajectory(x,verbose=TRUE)",sep="")))
      eval(parse(text=paste("save(file='",file.path(path,fname.m),"',",m,")",sep="")))
    }
  }
}

select.cyclones <- function(x,param=NULL,pmin=NULL,pmax=NULL,date=NULL,dates=NULL,years=NULL,
                            xlim=NULL,ylim=NULL,it=NULL,verbose=FALSE) {
  if(verbose) print("select.cyclones")
  i <- rep(TRUE,dim(x)[1])
  if(!is.null(param)) {
    for (j in seq(length(param))) i <- i & select.param(x,param[j],pmin[j],pmax[j])
  }
  if(!is.null(xlim) | !is.null(ylim)) i <- i & select.lonlat(x,xlim=xlim,ylim=ylim)
  if(!is.null(it)) i <- i & select.season(x,it=it)
  if(!is.null(date)) i <- i & select.date(x,it=date)
  if(!is.null(dates)) i <- i & select.date(x,it=dates)
  if(!is.null(years)) i <- i & select.years(x,it=years)
  y <- x[i,]
  class(y) <- class(x)
  y <- attrcp(x,y)
  return(y)
}

select.lonlat <- function(x,xlim=c(-90,90),ylim=c(20,90),verbose=FALSE) {
  if(verbose) print("select.lonlat")
  ilon <- colnames(x)=="lon"
  ilat <- colnames(x)=="lat"
  if(is.null(xlim) & is.null(ylim)) {
    i <- rep(TRUE,length(ilon))
  } else {
    if(is.null(xlim)) xlim <- c(-180,180)
    if(is.null(ylim)) xlim <- c(0,90)
    i <- apply(x,1,function(x) do.call(any,list(x[ilon]>=xlim[1] & x[ilon]<=xlim[2] & 
                                                  x[ilat]>=ylim[1] & x[ilat]<=ylim[2] )))
  }
  return(i)
}

select.years <- function(x,it=2015,verbose=FALSE) {
  if(verbose) print("select.years")
  d <- x[,colnames(x) %in% c("start","end")]
  y <- round(d*1E-6)
  i <- y[,1] %in% unique(it) | y[,2] %in% unique(it)
  return(i)
}

select.season <- function(x,it="djf",verbose=FALSE) {
  if(verbose) print("select.season")
  d <- x[,colnames(x) %in% c("start","end")]
  m <- round((d - round(d*1E-6)*1E6)*1E-4)
  fn <- function(it) switch(it,"all"=1:12,"djf"=c(12,1,2),"mam"=c(3,4,5),
                            "jja"=c(6,7,8),"son"=c(9,10,11),
                            "winter"=c(12,1,2),"spring"=c(3,4,5),
                            "summer"=c(6,7,8),"fall"=c(9,10,11),"autumn"=c(9,10,11))
  it <- as.vector(sapply(it,fn))
  i <- m[,1] %in% unique(it) | m[,2] %in% unique(it)
  return(i)
}

select.date <- function(x,it=c("1984-01-01","1984-02-01"),verbose=FALSE) {
  if(verbose) print("select.date")
  if(inherits(it,c("Date","character"))) it <- as.numeric(strftime(it,format="%Y%m%d"))
  if(length(it)==1) it <- c(it,it)
  fn <- function(y) {
    y <- strptime(round(y*1E-2),format="%Y%m%d")
    y <- seq(y[1],y[2],by="day")
    y <- as.numeric(strftime(y,format="%Y%m%d"))
    return(any(y>=it[1] & y<=it[2]))
  }
  i <- apply(x[,colnames(x) %in% c("start","end")],1,fn)
  return(i)
}
