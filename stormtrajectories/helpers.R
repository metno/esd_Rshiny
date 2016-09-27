
map.fancy <- function(Y,param=NULL,xlim=c(-80,40),ylim=c(30,90)) {
  data(etopo5)
  breaks <- c(-10000,seq(0,1000,200),2000,4000,8000,10000)
  n <- length(breaks)
  cb.map <- list(col=rgb(rep(0,n-1),rep(0,n-1),c(0.3,rep(0.4,n-2)),
                         c(0.95,seq(0.8,0.6,length.out=n-2))),
             show=FALSE,breaks=breaks)
  cb.cyclones <- select.cb(param=param)
  alpha <- select.alpha(dim(Y)[1])
  par(mar=c(5.1, 4.1, 2.5, 2.5), mgp=c(2,0.5,0))
  par(bty="n", fig=c(0,1,0.1,1))
  map(etopo5,
      colbar=cb.map,geography=FALSE,gridlines=FALSE,
      type="fill",xlim=xlim,ylim=ylim,
      xaxt="n",yaxt="n")
  if(dim(Y)[1]>0) {
    map(Y,add=TRUE,new=FALSE,type="colors",lwd=4,cex=1.5,
      xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",alpha=alpha,
      param=param,colbar=cb.cyclones,show.subset=FALSE,
      col="burlywood1")
    mtext(select.unit(param),side=1,outer=FALSE,padj=-4,adj=0.5,cex=1.4)
  }
}
    
switch.param <- function(param) {
  if(grepl("bjornholt",tolower(param))) {
    data(bjornholt)
    param <- bjornholt
    d <- as.Date(paste(strftime(index(param),"%Y-%m"),"-01",sep=""))
    param <- aggregate(bjornholt,by=d,FUN="wetfreq")
  } else {
    param <- switch(param,"Pressure"="pcent","Month"="month","Year"="year",
                    "NAO"="nao","AMO"="amo","Global temperature"="t2m",
                    "Maximum wind speed"="max.speed","Radius"="radius","Uniform"=NULL)
  }
  return(param)
}

select.unit <- function(param) {
  unit <- " "
  if(is.character(param)) {
    if(param=="pcent") { 
      unit <- "(hPa)"
    } else if(param=="max.speed") {
      unit <- "(m/s)"
    } else if(param=="radius") { 
      unit <- "(km)"
    } else if(param=="month") {
      unit <- " "
    } 
  } else {
    unit <- attr(param,"unit")
  }
  return(unit)
}

select.cb <- function(param) {
  if(is.character(param)) {
    if(param=="nao") {
      breaks=seq(-2,2,0.5)
      col <- colscal(n=length(breaks)+4,col="t2m",rev=FALSE)
      col <- col[3:(length(col)-3)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else if(param=="amo") {
      breaks <- seq(-0.4,0.4,0.1)
      col <- colscal(n=length(breaks)+4,col="t2m",rev=FALSE)
      col <- col[3:(length(col)-3)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else if(param=="pcent") { 
      breaks <- seq(950,1000,5)
      col <- colscal(n=length(breaks)+4,col="t2m",rev=TRUE)#2,col="rd",rev=TRUE)
      col <- col[3:(length(col)-3)]#2:(length(col)-2)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else if(param=="max.speed") { 
      breaks <- seq(30,80,5)
      col <- colscal(n=length(breaks)+4,col="t2m",rev=FALSE)
      col <- col[3:(length(col)-3)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else if(param=="radius") { 
      breaks <- seq(200,1000,50)
      col <- colscal(n=length(breaks)+4,col="t2m",rev=FALSE)
      col <- col[3:(length(col)-3)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else if(param=="month") {
      breaks <- seq(1,13,1)
      col <- colscal(n=length(breaks)+2,col="t2m",rev=TRUE)
      col <- col[2:(length(col)-2)]
      colbar <- list(col=col,n=length(breaks),breaks=breaks)
    } else {
      col <- colscal(n=14,col="t2m",rev=FALSE)
      col <- col[4:(length(col)-3)]
      colbar <- list(col=col)
    } 
  } else {
    col <- colscal(n=14,col="t2m",rev=FALSE)
    col <- col[4:(length(col)-3)]
    colbar <- list(col=col)
  }
  return(colbar)
}

select.alpha <- function(n) {
  alpha <- 0.05 + min(50/n,0.4)
  return(alpha)
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
  
select.param <- function(x,param="pcent",pmin=0,pmax=1000,verbose=FALSE) {
  if(verbose) print("select.param")
  ip <- colnames(x)==param
  i <- apply(x,1,function(x) do.call(any,list(x[ip]>=pmin & x[ip]<=pmax)))
  return(i)
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
