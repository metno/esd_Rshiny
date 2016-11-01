library(shiny)
library(esd)

## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.

season <- 1
Z4 <- list()
load('data/dse.aaca.t2m.rcp45.djf.eof.rda')
Z4$djf <- Z
load('data/dse.aaca.t2m.rcp45.mam.eof.rda')
Z4$mam <- Z
load('data/dse.aaca.t2m.rcp45.jja.eof.rda')
Z4$jja <- Z
load('data/dse.aaca.t2m.rcp45.son.eof.rda')
Z4$son <- Z
iloc <- 1

shinyServer(function(input, output) {
  
  #is <- list(lon=input$lon,lat=input$lat)
  #it <- as.Date(input$Dates)
  
  Season <- reactive({ 
    switch(input$season,
           'djf'=1,'mam'=2,'jja'=3,'son'=4)})
  Im <- reactive({input$model})
  Lon <- reactive({input$lon})
  Lat <- reactive({input$lat})
  #is <- list(lon=Lon(),lat=Lat())
  Dates <- reactive({
    seq(min(year(input$dates)),max(year(input$dates)),by=1)
    })
  Baseline <- reactive({
    seq(min(year(input$baseline)),max(year(input$baseline)),by=1)
  })
  Param <- reactive({input$variable})
  Rcp <- reactive({input$scenario})
  Loc <- reactive({input$location})
  
  print(names(Z4))
  gcmnames <<- names(Z4[[season]])[-c(1,2,length(Z4[[season]]))]
  
  #print(names(z))
  
  #output$map <- renderPlot({plot(rnorm(100),rnorm(100),main='test')})
  output$map <- renderPlot({ 
    #esd::map(Z4[[Season()]],it=It(),is=list(lon=Lon(),lat=Lat()),im=Im(),
    #         main=paste("You have selected", Param()),new=FALSE)},
    esd::map(Z4[[1]],it=2050,
             main="You have selected",new=FALSE)},
    height=function(){800})
  output$plot <- renderPlot({
    zz <- Z4[[Season()]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','list')
    z <- as.station(zz)
    iloc <- (1:length(names(z)))[is.element(names(z),Loc())]
    plot::plot(z[[iloc]],main=names(z)[iloc],new=FALSE)
  }, height=function(){800})
  output$plot.ens <- renderPlot({ 
    zz <- Z4[[Season()]]; zz$eof <- NULL;
    class(zz) <- c('dsensemble','pca','list')
    z <- as.station(zz)
    plot(subset(z,is=Loc()),im=Im(),main='Ensemble plot',new=FALSE)},
    height=function(){800})
    
})
