library(shiny)
library(esd)

## Preparations - grid the station data and reduce the size of the data by keeping only
## the most important PCA modes.
if (!file.exists('dse.eof.t2m.rcp45.rda')) {
  load("dse.aaca.t2m.rcp45.djf.rda")
  x <- subset(dse.aaca.t2m.rcp45,pattern=1:3)
  demo(pca2eof,ask=FALSE)
  Z <- as.eof.dsensemble.pca(dse.aaca.t2m.rcp45,verbose=TRUE)
  #class(Z) <- c("dsensemble", "eof", "list")
  save(file='dse.eof.t2m.rcp45.rda',Z)
} else load('dse.eof.t2m.rcp45.rda')

shinyServer(function(input, output) {
  
   output$map <- renderPlot({ 
    plot(Y[[1]],main=paste("You have selected", input$Element),new=FALSE)
    })
    
})
