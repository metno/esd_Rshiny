library(shiny)
library(esd)
if (!file.exists('dse.eof.t2m.rcp45.rda')) {
  load("dse.aaca.t2m.rcp45.djf.rda")
  Z <- as.eof.dsensemble.pca(dse.aaca.t2m.rcp45,verbose=TRUE)
  save(file='dse.eof.t2m.rcp45.rda',Z)
} else load('dse.eof.t2m.rcp45.rda')
shinyServer(function(input, output) {
  
   output$map <- renderPlot({ 
    plot(Y[[1]],main=paste("You have selected", input$Element),new=FALSE)
    })
    
})
