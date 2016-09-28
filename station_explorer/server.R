
library(shiny)
if (!require("DT")) install.packages('DT')
library(DT)
library(esd)

selectrowindex <- 1

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    datatable(select.station()[c(2,3,13,10,8,9,4,5,6)],
              colnames = c('Location','Country','Variable','Source','Start','End','Longitude','Latitude','Altitude'),
              filter='top',selection = 'single',options = list()) %>% 
      formatRound(c('longitude','latitude'), digits=2) 
  })
  output$text <- renderText({
    txt <- "This browse tab contains meta data for a list of commonly used weather variables from various freely available global and/or national weather data sets. 
           The various sources that have been included are : MET Norway archive (eklima.met.no) daily (METNOD) and monthly (METNOM) dataset, 
    the Global historical climatology network on a daily (GHCND) and monthly (GHCNM) datasets, the European climate data sets (ECAD), the Nordic monthly data sets (NACD) and data
    from the Nordic Arctic Research Program (NARP)."
  })
  
  output$text1 <- renderText({
    txt <- 'Description of the weather variables'
  })

  output$text2 <- renderText({
    txt <- 'Filter your search based on column names and click on one station from the table'
  })
  
  output$varnames <- DT::renderDataTable({
    datatable(rbind(c('T2m','Mean temperature'),
                    c('Tmin','Minimum temperature'),
                    c('Tmax','Maximum temperature'),
                    c('Precip','Total Precipitation'),
                    c('SS','Sunshine'),
                    c('Tl','Lowest minimum temperature'),
                    c('Dtl','Day of Tl'),
                    c('Rh', 'Relative humidity'),
                    c('Slp','Mean sea level pressure'),
                    c('Dsc','Number of days with snow cover 50 covered'),
                    c('Fx','Wind speed'),
                    c('Fd','Wind direction'),
                    c('Fg','Wind Gust'),
                    c('Pmax','Maximum 1 day precipitation'),
                    c('CC','Cloud cover'),
                    c('SD','Snow depth'),
                    c('SF','Snowfall')),
              colnames = c('Abbreviation','Long name'),options = list(pageLength = 3))
  })
  
  output$plot <- renderPlot({
    library(esd)
    #browser()
    if (length(input$table_rows_selected)>0) {
      selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
      selectedrow <- (select.station()[selectedrowindex,]) # datasetInput() 
      #selectedrow
    } else
      selectedrowindex <- 1
    plot(station(select.station()[selectedrowindex,]),new=FALSE)  # datasetInput()
    #data(Oslo)
    #plot(Oslo)
  })
  
  output$selectedrow <- DT::renderDataTable({
    if (length(input$table_rows_selected)>0) {
      selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
    } else
      selectedrowindex
    selectedrow <- (select.station()[selectedrowindex,])
    selectedrow
    ##plot(station(select.station()[selectedrowindex,]))  
  })
  
  output$selsta <- DT::renderDataTable({
    #browser()
    if (length(input$table_rows_selected)>0) {
      selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
    } else 
      selectedrowindex <- 1
    selectedrow <- (select.station()[selectedrowindex,])
    selectedrow
    sel.sta <- select.station()[selectedrowindex,]  
    DT::datatable(sel.sta,options = list(dom = 't'))
  })
  
  output$rawdata <- DT::renderDataTable({
    #browser()
    if (length(input$table_rows_selected)>0) {
      selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
    } else 
      selectedrowindex <- 1
    selectedrow <- (select.station()[selectedrowindex,])
    selectedrow
    sel.sta <- station(select.station()[selectedrowindex,])  
    # browser()
    DT::datatable(data.frame(Date=index(sel.sta),Value=coredata(sel.sta)),filter='top',options = list(paging = TRUE))
  })
  
  output$map <- renderPlot({
    library(esd)
    if (length(input$table_rows_selected)>0) {
      selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
    } else 
      selectedrowindex <- 1
    selectedrow <- (select.station()[selectedrowindex,])
    selectedrow
    st <- station(select.station()[selectedrowindex,])
    map(st,showall=TRUE,xlim=c(-180,180),ylim=c(-90,90),new=FALSE,cex=3)  # datasetInput() # select.station(),subset=
    points(lon(st),lat(st),cex=1.5,col='red',bg='orange')
  })
  
  # output$plots <- renderPlot({
  #   
  #   variable <- table[selectedrowindex,1]
  #   #write your plot function
  # })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  
  #output$plot <- renderPlot({
  # library(esd)
  # data(Oslo)
  # # y <- station(datasetInput())
  # plot(Oslo,new=FALSE)
  #})
  
  output$downloadMeta <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      library(esd)
      if (length(input$table_rows_selected)>0) {
        selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
        selectedrowindex <- as.numeric(selectedrowindex)
      } else
        selectedrowindex <- 1
      #browser()
      param <- ele2param(ele = select.station()[selectedrowindex,7],src=select.station()[selectedrowindex,10])
      loc <- select.station()[selectedrowindex,2]
      src <-  select.station()[selectedrowindex,10]
      cntr <- select.station()[selectedrowindex,3]
      paste(gsub(' ','-',param[2]),'_',loc,'-',cntr,'_',src,'meta.csv', sep = "")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      library(esd)
      if (length(input$table_rows_selected)>0) {
        selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
        selectedrowindex <- as.numeric(selectedrowindex)
      } else
        selectedrowindex <- 1
      # Write to a file specified by the 'file' argument
      write.csv(as.data.frame(select.station()[selectedrowindex,]), file, row.names = FALSE)
    }
  )
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      library(esd)
      if (length(input$table_rows_selected)>0) {
        selectedrowindex <- input$table_rows_selected[length(input$table_rows_selected)]
        selectedrowindex <- as.numeric(selectedrowindex)
      } else
        selectedrowindex <- 1
      param <- ele2param(ele = select.station()[selectedrowindex,7],src=select.station()[selectedrowindex,10])
      loc <- select.station()[selectedrowindex,2]
      src <-  select.station()[selectedrowindex,10]
      cntr <- select.station()[selectedrowindex,3]
      paste(gsub(' ','-',param[2]),'_',loc,'-',cntr,'_',src,'data.csv', sep = "")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      data <- esd::station(select.station()[1,])
      #save(data,file=file)
      write.table(data.frame(Date=index(data),Value=coredata(data)), file=file, row.names = FALSE)
    }
  )
  
})

