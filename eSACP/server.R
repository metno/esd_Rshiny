library(shiny)
library(DT)
# Load the ggplot2 package which provides
# the 'mpg' dataset.
#library(esd)
selectrowindex <- 1
# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    library(xlsx)
    x <- xlsx::read.xlsx('WP_1_data_sources_and_links_Version_0.91.xlsx',
                         sheetIndex = 1)
    x$url <- paste0("<a href='",x$url,"'>",x$url,"</a>")
    datatable(x,filter='top',selection = 'single',escape = FALSE
              # options = list(columnDefs = list(list(
              #   targets = 21,
              #   render = JS(
              #     "function(data, type, row, meta) {",
              #     "return type === 'display' && data.length > 6 ?",
              #     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
              #     "}")
              # ))), callback = JS('table.page(3).draw(false);')
              ) 
  })
  output$text <- renderText({
    txt <- "This browse tab contains meta data for selecting variables from eSACP predefined list."
  })
  
  output$text2 <- renderText({
    txt <- 'Filter your search based on column names and click on one station from the table. \n 
    The link to the original file is the follwoing : https://github.com/eSACP/internal/raw/master/WP1_deliverables/WP_1_data_sources_and_links_Version_0.91.xlsx'
  
  })
  
  output$downloadMeta <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
          },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
          }
  )
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
       }
  )
  
})

