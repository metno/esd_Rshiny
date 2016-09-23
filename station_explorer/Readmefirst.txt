Read me file for station_explorer app

- How to run app from repository

# Run the app locally,
library(shiny)
runApp('https://github.com/metno/esd_Rshiny/tree/master/station_explorer')

# Run the app from server
To deploy changes or new Apps on shiny server, you need to install the esd package using install_github('metno/esd') command, otherwise, it will not work
library(rsconnect) # You need to set user account and token key, see documentation.
deployApp('https://github.com/metno/esd_Rshiny/tree/master/station_explorer')

