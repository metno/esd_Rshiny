# Choices for drop-downs
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution

library(shinydashboard)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Projected changes for the Nordic countries ",titleWidth = 600,
                              dropdownMenuOutput("messageMenu")),
              
              dashboardSidebar(
                tags$h4("Settings Panel "),
                selectInput("param7",
                            label = "Element",
                            choices = c("Temperature", # "Precip. sum",
                                        "Wet-day freq.","Precip. intensity"),
                            selected = "Temperature",width = '100%'),
                selectInput("rcp7",label = "Scenario",
                            choices = c("RCP4.5","RCP2.6", "RCP8.5"),
                            selected = "RCP2.6",width = '100%'),
                selectInput(inputId = 'im',label = "Model",choices = c('Ens. Mean','------',gcmnames.26),
                            selected = 'Ens. Mean',width = '100%'),
                selectInput("season7",label = "Season",
                            choices = c("Annual (All seasons)","Winter (DJF)","Spring (MAM)", "Summer (JJA)", "Autumn (SON)"),
                            selected = "Annual (All seasons)",width = '100%'),
                sliderInput("lon7", 
                            label = "Longitudes",
                            min = 0, max = 30, value = c(0, 30)),
                sliderInput("lat7", 
                            label = "Latitudes",
                            min = 55, max = 72, value = c(55, 72)),
                sliderInput("dates7", "Years",min=1900, max=2099, 
                            step = 5, value= c(2070,2099),
                            sep="",width = "100%"),
                sliderInput("datesref", "Base period",min=1900, max=2099, 
                            step = 5, value= c(1980,2010),
                            sep="",width = "100%"),
                selectInput("legend",label = "Legend",
                            choices = c("Display","Hide"),
                            selected = "Display",width = '100%'),
                selectInput("minimap",label = "Mini Map",
                            choices = c("Display","Hide"),
                            selected = "Display",width = '100%')
                # textInput("threshold8",label = "Threshold", placeholder = "1",width = '100%')
                
                
              ),
              dashboardBody(
                fluidPage(
                  valueBoxOutput("changebox"),
                  valueBoxOutput("rcpbox"),
                  valueBoxOutput("ngcmbox"),
                  fluidRow(title = "Disclaimer & info",
                           column(12, 
                                  box(title = "Disclaimer", status = 'info',collapsible = TRUE,collapsed = FALSE,width = '100%',
                                      tags$body("These results are the best estimated based on current knowledge, but do not account for any unknown,
                                                or unexpected events or phenomena that may cause an additional impact on the future temperature and precipitation statistics in the Nordic
                                                region. They are also limited by the degree which the global climate models capture the large-scale conditions and the ability of the downscaling to capture the dependency between large and local scales.,
                                                The results presented here are derived using a similar approach as described in",tags$a(href='http://goo.gl/u3IK4y', 'Benestad et al. (2016)'),"."))
                           )
                  )
                ),
                
                fluidPage(theme = "bootstrap.css",
                          fluidRow(
                            column(12, 
                                   box(title = 'Explore Map', width = '100%' , status = 'primary', # textOutput("title1")
                                       footer = tags$h5(textOutput("subtitle1")),solidHeader = TRUE,
                                       collapsible = TRUE,collapsed = FALSE,
                                       leafletOutput("map0sm",width = '100%', height = 450)))#,
                            # p() ,
                            # column(12, 
                            #        box(title = "Explore Chart (Not yet finished)",status = 'primary', width = '100%',
                            #     footer = tags$h4(textOutput("subtitle2")), solidHeader = TRUE,
                            #     collapsible = TRUE,collapsed = TRUE,  plotOutput("plot", width = '100%', height = 300))) #,
                            # box(title = "Spring Changes",status = 'primary',
                            #     footer = tags$h4('Changes with regards to the reference period.'),solidHeader = TRUE,
                            #     collapsible = TRUE,collapsed = TRUE, leafletOutput("map2sm",width = '100%', height = 450)),
                            # box(title = "Autumn Changes",status = 'primary',
                            #     footer = tags$h4('Changes with regards to the reference period.'),solidHeader = TRUE,
                            #     collapsible = TRUE,collapsed = TRUE, leafletOutput("map3sm",width = '100%', height = 450)),
                            # box(title = "Summer Changes", status = 'primary',
                            #     footer = tags$h4('Changes with regards to the reference period.'),solidHeader = TRUE,
                            #     collapsible = TRUE,collapsed = TRUE, leafletOutput("map4sm",width = '100%', height = 450))
                          )
                          
                ),
                fluidPage(
                  
                  fluidRow(title = "Disclaimer & info",
                           column(12, 
                                  box(title = "More info", status = 'info',collapsible = TRUE,collapsed = FALSE,width = '100%',
                                      tags$body(h4("Compressed data"),
                                                "This app provides a demonstration of compressing information: the data presented here would require more
                                                than 900Mb if it were stored uncompressed as 16-bit numbers, whereas the actual space needed is 97Mb for compressed
                                                R-binary (rda) files. The reduction in data volume is achieved by using EOFs to discard noise and emphasise 
                                                the signal. The use of EOFs also greatly speed up the data processing.",
                                                h4("Downscaling method"),
                                                "For an introduction about downscaling, please see the article on Downscaling Climate Information in",
                                                a("Oxford Research Encyclopedia.",href="http://goo.gl/J5Q2Py"),
                                                h4("Source code"),
                                                a("GitHub",href="https://github.com/metno/esd_Rshiny/tree/master/dse4KSS"), 
                                                "which is based on the R-package 'esd' also available on ",a("GitHub.",href="https://github.com/metno/esd"),
                                                h4("Data"),
                                                "The data (DOI: 10.6084/m9.figshare.2056701.v2) is available from ",
                                                a("GitHub.",href="https://github.com/metno/esd_Rshiny/tree/master/dse4KSS/data"),
                                                h4("Documentation"),
                                                "Further description of this kind of tool is provided in ",
                                                a("Benestad et al. (2017)",href="http://www.sciencedirect.com/science/article/pii/S2405880717300043")))
                           )
                  )
                  
                )
                
                
              )
)