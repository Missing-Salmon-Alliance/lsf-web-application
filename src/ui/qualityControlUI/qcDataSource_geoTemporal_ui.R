box(title = p("Geographic Coverage and Temporal Coverage"),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
    footer = p(actionButton('submitQCGeoTemporal',"Commit Changes",class = 'btn-warning'),actionButton('resetQCGeoTemporal',"Reset Changes",class = 'btn-success')),
                         column(
                           width = 3,
                           textAreaInput(inputId = "qcGeographicDescription", label = "Short Geographic Description",value = ""),
                           hr(),
                           h4("Longitudinal Boundaries"),
                           column(
                             width = 6,
                             numericInput(inputId = 'qcWest', label = "West",min=-180,max=180,value = -15,step = 0.0001),
                             textOutput('originalWestValue')
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'qcEast', label = "East",min=-180,max=180,value = -14,step = 0.0001),
                             textOutput('originalEastValue')
                           ),
                           
                           hr(),
                           h4("Latitudinal Boundaries"),
                           column(
                             width = 6,
                             numericInput(inputId = 'qcNorth', label = "North",min=-90,max=90,value = 61,step = 0.0001),
                             textOutput('originalNorthValue')
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'qcSouth', label = "South",min=-90,max=90,value = 60,step = 0.0001),
                             textOutput('originalSouthValue')
                           )
                         ),
                         column(
                           width = 6,
                           leaflet::leafletOutput('qcMap')
                         ),
                         column(
                           width = 3,
                           tags$b("Indicate Range of Years Covered"),
                           numericInput(inputId = 'qcStartYear',label = "Start",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
                           numericInput(inputId = 'qcEndYear',label = "End",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
                           shinyWidgets::checkboxGroupButtons("qcMonthsOfYear", "Indicate Months Covered", choices = month.abb,
                                                              status = "default",
                                                              checkIcon = checkboxGroupButtonsIcons)
                         )
                         
)