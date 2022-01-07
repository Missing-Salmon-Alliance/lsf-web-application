#shinyBS::bsCollapsePanel(title = "Geographic Coverage and Temporal Coverage",style = 'primary',
box(title = p("Geographic Coverage and Temporal Coverage",actionLink(inputId = "geogTimeFields", label = "", icon = icon("question-circle"))),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
    
    #div(style="float:right",),
    #br()
   column(
     width = 4,
     textAreaInput(inputId = "sourceGeographicDescription", label = "Short Geographic Description",value = "Commonly accepted SALMO SALAR range."),
     tags$b("Define a Rectangular Bounding Box"),
     p("Enter the min/max extents the data cover (decimal degrees EAST and NORTH). Click map to autofill, adjust inputs as required.
       Negative values should be used for WEST of meridian and SOUTH of equator."),
     column(
       width = 3,
       numericInput(inputId = 'submitNorth', label = "North",min=-90,max=90,value = round(salmosalarExtents[['ymax']],digits = 4),step = 0.0001),
       numericInput(inputId = 'submitSouth', label = "South",min=-90,max=90,value = round(salmosalarExtents[['ymin']],digits = 4),step = 0.0001)
     ),
     column(
       width = 3,
       numericInput(inputId = 'submitEast', label = "East",min=-180,max=180,value = round(salmosalarExtents[['xmax']],digits = 4),step = 0.0001),
       numericInput(inputId = 'submitWest', label = "West",min=-180,max=180,value = round(salmosalarExtents[['xmin']],digits = 4),step = 0.0001)
     ),
     column(
       width = 6,
       tags$b("Pre-defined Bounding Boxes:"),
       br(),
       actionButton('salmonRangeExtents',label = "SALMO SALAR Range",class = "btn-default btn-sm"),
       br(),
       actionButton('predefinedRectangleNorthernHemi',label = "Northern Hemisphere",class = "btn-default btn-sm"),
       br(),
       actionButton('predefinedRectangleGlobal',label = "Global",class = "btn-default btn-sm")
     )
   ),
   column(
     width = 6,
     leaflet::leafletOutput('submitMap')
   ),
   column(
     width = 2,
     tags$b("Indicate Range of Years Covered"),
     numericInput(inputId = 'sourceStartYear',label = "Start",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
     numericInput(inputId = 'sourceEndYear',label = "End",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
     shinyWidgets::checkboxGroupButtons("monthsOfYear", "Indicate Months Covered", choices = month.abb,
                                        justified = F,
                                        status = 'default',
                                        size = 'sm',
                                        width = '100%',
                                        checkIcon = checkboxGroupButtonsIcons),
     actionButton("monthsOfYearToggleAll","Select All")
   )
                         
)
