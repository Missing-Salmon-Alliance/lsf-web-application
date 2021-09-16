#shinyBS::bsCollapsePanel(title = "Geographic Coverage and Temporal Coverage",style = 'primary',
box(title = p("Geographic Coverage and Temporal Coverage",actionLink(inputId = "geogTimeFields", label = "", icon = icon("question-circle"))),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
    
    #div(style="float:right",),
    #br(),
                         column(
                           width = 3,
                           textAreaInput(inputId = "sourceGeographicDescription", label = "Short Geographic Description",value = ""),
                           hr(),
                           tags$b("Define a Rectangular Bounding Box"),
                           p("Enter the min/max extents the data cover (decimal degrees). Click map to autofill, adjust inputs as required."),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitWest', label = "West",min=-180,max=180,value = -15,step = 0.0001)
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitEast', label = "East",min=-180,max=180,value = -14,step = 0.0001)
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitNorth', label = "North",min=-90,max=90,value = 61,step = 0.0001)
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitSouth', label = "South",min=-90,max=90,value = 60,step = 0.0001)
                           )
                         ),
                         column(
                           width = 6,
                           leaflet::leafletOutput('submitMap')
                         ),
                         column(
                           width = 3,
                           tags$b("Indicate Range of Years Covered"),
                           numericInput(inputId = 'sourceStartYear',label = "Start",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
                           numericInput(inputId = 'sourceEndYear',label = "End",min = 0,max = 9999,value = 2021,step = 1,width = '45%'),
                           shinyWidgets::checkboxGroupButtons("monthsOfYear", "Indicate Months Covered", choices = month.abb,
                                                              status = "default",
                                                              checkIcon = checkboxGroupButtonsIcons),
                           actionButton("monthsOfYearToggleAll","Select All")
                         )
                         
)