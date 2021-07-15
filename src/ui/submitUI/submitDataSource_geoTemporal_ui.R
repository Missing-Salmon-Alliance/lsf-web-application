#shinyBS::bsCollapsePanel(title = "Geographic Coverage and Temporal Coverage",style = 'primary',
box(title = p("Geographic Coverage and Temporal Coverage",actionLink(inputId = "geogTimeFields", label = "", icon = icon("question-circle"))),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
    
    #div(style="float:right",),
    #br(),
                         column(
                           width = 3,
                           textAreaInput(inputId = "sourceGeographicDescription", label = "Short Geographic Description",value = ""),
                           hr(),
                           h4("Longitudinal Boundaries"),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitWest', label = "West",min=-180,max=180,value = -15,step = 0.0001)
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitEast', label = "East",min=-180,max=180,value = -14,step = 0.0001)
                           ),
                           
                           hr(),
                           h4("Latitudinal Boundaries"),
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
                           sliderInput(inputId = "sourceStartEndYear", label = "Indicate Range of Years Covered", min = 1900, max = 2021, value = c(2000,2020), sep = ""),
                           shinyWidgets::checkboxGroupButtons("monthsOfYear", "Indicate Months Covered", choices = month.abb,
                                                              status = "default",
                                                              checkIcon = checkboxGroupButtonsIcons),
                           actionButton("monthsOfYearToggleAll","Select All")
                         )
                         
)