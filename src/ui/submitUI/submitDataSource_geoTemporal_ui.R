#shinyBS::bsCollapsePanel(title = "Geographic Coverage and Temporal Coverage",style = 'primary',
box(title = "Geographic Coverage and Temporal Coverage",status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
                         column(
                           width = 3,
                           textAreaInput(inputId = "sourceGeographicDescription", label = "Short Geographic Description",value = ""),
                           hr(),
                           h4("Longitudinal Boundaries"),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitWest', label = "",min=-180,max=180,value = -15,step = 0.1)
                           ),
                           column(
                             width = 6,
                             numericInput(inputId = 'submitEast', label = "",min=-180,max=180,value = -14,step = 0.1)
                           ),
                           
                           hr(),
                           h4("Latitudinal Boundaries"),
                           numericInput(inputId = 'submitNorth', label = "Northern Boundary",min=-90,max=90,value = 61,step = 0.1, width = '40%'),
                           numericInput(inputId = 'submitSouth', label = "Southern Boundary",min=-90,max=90,value = 60,step = 0.1, width = '40%')
                         ),
                         column(
                           width = 6,
                           leaflet::leafletOutput('submitMap')
                         ),
                         column(
                           width = 3,
                           div(style="float:right",actionLink(inputId = "geogTimeFields", label = "Help", icon = icon("question-circle"))),
                           br(),
                           sliderInput(inputId = "sourceStartEndYear", label = "Indicate Range of Years Covered", min = 1900, max = 2021, value = c(2000,2020), sep = ""),
                           shinyWidgets::checkboxGroupButtons("monthsOfYear", "Indicate Months Covered", choices = month.abb,
                                                              status = "default",
                                                              checkIcon = checkboxGroupButtonsIcons),
                           actionButton("monthsOfYearToggleAll","Select All")
                         )
                         
)