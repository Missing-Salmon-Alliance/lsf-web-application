############################
#administratorArea_ui.R START
############################
tabItem( # tabItem 
  tabName = 'adminZone',
tabsetPanel(id = "useradmin",
            selected = "User Administration",
            tabPanel(title = "User Administration",
                     box(title = "New User Form",
                         status = 'warning',
                         textInput('useradminFullname',label = "Fullname"),
                         textInput('useradminEmail',label = "Email"),
                         textInput('useradminAffiliation',label = "Affiliation"),
                         passwordInput('useradminPassword',label = "Password"),
                         checkboxInput('useradminAdmin',label = "Administrator?")
                         ),
                     actionButton('addNewUser',"Add New User", icon = icon('user-check'))
                    ),
                   tabPanel(title = "Quality Control",
                            column(
                              width = 1,
                              br(),
                              h4("Load and review"),
                              selectizeInput('QCidSelector','Dropdown',choices = LSFMetadataTibble$id),
                              column(
                                width = 6,
                                actionLink('QCpreviousID',"Prev")
                              ),
                              column(
                                width = 6,
                                actionLink('QCnextID',"Next")
                              ),
                              br(),
                              h6("...OR...",align = 'center'),
                              textInput('QCcustomID','Manual input:'),
                              actionButton('QCcustomIDButton',"Load from ID")
                            ),
                            column(11,
                                   shinyBS::bsCollapse(id = "QCDataSourceFields", open = c("Data Source Details","Geographic and Temporal Coverage"), multiple = TRUE,
                                                       shinyBS::bsCollapsePanel(title = "Data Source Details",
                                                              column(
                                                                width = 4,
                                                                textAreaInput(inputId = "QCTitle", label = "Title",value = "", width = '100%'),
                                                                textInput(inputId = "QCCreator", label = "Primary Contact", value = "", width = '100%'),
                                                                textInput(inputId = "QCOrganisation", label = "Organisation", value = "", width = '100%')
                                                              ),
                                                              column(
                                                                width = 4,
                                                                textAreaInput(inputId = "QCAbstract", label = "Abstract",value = "", width = '100%'),
                                                                textInput(inputId = "QCCreatorEmail", label = "Primary Contact Email", value = "", width = '100%'),
                                                                textInput(inputId = "QCURI", label = "KNB URL", value = "", width = '100%')
                                                              ),
                                                              column(
                                                                width = 4,
                                                                textInput(inputId = 'QCCreatorORCID', label = "Primary Contact ORCID", value = "", width = '100%'),
                                                                column(
                                                                  width = 6,
                                                                  # options below based on EML frequency types https://eml.ecoinformatics.org/schema/
                                                                  selectInput(inputId = 'QCMaintenance', label = "Select Update Frequency",
                                                                              choices = c("annually",
                                                                                          "asNeeded",
                                                                                          "biannually",
                                                                                          "continually",
                                                                                          "daily",
                                                                                          "irregular",
                                                                                          "monthly",
                                                                                          "notPlanned",
                                                                                          "weekly",
                                                                                          "unknown",
                                                                                          "otherMaintenancePeriod"),
                                                                              selected = "unknown", width = '100%')
                                                                ),
                                                                column(
                                                                  width = 6,
                                                                  selectInput(inputId = 'QCAccessLevel',label = "Access Level",
                                                                              choices = c("Members Only","Open"), multiple = FALSE)
                                                                ),
                                                                textInput(inputId = "QCALTURI", label = "Alt URL", value = "", width = '100%')
                                                              )
                                              ),

                                              shinyBS::bsCollapsePanel(title = "Geographic and Temporal Coverage",
                                                              column(
                                                                width = 3,
                                                                textAreaInput(inputId = "QCGeographicDescription", label = "Short Geographic Description",value = ""),
                                                                hr(),
                                                                h4("Longitudinal Boundaries"),
                                                                numericInput(inputId = 'QCWest', label = "Western Boundary",min=-180,max=180,value = -15,step = 0.0001),
                                                                numericInput(inputId = 'QCEast', label = "Eastern Boundary",min=-180,max=180,value = -14,step = 0.0001),
                                                                hr(),
                                                                h4("Latitudinal Boundaries"),
                                                                numericInput(inputId = 'QCNorth', label = "Northern Boundary",min=-90,max=90,value = 61,step = 0.0001),
                                                                numericInput(inputId = 'QCSouth', label = "Southern Boundary",min=-90,max=90,value = 60,step = 0.0001)
                                                              ),
                                                              column(
                                                                width = 6,
                                                                leaflet::leafletOutput('QCMap')
                                                              ),
                                                              column(
                                                                width = 3,
                                                                sliderInput(inputId = "QCStartEndYear", label = "Indicate Range of Years Covered", min = 1900, max = 2021, value = c(2000,2020), sep = ""),
                                                                shinyWidgets::checkboxGroupButtons("QCmonthsOfYear", "Indicate Months Covered", choices = month.abb,
                                                                                     status = "default",
                                                                                     checkIcon = checkboxGroupButtonsIcons),
                                                                actionButton("QCmonthsOfYearToggleAll","Select All")
                                                              )
                                              ),


                                              shinyBS::bsCollapsePanel(title = "Domain and Variable Classes",
                                                              column(
                                                                width = 5,
                                                                shinyWidgets::checkboxGroupButtons('QCdomainNodeList',
                                                                                     'Domain',
                                                                                     choices = LSFDomainTibble$domainTitle,
                                                                                     justified = F,
                                                                                     status = 'default',
                                                                                     checkIcon = checkboxGroupButtonsIcons),
                                                                shinyWidgets::checkboxGroupButtons('QCesvCategory',
                                                                                     'Variable Classes',
                                                                                     choices = c('Biological','Physical','Salmon Trait'),
                                                                                     status = 'default',checkIcon = checkboxGroupButtonsIcons)
                                                              ),
                                                              column(
                                                                width = 7,
                                                                uiOutput('QCesvPerDomain')
                                                              )
                                              )
                                   ) # close bsCollapse
                            ) # close column 2
                            
                   ) # close tabPanel 2 (QC Panel)
) # close tabsetPanel
)

############################
#administratorArea_ui.R START
############################