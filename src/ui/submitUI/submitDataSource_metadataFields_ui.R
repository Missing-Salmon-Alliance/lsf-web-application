#shinyBS::bsCollapsePanel(title = "Data Source Details",style = 'primary',
box(title = "Data Source Details",status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
                         div(style="float:right",actionLink(inputId = 'metadataFields', label = "Help", icon = icon("question-circle"))),
                         br(),
                         column(
                           width = 4,
                           textAreaInput(inputId = 'sourceTitle', label = "Title",value = "", width = '100%'),
                           textAreaInput(inputId = 'sourceAbstract', label = "Abstract",value = "", width = '100%')
                         ),
                         column(
                           width = 4,
                           textInput(inputId = 'sourceCreator', label = "Primary Contact Name", width = '100%'),
                           textInput(inputId = 'sourceCreatorEmail', label = "Primary Contact Email", width = '100%'),
                           textInput(inputId = 'sourceCreatorORCID', label = "Primary Contact ORCID", value = "", width = '100%'),
                           textInput(inputId = 'sourceOrganisation', label = "Organisation", width = '100%')
                         ),
                         column(
                           width = 4,
                           fluidRow(
                             column(
                               width = 3,
                               checkboxInput(inputId = 'sourceAvailableOnline',label = "Available Online?",value = FALSE)
                             ),
                             column(
                               width = 9,
                               shinyjs::hidden(textInput(inputId = "sourceURI", label = "KNB URL", value = "", width = '100%')),
                               textInput(inputId = "sourceALTURI", label = "Online Location (URL)", value = "", width = '100%')
                             )
                           ),
                           fluidRow(
                             column(
                               width = 3,
                               checkboxInput("sourceMaintenanceToggle",label = "Will this data be updated in the future?", value = FALSE)
                             ),
                             column(
                               width = 9,
                               # options below based on EML frequency types https://eml.ecoinformatics.org/schema/
                               selectInput(inputId = 'sourceMaintenance', label = "Update Frequency",
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
                                           selected = "notPlanned", width = '100%')
                             )
                           ),
                           fluidRow(
                             column(
                               width = 3,
                               checkboxInput("embargoEndToggle",label = "Embargoed?", value = FALSE)
                             ),
                             column(
                               width = 9,
                               dateInput('embargoEnd',"Embargo End Date:",value = Sys.Date())
                             )
                           )

                         )
)