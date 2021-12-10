#shinyBS::bsCollapsePanel(title = "Data Source Details",style = 'primary',
box(title = p("Data Source Details (* required field)",actionLink(inputId = 'metadataFields', label = "", icon = icon("question-circle"))),
    status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
                         #div(style="float:right",actionLink(inputId = 'metadataFields', label = "Help", icon = icon("question-circle"))),
                         #br(),
                         column(
                           width = 4,
                           textAreaInput(inputId = 'sourceTitle', label = "Title*",value = "", width = '100%'),
                           textAreaInput(inputId = 'sourceAbstract', label = "Abstract*",value = "", width = '100%')
                         ),
                         column(
                           width = 4,
                           textInput(inputId = 'sourceCreator', label = "Primary Contact Name*", value = user_info()$user_info$fullname, width = '100%'),
                           textInput(inputId = 'sourceCreatorEmail', label = "Primary Contact Email*", value = user_info()$user_info$email, width = '100%'),
                           textInput(inputId = 'sourceCreatorORCID', label = "Primary Contact ORCID", value = "", width = '100%'),
                           textInput(inputId = 'sourceOrganisation', label = "Organisation*", value = user_info()$user_info$affiliation, width = '100%')
                         ),
                         column(
                           width = 4,
                           fluidRow(
                             column(
                               width = 4,
                               checkboxInput(inputId = 'sourceAvailableOnline',label = "Available Online?",value = FALSE)
                             ),
                             column(
                               width = 8,
                               shinyjs::hidden(textInput(inputId = "sourceURI", label = "KNB URL", value = "", width = '100%')),
                               textInput(inputId = "sourceALTURI", label = "Online Location (URL)", value = "", width = '100%')
                             )
                           ),
                           fluidRow(
                             column(
                               width = 4,
                               checkboxInput("sourceMaintenanceToggle",label = "Are updates expected?", value = FALSE)
                             ),
                             column(
                               width = 8,
                               # options below based on EML frequency types https://eml.ecoinformatics.org/schema/
                               selectInput(inputId = 'sourceMaintenance', label = "Update Frequency",
                                           choices = c("notPlanned"),
                                           selected = "notPlanned", width = '100%')
                             )
                           ),
                           fluidRow(
                             column(
                               width = 4,
                               checkboxInput("embargoEndToggle",label = "Is the data embargoed?", value = FALSE)
                             ),
                             column(
                               width = 8,
                               textInput('embargoEnd',"Embargo End Date:",value = "")
                             )
                           )

                         )
)