box(title = p("Data Source Details"),
     status = 'primary',solidHeader = TRUE, collapsible = TRUE, width = 12,
     footer = p(actionButton('submitQCMetadata',"Commit Changes",class = 'btn-warning'),actionButton('resetQCMetadata',"Reset Changes",class = 'btn-success')),
     column(
       width = 4,
       textAreaInput(inputId = 'qcTitle', label = "Title*",value = "", width = '100%'),
       textAreaInput(inputId = 'qcAbstract', label = "Abstract*",value = "", width = '100%')
     ),
     column(
       width = 4,
       textInput(inputId = 'qcCreator', label = "Primary Contact Name*", width = '100%'),
       textInput(inputId = 'qcCreatorEmail', label = "Primary Contact Email*", width = '100%'),
       textInput(inputId = 'qcCreatorORCID', label = "Primary Contact ORCID", value = "", width = '100%'),
       textInput(inputId = 'qcOrganisation', label = "Organisation*", width = '100%')
     ),
     column(
       width = 4,
       fluidRow(
         column(
           width = 4,
           checkboxInput(inputId = 'qcAvailableOnline',label = "Available Online?",value = FALSE)
         ),
         column(
           width = 8,
           shinyjs::hidden(textInput(inputId = "qcURI", label = "KNB URL", value = "", width = '100%')),
           textInput(inputId = "qcALTURI", label = "Online Location (URL)", value = "", width = '100%')
         )
       ),
       fluidRow(
         column(
           width = 12,
           # options below based on EML frequency types https://eml.ecoinformatics.org/schema/
           selectInput(inputId = 'qcMaintenance', label = "Update Frequency",
                       choices = c("continually",
                                   "daily",
                                   "weekly",
                                   "monthly",
                                   "annually",
                                   "irregular",
                                   "unknown",
                                   "notPlanned"),
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
