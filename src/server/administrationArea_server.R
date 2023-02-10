# User statistics
activeUsers = reactiveValues(count = 0)

onSessionStart = isolate({
  activeUsers$count = activeUsers$count + 1
})

onSessionEnded(function() {
  isolate({
    activeUsers$count = activeUsers$count - 1
  })
})

output$activeUserOutput = renderText({activeUsers$count})
#   h1(paste0("There are ", activeUsers$count, " user(s) connected to this app"))
# })

output$adminTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    tabsetPanel(id = "useradmin",
                selected = "User Administration",
                tabPanel(
                  id = 'useradmintabNewUser',
                  title = "User Administration",
                  box(title = "New User Form",
                      status = 'warning',
                      textInput('useradminFullname',label = "Fullname"),
                      textInput('useradminEmail',label = "Email"),
                      textInput('useradminAffiliation',label = "Affiliation"),
                      passwordInput('useradminPassword',label = "Password"),
                      checkboxInput('useradminAcceptDSA',label = "DSA Accepted?",value = FALSE),
                      checkboxInput('useradminPromoteOrg',label = "Promote Organisation as DSG Member?",value = FALSE),
                      checkboxInput('useradminAdmin',label = "Administrator?",value = FALSE)
                  ),
                  actionButton('addNewUser',"Add New User", icon = icon('user-check')),
                  verbatimTextOutput('activeUserOutput')
                ),
                tabPanel(
                  id = 'useradmintabQC',
                  title = "Quality Control",
                  fluidRow(
                    source("./src/ui/qualityControlUI/qcDataSource_metadataFields_ui.R",local = TRUE)$value,
                    source("./src/ui/qualityControlUI/qcDataSource_geoTemporal_ui.R",local = TRUE)$value,
                    source("./src/ui/qualityControlUI/qcDataSource_domainsESV_ui.R",local = TRUE)$value
                  )
                )
    ) # close tabsetPanel
  }
})

observeEvent(input$addNewUser,{
  # send details to adminCreateNewUser function located in custom_functions.R
  adminCreateNewUser(fullname = input$useradminFullname,
                     pw = input$useradminPassword,
                     email = input$useradminEmail,
                     affiliation = input$useradminAffiliation,
                     acceptDSA = input$useradminAcceptDSA,
                     promoteORG = input$useradminPromoteOrg,
                     admin = input$useradminAdmin)
  # reset input fields
  updateTextInput(session, inputId = 'useradminFullname',value = "")
  updateTextInput(session, inputId = 'useradminEmail',value = "")
  updateTextInput(session, inputId = 'useradminAffiliation',value = "")
  updateTextInput(session, inputId = 'useradminPassword',value = "")
  updateCheckboxInput(session, inputId = 'useradminAcceptDSA',value = FALSE)
  updateCheckboxInput(session, inputId = 'useradminPromoteOrg',value = FALSE)
  updateCheckboxInput(session, inputId = 'useradminAdmin',value = FALSE)
})

# Quality Control Section