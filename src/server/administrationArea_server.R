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