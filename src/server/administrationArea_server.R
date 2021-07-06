observeEvent(input$addNewUser,{
  adminCreateNewUser(input$useradminFullname,input$useradminPassword,input$useradminEmail,input$useradminAffiliation,input$useradminAdmin)
  updateTextInput(session, inputId = 'useradminFullname',value = "")
  updateTextInput(session, inputId = 'useradminEmail',value = "")
  updateTextInput(session, inputId = 'useradminAffiliation',value = "")
  updateTextInput(session, inputId = 'useradminPassword',value = "")
  updateCheckboxInput(session, inputId = 'useradminAdmin',value = FALSE)
})