# observe agreement tickbox - action: enable or disable Submit button
observeEvent(input$dataSharingAgreementAccept,{
  shinyjs::toggle('newUserRegisterSubmit')
})


# observer new user registration submit button - action: write csv file with info
# TODO: change from saving as CSV to producing some kind of email to data admin inbox
observeEvent(input$newUserRegisterSubmit, {
  # check for blank fields
  if(input$newUserName != "" && input$newUserEmail != "" && input$newUserOrganisation != ""){
    # save csv file directly to AWS S3 storage because shinyserver write permissions prevented local write_csv
    # create temp area in memory to write to
    rc <- rawConnection(raw(0), 'r+')
    # write csv to temp area
    write_csv(tibble(name = input$newUserName, email = input$newUserEmail, org = input$newUserOrganisation,acceptDSA = input$dataSharingAgreementAccept,acceptOrgPromote = input$dataSharingAgreementAdvertiseOrganisation),rc)
    # send csv object from temp area to S3
    aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/userRegistration",object = paste0(as.character(floor(runif(1,min = 100000,max = 999999))),"_newUser.csv"))
    # close and remove temp area
    close(rc)
    # clear registration fields
    updateTextInput(session, inputId = 'newUserName',value = "")
    updateTextInput(session, inputId = 'newUserEmail',value = "")
    updateTextInput(session, inputId = 'newUserOrganisation',value = "")
    updateCheckboxInput(session, inputId = 'dataSharingAgreementAccept',value = FALSE)
    updateCheckboxInput(session, inputId = 'dataSharingAgreementAdvertiseOrganisation',value = FALSE)
    # send user back to intro tab
    updateTabItems(session, 'menu1', 'searchlsf')
    
    # modal window to thank for new registration
    showModal(
      modalDialog(title = "Thank You!",
                  p("You're information has been sent to the Salmon Ecosystem Data Hub administrator and we will be in touch soon with your logon details"),
                  easyClose = TRUE
      )
    )
  }else{
    output$registerFieldBlank <- renderUI(span(style='color:red',"Please complete all fields"))
  }

})