############################
# informationTab_server.R START
############################

# observer for intro page register - action send user to new member registration page
observeEvent(input$introRegisterLink, {
  updateTabItems(session, 'menu1', 'newMemberRegistration')
})

# output$introMemOfAgreementDownload <- downloadHandler(
#   filename = "MemorandumofAgreement.pdf",
#   contentType = 'pdf',
#   
# )
############################
# informationTab_server.R END
############################