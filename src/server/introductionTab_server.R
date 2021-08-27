############################
# informationTab_server.R START
############################

output$varClassesFull <- DT::renderDT({
  input$varClassesDomain
  variableClasses <- neo4r::call_neo4j(paste0("MATCH (esv:EssentialSalmonVariable)-[r:HAS_DOMAIN]->(d:Domain{domainTitle: '",input$varClassesDomain,"'}) RETURN esv;"),neo_con,type='row')
  variableClasses$esv[variableClasses$esv$esvCategory == input$varClassesCats,c('esvTitle','esvDescription')]
  },
  selection = 'single',
  rownames = FALSE,
  colnames = c('Title','Description'),
  options = list(pageLength = 15, # set initial page result length
                 lengthChange = FALSE, # disable result length change
                 searching = FALSE, # disable search field
                 paging = FALSE, # disable paging menu
                 info = FALSE)
  )

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