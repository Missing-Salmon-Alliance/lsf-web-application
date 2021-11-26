############################
#administratorArea_ui.R START
############################
tabItem( # tabItem 
  tabName = 'adminZone',
  uiOutput('adminTabUI')
)

### DATAONE CODE REQUIRED FOR FUTURE USE
## FROM GLOBAL.R
# DataONE settings
# SET ENVIRONMENT
# Production
# cn <- dataone::CNode("PROD")
# mn <- dataone::getMNode(cn, "urn:node:KNB")
# d1c <- dataone::D1Client(cn, mn)
# Staging
# cn <- dataone::CNode("STAGING2")
# mn <- dataone::getMNode(cn, "urn:node:mnTestKNB") # note command 'unlist(lapply(listNodes(cn), function(x) x@subject))' from dataone-federation vignette
# d1c <- dataone::D1Client(cn, mn)
#am <- dataone::AuthenticationManager()
##
## FROM infoPopOvers_server.R
# shinyBS::addPopover(session, id = 'sourceKNBURIUI',title = "Enter a KNB URN or DOI",
#                     content = HTML(paste0(em("If the data you would like to register is already within the "),a(href="https://knb.ecoinformatics.org/","Knowledge Network for Biocomplexity"),em(" (KNB) then this can be loaded into the form via the URN or DOI of the KNB data package."),
#                                           p(),em("This will automatically populate the fields within 'Data Source Details', 'Temporal Coverage' and 'Geographic Coverage', as far as the equivalent information exists in KNB."),
#                                           p(),em("For example enter a URN:UUID:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx or DOI:10.5063/xxxxxx and press 'Load...'"),
#                                           p(),em("If the KNB object is set to private, you may use your KNB access token below."))),
#                     placement = 'top',
#                     options = list(container = "body")
# )
##
## FROM submitDataSource_Server.R
# # define all EML nodes relevant to our metadata
# emlNodes <- list(title = "/eml:eml/dataset/title",
#                  altURL = "/eml:eml/dataset/alternateIdentifier",
#                  abstract = "/eml:eml/dataset/abstract/para",
#                  pubDate = "/eml:eml/dataset/pubDate",
#                  rights = "/eml:eml/dataset/intellectualRights/para",
#                  metaProviderGivenName = "/eml:eml/dataset/metadataProvider/individualName/givenName",
#                  metaProviderSurName = "/eml:eml/dataset/metadataProvider/individualName/surName",
#                  metaProvidereMail = "/eml:eml/dataset/metadataProvider/electronicMailAddress",
#                  metaProviderUserID = "/eml:eml/dataset/metadataProvider/userId",
#                  metaProviderOrg = "/eml:eml/dataset/metadataProvider/organizationName",
#                  creatorGivenName = "/eml:eml/dataset/creator/individualName/givenName",
#                  creatorSurName = "/eml:eml/dataset/creator/individualName/surName",
#                  creatoreMail = "/eml:eml/dataset/creator/electronicMailAddress",
#                  creatorUserID = "/eml:eml/dataset/creator/userId",
#                  creatorOrg = "/eml:eml/dataset/creator/organizationName",
#                  geogDescription = "/eml:eml/dataset/coverage/geographicCoverage/geographicDescription",
#                  geogWest = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate",
#                  geogEast = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate",
#                  geogNorth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate",
#                  geogSouth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate",
#                  dateStart = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/beginDate/calendarDate",
#                  dateEnd = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/endDate/calendarDate",
#                  taxonDescription = "/eml:eml/dataset/coverage/taxonomicCoverage/generalTaxonomicCoverage",
#                  taxonRankName = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankName",
#                  taxonRankValue = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankValue",
#                  maintDescription = "/eml:eml/dataset/maintenance/description",
#                  maintFrequency = "/eml:eml/dataset/maintenance/maintenanceUpdateFrequency",
#                  contactID = "/eml:eml/dataset/contact/references",
#                  keywordSet = "/eml:eml/dataset/keywordSet/keyword")
# ##
# ## Observe event refreshKNBToken actionButton
# ## pop-up to capture user input new KNB token
# ## updates token_info reactiveVal
# observeEvent(input$refreshKNBToken,
#              {
#                showModal(modalDialog(title = "Refresh KNB Token",
#                                      textInput('KNBTokenValue', "Token"),
#                                      easyClose = FALSE, footer = tagList(
#                                        modalButton("Cancel"),
#                                        actionButton('submitKNBToken', "Submit"))
#                ))
#                
#                
#              })
# 
# observeEvent(input$submitKNBToken, {
#   
#   options(dataone_token = input$KNBTokenValue)
#   # if no token available, create dummy info data.frame
#   tryCatch(token_info(dataone::getTokenInfo(am)),error = function(e){token_info(data.frame(expired=TRUE))})
#   removeModal()
# })
# ##
# ##
# 
# # Display KNB Token
# output$expiryDatetimeKNBToken <- renderText({
#   req(token_info())
#   if(token_info()$expired){
#     infoOut <- 'Token Expired or Not Valid'
#     
#   }else{
#     infoOut <- 'Token Validated!'
#   }
#   infoOut
# })
# 
# #output$expiryDatetimeKNBToken <- DT::renderDT(token_info())
# 
# # Observer for actionButton Load From KNB
# observeEvent(input$loadKNB,{
#   
#   # Load information from KNB based on UUID
#   # Pull data via xml
#   # Check for: EMPTY STRING (bad) - ELSE modal invalid UUID
#   if(input$sourceKNBURI != ""){
#     # Check for: getObject error (bad) - Set sessionXML(NULL)
#     tryCatch(sessionXML(dataone::getObject(d1c@mn,input$sourceKNBURI)),error = function(e){sessionXML(NULL)})
#     # Check for: sessionXML(NULL) (bad) - ELSE modal invalid UUID OR invalid KNB Token
#     if(!is.null(sessionXML())){
#       xml_doc <- xml2::read_xml(sessionXML())
#       
#       # Update the text areas with results
#       updateTextAreaInput(session,inputId = 'sourceTitle',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$title)))
#       updateTextInput(session,inputId = 'sourceCreator',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorSurName)))
#       updateTextInput(session,inputId = 'sourceOrganisation',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorOrg)))
#       updateTextAreaInput(session,inputId = 'sourceAbstract',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$abstract)))
#       updateTextInput(session,inputId = 'sourceCreatorEmail',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatoreMail)))
#       updateTextInput(session,inputId = 'sourceURI',value = paste0("https://knb.ecoinformatics.org/view/",input$sourceKNBURI)) #TODO Improve this population
#       updateTextInput(session,inputId = 'sourceCreatorORCID',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorUserID)))
#       updateTextInput(session,inputId = 'sourceALTURI',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$altURL)))
#       
#       updateNumericInput(session,inputId = 'sourceStartYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateStart)), format = "%Y")))
#       updateNumericInput(session,inputId = 'sourceEndYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateEnd)), format = "%Y")))
#       
#       updateTextAreaInput(session,inputId = 'sourceGeographicDescription',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogDescription)))
#       updateNumericInput(session,inputId = 'submitNorth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogNorth)))
#       updateNumericInput(session,inputId = 'submitEast',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogEast)))
#       updateNumericInput(session,inputId = 'submitSouth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogSouth)))
#       updateNumericInput(session,inputId = 'submitWest',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogWest)))
#       
#       
#       # set session UUID to match the KNB URI/DOI
#       # TODO: This will create a mismatch between non-KNB and KNB sourced metadata as the ID will start with DOI: or URN:UUID:
#       # TODO: How to stop this being overwritten at the start of the actionButton submitNewDataSource process
#       sessionUUID(input$sourceKNBURI)
#       # set sessionXML to NULL to avoid spill over to new load
#       sessionXML(NULL)
#     }else{
#       #TODO: regex to detect correct UUID or DOI
#       showModal(modalDialog(
#         title = "Please check KNB ID or KNB Token expiry, the supplied ID returned no KNB object.",
#         sessionUUID(),
#         easyClose = TRUE,
#         footer = NULL
#       ))
#     }
#   }else{
#     showModal(modalDialog(
#       title = "Invalid URI or Blank string",
#       easyClose = TRUE,
#       footer = NULL
#     ))
#   }
#   
# })
##
############################
#administratorArea_ui.R START
############################