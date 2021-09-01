# function to load all
# TODO: REMEMBER TO ADD ANY NEW INPUTS HERE!

# reactive value to hold the current database state of the selected metadata node
qcDatabaseMetadata <- reactiveVal()

# log update function
qcLogUpdate <- function(resultList,qcAreaVal){
  #### add entry to log (QC log)
  # Create log for submissions
  # result requires transposed to fit log file
  resultTransform <- data.frame(t(resultList),row.names = NULL)[2,]
  names(resultTransform) <- resultList$type
  # reduce result columns to required information
  resultTransform <- resultTransform[,neo4rResultFields]
  
  # create QC base info, date,user,UUID of metadata node
  qcLogBase <- data.frame(date = Sys.time(),user = user_info()$user_info$email,UUID = qcDatabaseMetadata()$m$metadataUUID,qcArea = qcAreaVal)
  # combine the columns into a single row
  logFileDataFrame <- dplyr::bind_cols(qcLogBase,resultTransform)
  # send log to AWS, append to existing file
  # get current logfile
  # TODO - THis process can cause a collision if two people submit at the exact same time, add some collision avoidance
  logfile <- readr::read_csv(aws.s3::get_object(object = "qualityControl.log",bucket = "likelysuspects-datastore/logs"),col_types = 'Tccccccccccc')
  # append new log item
  logfile <- dplyr::bind_rows(logfile,logFileDataFrame)
  # create temp area in memory to write to
  rc <- rawConnection(raw(0), 'r+')
  # write csv to temp area
  readr::write_csv(logfile,rc)
  # send csv object from temp area to S3
  aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/logs",object = "qualityControl.log")
  # close and remove temp area
  close(rc)
}

# load values in to metadata fields
qcLoadMetadata <- function(){
  
  #reset Source Details Section
  updateTextAreaInput(session,inputId = 'qcTitle',value = qcDatabaseMetadata()$m$metadataTitle)
  updateTextInput(session,inputId = 'qcCreator',value = qcDatabaseMetadata()$m$metadataCreator)
  updateTextInput(session,inputId = 'qcOrganisation',value = qcDatabaseMetadata()$m$metadataOrganisation)
  
  updateTextAreaInput(session,inputId = 'qcAbstract',value = qcDatabaseMetadata()$m$metadataAbstract)
  updateTextInput(session,inputId = 'qcCreatorEmail',value = qcDatabaseMetadata()$m$metadataCreatorEmail)
  updateTextInput(session,inputId = 'qcURI',value = qcDatabaseMetadata()$m$metadataKNBURI) # KNB URL
  updateTextInput(session, inputId = 'qcALTURI', value = qcDatabaseMetadata()$m$metadataAltURI)
  
  updateTextInput(session,inputId = 'qcCreatorORCID',value = qcDatabaseMetadata()$m$metadataCreatorORCID)
  updateSelectInput(session, inputId = 'qcMaintenance', selected = qcDatabaseMetadata()$m$metadataMaintenance)
  updateCheckboxInput(session, inputId = 'qcAvailableOnline', value = qcDatabaseMetadata()$m$metadataAvailableOnline)
  updateCheckboxInput(session,inputId = 'qcEmbargoEndToggle',value = qcDatabaseMetadata()$m$metadataEmbargoed)
  updateTextInput(session,inputId = 'qcEmbargoEnd',value = qcDatabaseMetadata()$m$metadataEmbargoEnd)

}

# load values in to geo temporal fields
qcLoadGeoTemporal <- function(){
  #reset Source Temporal Section
  updateNumericInput(session,inputId = 'qcStartYear',value = qcDatabaseMetadata()$m$metadataCoverageStartYear)
  updateNumericInput(session,inputId = 'qcEndYear',value = qcDatabaseMetadata()$m$metadataCoverageEndYear)
  selectedMonths <- stringr::str_split(qcDatabaseMetadata()$m$metadataCoverageMonthsOfYear,",")[[1]]
  shinyWidgets::updateCheckboxGroupButtons(session, inputId = 'qcMonthsOfYear',selected = selectedMonths)
  
  #reset Source Geography Section
  updateTextAreaInput(session, inputId = 'qcGeographicDescription',value = qcDatabaseMetadata()$m$metadataGeographicDescription)
  updateNumericInput(session, inputId = 'qcNorth', value = qcDatabaseMetadata()$m$metadataCoverageNorth)
  updateNumericInput(session, inputId = 'qcEast', value = qcDatabaseMetadata()$m$metadataCoverageEast)
  updateNumericInput(session, inputId = 'qcSouth', value = qcDatabaseMetadata()$m$metadataCoverageSouth)
  updateNumericInput(session, inputId = 'qcWest', value = qcDatabaseMetadata()$m$metadataCoverageWest)
  
  output$originalNorthValue <- renderText({qcDatabaseMetadata()$m$metadataCoverageNorth})
  output$originalSouthValue <- renderText({qcDatabaseMetadata()$m$metadataCoverageSouth})
  output$originalEastValue <- renderText({qcDatabaseMetadata()$m$metadataCoverageEast})
  output$originalWestValue <- renderText({qcDatabaseMetadata()$m$metadataCoverageWest})
  
}


# load values in to variable classes and domains
qcLoadDomainESV <- function(){
  #reset Source Domain/ESV Section
  shinyWidgets::updateCheckboxGroupButtons(session, inputId = 'qcDomainNodeList',selected = character(0))
  

}

# Geographic Detail Server
output$qcMap <- leaflet::renderLeaflet({
  leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 10)) %>% # maxZoom set so that user can always see their rectangle in context of a coastline
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>%
    leaflet::addRectangles(-15,61,-14,60,group = 'qcRectangle') %>% 
    leaflet::fitBounds(-15,61,-14,60,options = leaflet::leafletOptions(maxZoom = 5))
})
# observer for 4 numeric inputs lat/lon - action update userRectangle on submitMap, change zoom and centre
observeEvent(input$qcEast | input$qcWest | input$qcNorth | input$qcSouth,{
  req(input$qcEast && input$qcWest && input$qcNorth && input$qcSouth) # catch empty value, prevent crash?
  leaflet::leafletProxy('qcMap') %>%
    leaflet::clearGroup(group = 'qcRectangle') %>%
    leaflet::addRectangles(input$qcWest,input$qcNorth,input$qcEast,input$qcSouth,group = 'qcRectangle') %>%
    leaflet::fitBounds(input$qcWest,input$qcNorth,input$qcEast,input$qcSouth)
})

# observe user click on map and set coordinates
observeEvent(input$qcMap_click,{
  updateNumericInput(session,inputId = 'qcNorth',value = round(input$qcMap_click$lat,digits = 4))
  updateNumericInput(session,inputId = 'qcEast',value = round(input$qcMap_click$lng,digits = 4))
  updateNumericInput(session,inputId = 'qcSouth',value = round(input$qcMap_click$lat,digits = 4))
  updateNumericInput(session,inputId = 'qcWest',value = round(input$qcMap_click$lng,digits = 4))
})

# Observe ID change - Action: Load data from database and update all fields
observeEvent(input$QCidSelector,{
  qcDatabaseMetadata(neo4r::call_neo4j(paste0("MATCH (m:Metadata) WHERE id(m) = ",input$QCidSelector," RETURN m;"),con = neo_con, type = 'row'))
  qcLoadMetadata()
  qcLoadGeoTemporal()
  qcLoadDomainESV()
})

# Observe User Reset to DB values - Action: Reload values from database
# separate button for each input area therefore three
observeEvent(input$resetQCMetadata,{
  qcDatabaseMetadata(neo4r::call_neo4j(paste0("MATCH (m:Metadata) WHERE id(m) = ",input$QCidSelector," RETURN m;"),con = neo_con, type = 'row'))
  qcLoadMetadata()
})
observeEvent(input$resetQCGeoTemporal,{
  qcDatabaseMetadata(neo4r::call_neo4j(paste0("MATCH (m:Metadata) WHERE id(m) = ",input$QCidSelector," RETURN m;"),con = neo_con, type = 'row'))
  qcLoadGeoTemporal()
})
observeEvent(input$resetQCDomainESV,{
  qcDatabaseMetadata(neo4r::call_neo4j(paste0("MATCH (m:Metadata) WHERE id(m) = ",input$QCidSelector," RETURN m;"),con = neo_con, type = 'row'))
  qcLoadDomainESV()
})

# Observe User Update to DB values - Action: Apply new values to database
# separate button for each input area therefore three
observeEvent(input$submitQCMetadata,{
  # match metadata node
  # update metadata fields to values in form
  # update lastModified value in [:HAS_SUBMITTED] relationship set to Sys.time()
  # update status in [:HAS_SUBMITTED] relationship set to startedQC to signify that QC has begun
  updateMetadataNodeQuery <- paste0("MATCH (m:Metadata)-[r:HAS_SUBMITTED]-() WHERE id(m) = ",input$QCidSelector,
                                    " SET m.metadataTitle = '",sanitiseFreeTextInputs(input$qcTitle),
                                    "', m.metadataCreator = '",sanitiseFreeTextInputs(input$qcCreator),
                                    "', m.metadataOrganisation = '",sanitiseFreeTextInputs(input$qcOrganisation),
                                    "', m.metadataAbstract = '",sanitiseFreeTextInputs(input$qcAbstract),
                                    "', m.metadataCreatorEmail = '",sanitiseFreeTextInputs(input$qcCreatorEmail),
                                    "', m.metadataAltURI = '",sanitiseFreeTextInputs(input$qcALTURI),
                                    "', m.metadataCreatorORCID = '",sanitiseFreeTextInputs(input$qcCreatorORCID),
                                    "', m.metadataMaintenance = '",input$qcMaintenance,
                                    "', m.metadataAvailableOnline = ",input$qcAvailableOnline,
                                    ", m.metadataEmbargoed = ",input$qcEmbargoEndToggle,
                                    ", m.metadataEmbargoEnd = '",sanitiseFreeTextInputs(input$qcEmbargoEnd),
                                    "', m.metadataLastModified = '",Sys.time(),
                                    "', r.status = 'startedQC', r.lastModified = '",Sys.time(),"';")
  
  resultUpdateMetadataNode <- neo4r::call_neo4j(updateMetadataNodeQuery,neo_con,type = 'row',include_stats = T,include_meta = T)
  qcLogUpdate(resultList = resultUpdateMetadataNode,qcAreaVal = 'metadata')

})

observeEvent(input$submitQCGeoTemporal,{
  # match metadata node
  # update geotemporal fields to values in form
  # update lastModified value in [:HAS_SUBMITTED] relationship set to Sys.time()
  # update status in [:HAS_SUBMITTED] relationship set to startedQC to signify that QC has begun
  updateMetadataNodeQuery <- paste0("MATCH (m:Metadata)-[r:HAS_SUBMITTED]-() WHERE id(m) = ",input$QCidSelector,
                                    " SET m.metadataGeographicDescription = '",sanitiseFreeTextInputs(input$qcGeographicDescription),
                                    "', m.metadataCoverageNorth = '",sanitiseFreeTextInputs(input$qcNorth),
                                    "', m.metadataCoverageEast = '",sanitiseFreeTextInputs(input$qcEast),
                                    "', m.metadataCoverageSouth = '",sanitiseFreeTextInputs(input$qcSouth),
                                    "', m.metadataCoverageWest = '",sanitiseFreeTextInputs(input$qcWest),
                                    "', m.metadataCoverageStartYear = '",sanitiseFreeTextInputs(input$qcStartYear),
                                    "', m.metadataCoverageEndYear = '",sanitiseFreeTextInputs(input$qcEndYear),
                                    "', metadataCoverageMonthsOfYear:'",paste(input$qcMonthsOfYear,collapse = ","),
                                    "', m.metadataLastModified = '",Sys.time(),
                                    "', r.status = 'startedQC', r.lastModified = '",Sys.time(),"';")
  
  resultUpdateMetadataNode <- neo4r::call_neo4j(updateMetadataNodeQuery,neo_con,type = 'row',include_stats = T,include_meta = T)
  qcLogUpdate(resultList = resultUpdateMetadataNode,qcAreaVal = 'geotemporal')
  
})

observeEvent(input$qcConfirmComplete,{
  # TODO: Apply these changes only when user clicks QC Completed button
  # set QC person as current user, new relationship HAS_QCED
  # set metadataQCCheck = TRUE
  #updateMetadataNodeQuery <- paste0("MATCH (m:Metadata)-[r:HAS_SUBMITTED]-() WHERE id(m) = ",input$QCidSelector,
                                    # " SET m.metadataTitle = '",sanitiseFreeTextInputs(input$qcTitle),
                                    # "', m.metadataCreator = '",sanitiseFreeTextInputs(input$qcCreator ),
                                    # "', m.metadataOrganisation = '",sanitiseFreeTextInputs(input$qcOrganisation),
                                    # "', m.metadataAbstract = '",sanitiseFreeTextInputs(input$qcAbstract),
                                    # "', m.metadataCreatorEmail = '",sanitiseFreeTextInputs(input$qcCreatorEmail),
                                    # "', m.metadataAltURI = '",sanitiseFreeTextInputs(input$qcALTURI),
                                    # "', m.metadataCreatorORCID = '",sanitiseFreeTextInputs(input$qcCreatorORCID),
                                    # "', m.metadataMaintenance = ",input$qcMaintenance,
                                    # ", m.metadataAvailableOnline = ",input$qcAvailableOnline,
                                    # ", m.metadataEmbargoed = ",input$qcEmbargoEndToggle,
                                    # ", m.metadataEmbargoEnd = '",sanitiseFreeTextInputs(input$qcEmbargoEnd),
                                    # "', m.metadataLastModified = '",Sys.time(),
                                    # "', r.status = 'startedQC', r.lastModified = '",Sys.time(),"';")
  
})

  



# Observer to pass file upload details to sessionFile reactive value
# This is passed to a reactiveVal so that it can be cleared out once user has submitted the form.
# observeEvent(input$qcUploadDataFile,{
#   sessionFile(input$qcUploadDataFile)
# })

# Reactive process to tell shiny what to do with the users file. Activated by submitNewDataSource
# uses UUID generated during actionButton click to submit datasource
# if no file has been uploaded sessionFile reactiveVal will be NULL and so no upload is triggered
# uploadReactive <- reactive({
#   if(!is.null(sessionFile())){
#     fileName <- paste0(sessionUUID(),"_",sessionFile()$name)
#     aws.s3::put_object(file = sessionFile()$datapath,object = fileName,bucket = "likelysuspects-datastore/data")
#   }
#   
# })





# observeEvent(input$qcUpdateMetadataFields, {
# 
#     metadataNodeCreateQuery <- paste("MATCH (p:Person{personEmail:'",user_info()$user_info$email,
#                                      "'}) CREATE (p)-[:HAS_SUBMITTED{created:'",Sys.time(),
#                                      "',lastModified:'",Sys.time(),
#                                      "',status:'pendingQC'}]->(:Metadata{metadataTitle:'",sanitiseFreeTextInputs(input$sourceTitle),
#                                      "',metadataCreator:'",sanitiseFreeTextInputs(input$sourceCreator),
#                                      "',metadataKNBURI:'",input$sourceURI,
#                                      "',metadataAlternateURI:'",sanitiseFreeTextInputs(input$sourceALTURI),
#                                      "',metadataOrganisation:'",sanitiseFreeTextInputs(input$sourceOrganisation),
#                                      "',metadataAbstract:'",sanitiseFreeTextInputs(input$sourceAbstract),
#                                      "',metadataAvailableOnline:",input$sourceAvailableOnline,
#                                      ",metadataEmbargoed:",input$embargoEndToggle,
#                                      ",metadataEmbargoEnd:'",sanitiseFreeTextInputs(input$embargoEnd),
#                                      "',metadataGeographicDescription:'",sanitiseFreeTextInputs(input$sourceGeographicDescription),
#                                      "',metadataCreatorEmail:'",sanitiseFreeTextInputs(input$sourceCreatorEmail),
#                                      "',metadataCreatorORCID:'",sanitiseFreeTextInputs(input$sourceCreatorORCID),
#                                      "',metadataCoverageStartYear:'",input$sourceStartYear,
#                                      "',metadataCoverageEndYear:'",input$sourceEndYear,
#                                      "',metadataCoverageMonthsOfYear:'",paste(input$monthsOfYear,collapse = ","),#collapse months of year into csv string
#                                      "',metadataCoverageNorth:'",input$submitNorth,
#                                      "',metadataCoverageSouth:'",input$submitSouth,
#                                      "',metadataCoverageEast:'",input$submitEast,
#                                      "',metadataCoverageWest:'",input$submitWest,
#                                      "',metadataMaintenance:'",input$sourceMaintenance,
#                                      "',metadataQCCheck:",FALSE,
#                                      ",metadataCoverageCentroid:'",paste0("POINT (",lngCenter," ",latCenter,")"),
#                                      "',metadataCoverageIntersectICESEcoRegion:'","",
#                                      "',metadataCoverageIntersectNAFODivision:'","",
#                                      "',metadataCoverageIntersectMigrationRoutes:'","",
#                                      "',metadataUUID:'",sessionUUID(),
#                                      "',metadataFilename:'",filename,
#                                      "',metadataTimestamp:'",Sys.time(),
#                                      "'});",sep = "")
#     
#     resultCreateMetadataNode <- neo4r::call_neo4j(metadataNodeCreateQuery,neo_con,type = 'row',include_stats = T,include_meta = T)
#     #results$value <- results$value + resultCreateMetadataNode$value
#     
# 
#     
#     #####################
#     # Create relationships to ESV/Variable Class using DOMAIN SPECIFIC RELATIONSHIPS
#     #####################
#     # initiate a vector to capture the queries
#     queryMasterList <- c()
#     
#     # create base query elements
#     queryBase <- c("MATCH (esv:EssentialSalmonVariable{esvTitle:'","'}),(md:Metadata{metadataUUID:'","'}) CREATE (esv)<-[:HAS_ESV{domain:'","'}]-(md);")
#     # cycle through all the selected ESV for each domain IS THIS EFFICIENT?
#     for(domain in input$domainNodeList){
#       # create basic relationship between metadata and domain (capture even if user selects no ESV's)
#       addDomainQuery <- paste0("MATCH (d:Domain{domainTitle:'",domain,"'}),(md:Metadata{metadataUUID:'",sessionUUID(),"'}) CREATE (md)-[:HAS_DOMAIN]->(d);")
#       queryMasterList <- append(queryMasterList,addDomainQuery)
#       
#       # build full ESV list for domain (collapse from 3 categories presented in the UI)
#       variableClassesForDomain <- c(input[[paste0(domain,"_biological")]],input[[paste0(domain,"_physical")]],input[[paste0(domain,"_salmontrait")]])
#       for(esv in variableClassesForDomain){
#         # create sub query for single esv-metadata
#         querySubMaster <- paste(queryBase[1],esv,queryBase[2],sessionUUID(),queryBase[3],domain,queryBase[4],sep = "")
#         # add sub query to master query list
#         queryMasterList <- append(queryMasterList,querySubMaster)
#       }
#     }
#     
#     #####################^^^^^^^^
#     # Create relationships using DOMAIN SPECIFIC RELATIONSHIPS
#     #####################
#     
#     for(query in queryMasterList){
#       resultCreateESVRelationships <- neo4r::call_neo4j(query,neo_con,type = 'row',include_stats = T,include_meta = T)
#       resultCreateMetadataNode$value <- resultCreateMetadataNode$value + resultCreateESVRelationships$value
#     }
#     
#     
#     
#   }else{
#     # Else no action taken, empty results tibble required
#     # create a dummy results with 0 values for display only
#     resultCreateMetadataNode <- neo4r::call_neo4j("MATCH (n:Metadata{metadataUUID:'DummyDummy'}) RETURN n;",neo_con,type = 'row',include_stats = T,include_meta = T)
#     
#   }
#   
#   # Trigger File Upload
#   uploadReactive()
#   
#   # Create log for submissions
#   resultCreateMetadataNode # table of graph database updates
#   # requires transposed to fit log file
#   resultCreateMetadataNodeTransform <- data.frame(t(resultCreateMetadataNode),row.names = NULL)[2,]
#   names(resultCreateMetadataNodeTransform) <- resultCreateMetadataNode$type
#   
#   # create user feedback table
#   userSubmitSourceFeedback(data.frame(date = Sys.time(),user = user_info()$user_info$email, Title = input$sourceTitle,File = filename,UUID = sessionUUID()))
# 
#   logFileDataFrame <- dplyr::bind_cols(userSubmitSourceFeedback(),resultCreateMetadataNodeTransform)
#   
#   # send log to AWS, append to existing file
#   # get current logfile
#   # TODO - THis process can cause a collision if two people submit at the exact same time, add some collision avoidance
#   logfile <- readr::read_csv(aws.s3::get_object(object = "userSubmit.log",bucket = "likelysuspects-datastore/logs"),col_types = 'Tcccccccccccccccccc')
#   # append new log item
#   logfile <- dplyr::bind_rows(logfile,logFileDataFrame)
#   # create temp area in memory to write to
#   rc <- rawConnection(raw(0), 'r+')
#   # write csv to temp area
#   readr::write_csv(logfile,rc)
#   # send csv object from temp area to S3
#   aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/logs",object = "userSubmit.log")
#   # close and remove temp area
#   close(rc)
# 
#   
#   ##############
#   # Clear INPUTS
#   ##############
#   resetAll()
#   
#   #Close confirm modal
#   removeModal()
#   
#   showModal(submitSourceResultModal())
#   
# })