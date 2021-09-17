# NEW VERSION THAT LOADS KNB DATA VIA EML QUERY

############################
# submitDataSource_server.R START
############################

############
# SIDEBAR Conditional UI
output$uploadDataFileUI <- renderUI({
  fileInput(
    'uploadDataFile',
    'File Drop',
    multiple = FALSE,
    accept = NULL,
    width = NULL,
    buttonLabel = "Browse...",
    placeholder = "No file selected"
  )
})

output$clearSubmitFormUI <- renderUI({
  actionButton(inputId = 'clearSubmitForm',label = "Clear Form")
})

# two submit buttons that do the same thing but one is located on the sidebar the other at the bottom of the main submit page
output$submitNewDataSourceBodyUI <- renderUI({
  actionButton(inputId = 'submitNewDataSourceBody',label = "Submit New Data Source", class = 'btn-success', style='float:right;')
})
output$submitNewDataSourceSidebarUI <- renderUI({
  actionButton(inputId = 'submitNewDataSourceSidebar',label = "Submit New Data Source", class = 'btn-success')
})

# KNB User Interface Inputs COMMENTED OUT FOR NOW AS FUNCTIONALITY NOT FULLED DESCRIBED ON SITE INFORMATION
# output$sourceKNBURIUI <- renderUI({
#   textInput(inputId = 'sourceKNBURI', label = "",placeholder = "Enter a KNB URN or DOI")
# })
# output$loadKNBUI <- renderUI({
#   actionButton(inputId = 'loadKNB',label = "Load...")
# })
# output$refreshKNBTokenUI <- renderUI({
#   actionButton('refreshKNBToken', "Enter a KNB Access Token")
# })
# output$expiryDatetimeKNBTokenUI <- renderUI({
#   textOutput('expiryDatetimeKNBToken')
# })
############

# function to reset all submit form fields
# TODO: REMEMBER TO ADD ANY NEW INPUTS HERE!
resetAll <- function(){
  #sidebar
  shinyjs::reset('uploadDataFile')
  updateTextInput(session, inputId = 'sourceKNBURI',value = "")
  
  #reset Source Details Section
  updateTextAreaInput(session,inputId = 'sourceTitle',value = "")
  updateTextInput(session,inputId = 'sourceCreator',value = user_info()$user_info$fullname)
  updateTextInput(session,inputId = 'sourceOrganisation',value = user_info()$user_info$affiliation)
  
  updateTextAreaInput(session,inputId = 'sourceAbstract',value = "")
  updateTextInput(session,inputId = 'sourceCreatorEmail',value = user_info()$user_info$email)
  updateTextInput(session,inputId = 'sourceURI',value = "") # KNB URL
  updateTextInput(session, inputId = 'sourceALTURI', value = "")
  
  updateTextInput(session,inputId = 'sourceCreatorORCID',value = "")
  updateSelectInput(session, inputId = 'sourceMaintenance', selected = "notPlanned")
  updateCheckboxInput(session, inputId = 'sourceAvailableOnline', value = FALSE)
  updateCheckboxInput(session, inputId = 'sourceMaintenanceToggle', value = FALSE)
  updateCheckboxInput(session,inputId = 'embargoEndToggle',value = FALSE)
  updateTextInput(session,inputId = 'embargoEnd',value = "")
  #reset Source Domain/ESV Section
  shinyWidgets::updateCheckboxGroupButtons(session, inputId = 'domainNodeList',selected = character(0))
  #shinyWidgets::updateCheckboxGroupButtons(session, inputId = 'esvCategory',selected = character(0))
  
  #reset Source Temporal Section
  updateNumericInput(session,inputId = 'sourceStartYear',value = 2021)
  updateNumericInput(session,inputId = 'sourceEndYear',value = 2021)
  shinyWidgets::updateCheckboxGroupButtons(session, inputId = 'monthsOfYear',selected = character(0))
  updateActionButton(session, 'monthsOfYearToggleAll', label = "Select All")
  
  #reset Source Geography Section
  updateTextAreaInput(session, inputId = 'sourceGeographicDescription',value = "")
  updateNumericInput(session, inputId = 'submitNorth', value = 61)
  updateNumericInput(session, inputId = 'submitEast', value = -14)
  updateNumericInput(session, inputId = 'submitSouth', value = 60)
  updateNumericInput(session, inputId = 'submitWest', value = -15)
  

  # clear session UUID to avoid potential reuse (shouldn't be possible but just in case)
  sessionUUID(NULL)
  # clear session XML to avoid potential reuse (shouldn't be possible but just in case)
  sessionXML(NULL)
  # clear session FILE to avoid potential reuse (shouldn't be possible but just in case)
  sessionFile(NULL)
}

# define all EML nodes relevant to our metadata
emlNodes <- list(title = "/eml:eml/dataset/title",
                 altURL = "/eml:eml/dataset/alternateIdentifier",
                 abstract = "/eml:eml/dataset/abstract/para",
                 pubDate = "/eml:eml/dataset/pubDate",
                 rights = "/eml:eml/dataset/intellectualRights/para",
                 metaProviderGivenName = "/eml:eml/dataset/metadataProvider/individualName/givenName",
                 metaProviderSurName = "/eml:eml/dataset/metadataProvider/individualName/surName",
                 metaProvidereMail = "/eml:eml/dataset/metadataProvider/electronicMailAddress",
                 metaProviderUserID = "/eml:eml/dataset/metadataProvider/userId",
                 metaProviderOrg = "/eml:eml/dataset/metadataProvider/organizationName",
                 creatorGivenName = "/eml:eml/dataset/creator/individualName/givenName",
                 creatorSurName = "/eml:eml/dataset/creator/individualName/surName",
                 creatoreMail = "/eml:eml/dataset/creator/electronicMailAddress",
                 creatorUserID = "/eml:eml/dataset/creator/userId",
                 creatorOrg = "/eml:eml/dataset/creator/organizationName",
                 geogDescription = "/eml:eml/dataset/coverage/geographicCoverage/geographicDescription",
                 geogWest = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate",
                 geogEast = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate",
                 geogNorth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate",
                 geogSouth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate",
                 dateStart = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/beginDate/calendarDate",
                 dateEnd = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/endDate/calendarDate",
                 taxonDescription = "/eml:eml/dataset/coverage/taxonomicCoverage/generalTaxonomicCoverage",
                 taxonRankName = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankName",
                 taxonRankValue = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankValue",
                 maintDescription = "/eml:eml/dataset/maintenance/description",
                 maintFrequency = "/eml:eml/dataset/maintenance/maintenanceUpdateFrequency",
                 contactID = "/eml:eml/dataset/contact/references",
                 keywordSet = "/eml:eml/dataset/keywordSet/keyword")
##
## Observe event refreshKNBToken actionButton
## pop-up to capture user input new KNB token
## updates token_info reactiveVal
observeEvent(input$refreshKNBToken,
             {
               showModal(modalDialog(title = "Refresh KNB Token",
                                     textInput('KNBTokenValue', "Token"),
                                     easyClose = FALSE, footer = tagList(
                                       modalButton("Cancel"),
                                       actionButton('submitKNBToken', "Submit"))
               ))
               
               
             })

observeEvent(input$submitKNBToken, {

  options(dataone_token = input$KNBTokenValue)
  # if no token available, create dummy info data.frame
  tryCatch(token_info(dataone::getTokenInfo(am)),error = function(e){token_info(data.frame(expired=TRUE))})
  removeModal()
})
##
##

# Display KNB Token
output$expiryDatetimeKNBToken <- renderText({
  req(token_info())
  if(token_info()$expired){
    infoOut <- 'Token Expired or Not Valid'
    
  }else{
    infoOut <- 'Token Validated!'
  }
  infoOut
})

#output$expiryDatetimeKNBToken <- DT::renderDT(token_info())

# Observer for actionButton Load From KNB
observeEvent(input$loadKNB,{
  
  # Load information from KNB based on UUID
  # Pull data via xml
  # Check for: EMPTY STRING (bad) - ELSE modal invalid UUID
  if(input$sourceKNBURI != ""){
    # Check for: getObject error (bad) - Set sessionXML(NULL)
    tryCatch(sessionXML(dataone::getObject(d1c@mn,input$sourceKNBURI)),error = function(e){sessionXML(NULL)})
    # Check for: sessionXML(NULL) (bad) - ELSE modal invalid UUID OR invalid KNB Token
    if(!is.null(sessionXML())){
      xml_doc <- xml2::read_xml(sessionXML())
      
      # Update the text areas with results
      updateTextAreaInput(session,inputId = 'sourceTitle',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$title)))
      updateTextInput(session,inputId = 'sourceCreator',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorSurName)))
      updateTextInput(session,inputId = 'sourceOrganisation',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorOrg)))
      updateTextAreaInput(session,inputId = 'sourceAbstract',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$abstract)))
      updateTextInput(session,inputId = 'sourceCreatorEmail',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatoreMail)))
      updateTextInput(session,inputId = 'sourceURI',value = paste0("https://knb.ecoinformatics.org/view/",input$sourceKNBURI)) #TODO Improve this population
      updateTextInput(session,inputId = 'sourceCreatorORCID',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorUserID)))
      updateTextInput(session,inputId = 'sourceALTURI',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$altURL)))
      
      updateNumericInput(session,inputId = 'sourceStartYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateStart)), format = "%Y")))
      updateNumericInput(session,inputId = 'sourceEndYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateEnd)), format = "%Y")))
      
      updateTextAreaInput(session,inputId = 'sourceGeographicDescription',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogDescription)))
      updateNumericInput(session,inputId = 'submitNorth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogNorth)))
      updateNumericInput(session,inputId = 'submitEast',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogEast)))
      updateNumericInput(session,inputId = 'submitSouth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogSouth)))
      updateNumericInput(session,inputId = 'submitWest',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogWest)))
      
      
      # set session UUID to match the KNB URI/DOI
      # TODO: This will create a mismatch between non-KNB and KNB sourced metadata as the ID will start with DOI: or URN:UUID:
      # TODO: How to stop this being overwritten at the start of the actionButton submitNewDataSource process
      sessionUUID(input$sourceKNBURI)
      # set sessionXML to NULL to avoid spill over to new load
      sessionXML(NULL)
    }else{
      #TODO: regex to detect correct UUID or DOI
      showModal(modalDialog(
        title = "Please check KNB ID or KNB Token expiry, the supplied ID returned no KNB object.",
        sessionUUID(),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  }else{
    showModal(modalDialog(
      title = "Invalid URI or Blank string",
      easyClose = TRUE,
      footer = NULL
    ))
  }

})

# Clear/reset form actionButton observer
observeEvent(input$clearSubmitForm,{
  resetAll() # reset all function
})


##########################################################
# SubmitDataSource_server Life-Stage and Domains Selection
##########################################################


# User feedback - if no domain is selected
output$domainSelectionCheck <- renderText('Please Select a Life-Stage Domain...')

# Variable Class (esv) Check Boxes - Controlled by domainNodeList inputs
output$esvPerDomain <- renderUI({
  if(length(input$domainNodeList) == 0){
    textOutput('domainSelectionCheck')
  }else{
    lapply(1:length(input$domainNodeList), function(i) {
      domainName <- input$domainNodeList[i]
      esvNodeListResultPhys <- neo4r::call_neo4j(paste0("MATCH (n:EssentialSalmonVariable{esvCategory:'Physical'})-[:HAS_DOMAIN]-(m:Domain{domainTitle:'",domainName,"'}) RETURN n.esvTitle;"),neo_con, type = 'row')
      esvNodeListResultBiol <- neo4r::call_neo4j(paste0("MATCH (n:EssentialSalmonVariable{esvCategory:'Biological'})-[:HAS_DOMAIN]-(m:Domain{domainTitle:'",domainName,"'}) RETURN n.esvTitle;"),neo_con, type = 'row')
      esvNodeListResultTrait <- neo4r::call_neo4j(paste0("MATCH (n:EssentialSalmonVariable{esvCategory:'Salmon Trait'})-[:HAS_DOMAIN]-(m:Domain{domainTitle:'",domainName,"'}) RETURN n.esvTitle;"),neo_con, type = 'row')
      # UI code
      # Note dynamic input_ids created from domain name and 3 variable categories physical, biological and salmon trait
      # These dynamic id's are used later on in the server logic when the user presses submit and the selected
      # variable classes are passed back to the database. 
      box(
        width = 12,
        title = domainName,
        status = 'warning',
        column(
          width = 4,
          shinyWidgets::checkboxGroupButtons(paste0(domainName,"_physical"),"Physical",choices = sort(esvNodeListResultPhys$n.esvTitle$value),
                                             justified = F,
                                             individual = T,
                                             status = "default",
                                             size = 'xs',
                                             direction = 'vertical',
                                             checkIcon = checkboxGroupButtonsIcons)
        ),
        column(
          width = 4,
          shinyWidgets::checkboxGroupButtons(paste0(domainName,"_biological"),"Biological",choices = sort(esvNodeListResultBiol$n.esvTitle$value),
                                             justified = F,
                                             individual = T,
                                             status = "default",
                                             size = 'xs',
                                             direction = 'vertical',
                                             checkIcon = checkboxGroupButtonsIcons)
        ),
        column(
          width = 4,
          shinyWidgets::checkboxGroupButtons(paste0(domainName,"_salmontrait"),"Salmon Trait",choices = sort(esvNodeListResultTrait$n.esvTitle$value),
                                             justified = F,
                                             individual = T,
                                             status = "default",
                                             size = 'xs',
                                             direction = 'vertical',
                                             checkIcon = checkboxGroupButtonsIcons)
        )
      ) # close box
    }) # close lapply loop
    } # else clause end
})

##########################################################
# SubmitDataSource_server Life-Stage and Domains Selection
##########################################################


# observers for checkbox enable/disable inputs
observeEvent(input$embargoEndToggle,{
  if(input$embargoEndToggle){
    updateTextInput(session, 'embargoEnd', value = "")
    shinyjs::enable('embargoEnd')
  }else{
    updateTextInput(session, 'embargoEnd', value = "")
    shinyjs::disable('embargoEnd')
  }
})
observeEvent(input$sourceAvailableOnline,{
  if(input$sourceAvailableOnline){
    shinyjs::enable('sourceALTURI')
  }else{
    updateTextInput(session, 'sourceALTURI', value = "")
    shinyjs::disable('sourceALTURI')
  }
})
observeEvent(input$sourceMaintenanceToggle,{
  if(input$sourceMaintenanceToggle){
    updateSelectInput(session, inputId = 'sourceMaintenance', choices = c("continually",
                                                                          "daily",
                                                                          "weekly",
                                                                          "monthly",
                                                                          "biannually",
                                                                          "annually",
                                                                          "irregular",
                                                                          "unknown"),
                      selected = "unknown")
    shinyjs::enable('sourceMaintenance')
  }else{
    updateSelectInput(session, inputId = 'sourceMaintenance', choices = c("notPlanned"), selected = "notPlanned")
    shinyjs::disable('sourceMaintenance')
  }
})

# Temporal Detail Server
observeEvent(input$monthsOfYearToggleAll,{
  if(input$monthsOfYearToggleAll %% 2 == 0){
    shinyWidgets::updateCheckboxGroupButtons(session, 'monthsOfYear', selected = character(0))
    updateActionButton(session, 'monthsOfYearToggleAll', label = "Select All")
  }else{
    shinyWidgets::updateCheckboxGroupButtons(session, 'monthsOfYear', selected = month.abb)
    updateActionButton(session, 'monthsOfYearToggleAll', label = "Deselect All")
  }
})


# Geographic Detail Server
output$submitMap <- leaflet::renderLeaflet({
  leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 10)) %>% # maxZoom set so that user can always see their rectangle in context of a coastline
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>%
    leaflet::addRectangles(-15,61,-14,60,group = 'userRectangle') %>% 
    leaflet::fitBounds(-15,61,-14,60,options = leaflet::leafletOptions(maxZoom = 5))
})
# observer for 4 numeric inputs lat/lon - action update userRectangle on submitMap, change zoom and centre
observeEvent(input$submitEast | input$submitWest | input$submitNorth | input$submitSouth,{
  req(input$submitEast && input$submitWest && input$submitNorth && input$submitSouth) # catch empty value, prevent crash?
  leaflet::leafletProxy('submitMap') %>%
    leaflet::clearGroup(group = 'userRectangle') %>%
    leaflet::addRectangles(input$submitWest,input$submitNorth,input$submitEast,input$submitSouth,group = 'userRectangle') %>%
    leaflet::fitBounds(input$submitWest,input$submitNorth,input$submitEast,input$submitSouth)
})

# observe user click on map and set coordinates
observeEvent(input$submitMap_click,{
  updateNumericInput(session,inputId = 'submitNorth',value = round(input$submitMap_click$lat,digits = 4))
  updateNumericInput(session,inputId = 'submitEast',value = round(input$submitMap_click$lng,digits = 4))
  updateNumericInput(session,inputId = 'submitSouth',value = round(input$submitMap_click$lat,digits = 4))
  updateNumericInput(session,inputId = 'submitWest',value = round(input$submitMap_click$lng,digits = 4))
})

# Observer to pass file upload details to sessionFile reactive value
# This is passed to a reactiveVal so that it can be cleared out once user has submitted the form.
observeEvent(input$uploadDataFile,{
  sessionFile(input$uploadDataFile)
})

# Reactive process to tell shiny what to do with the users file. Activated by submitNewDataSource
# uses UUID generated during actionButton click to submit datasource
# if no file has been uploaded sessionFile reactiveVal will be NULL and so no upload is triggered
uploadReactive <- reactive({
  if(!is.null(sessionFile())){
    fileName <- paste0(sessionUUID(),"_",sessionFile()$name)
    aws.s3::put_object(file = sessionFile()$datapath,object = fileName,bucket = "likelysuspects-datastore/data")
  }
  
})

# Source Submit Button Actions
# TODO: Build a confirmation modal window - In progress
# TODO: Build a Submission Result modal window - In progress

# Create feedback information table - IN DEVELOPMENT
# reactive table creation Works but fails when all the below information is included
submitSourceConfirmDataTable <- reactive({
  data.frame(Title=input$sourceTitle,
             Creator=input$sourceCreator,
             Email=input$sourceCreatorEmail,
             Organisation=input$sourceOrganisation,
             Abstract=input$sourceAbstract,
             GeographicDescription=input$sourceGeographicDescription,
             # KNBURI=input$sourceURI,
             # metadataAltURI=input$sourceALTURI,
             # metadataCreatorORCID=input$sourceCreatorORCID,
             # metadataCoverageStartYear=input$sourceStartYear,
             # metadataCoverageEndYear=input$sourceEndYear,
             # metadataCoverageMonthsOfYear=paste(input$monthsOfYear,collapse = ","),#collapse months of year into csv string
             # metadataCoverageNorth=input$submitNorth,
             # metadataCoverageSouth=input$submitSouth,
             # metadataCoverageEast=input$submitEast,
             # metadataCoverageWest=input$submitWest,
             # metadataMaintenance=input$sourceMaintenance,
             # metadataQCCheck="FALSE",
             # metadataUUID=sessionUUID(),
             stringsAsFactors = FALSE)
})

submitSourceConfirmESVDomains <- reactive({
  # DEVELOPMENT - Build a table of Life-Stage Domains and Variable Classes chosen by user
  domainVector <- c(
    "River Rearing",
    "River Migration Smolt",
    "Estuary Migration Post-Smolt",
    "Coastal Migration Post-Smolt",
    "Ocean Migration",
    "Coastal Migration Adult",
    "Estuary Migration Adult",
    "River Migration Adult")
  selectedClasses <- c()
  for(dom in domainVector){
    categoryCollapse <- formatCheckboxGroupCategories(c(input[[paste0(dom,"_physical")]],
                                                        input[[paste0(dom,"_biological")]],
                                                        input[[paste0(dom,"_salmontrait")]]))
    selectedClasses <- append(selectedClasses,categoryCollapse)
  }
  tibble(Domains = domainVector,
      `Selected Variable Classes` = selectedClasses
      )
})

# render confirmation table as Table
output$submitSourceConfirmModalDataFrame <- shiny::renderTable({t(submitSourceConfirmDataTable())}, rownames = TRUE, colnames = FALSE)
output$submitSourceConfirmESVDomainsDataframe <- shiny::renderTable(submitSourceConfirmESVDomains()[submitSourceConfirmESVDomains()$Domains %in% input$domainNodeList,])
#output$submitSourceConfirmESVDomainsDataframe <- shiny::renderText(submitSourceConfirmESVDomains())
# Map to be rendered within confirmation modal
output$submitSourceConfirmMap <- leaflet::renderLeaflet({
  leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 5)) %>% # maxZoom set so that user can always see their rectangle in context of a coastline
    #leaflet::addPolygons(data = neContinentsSF, stroke = FALSE) %>%
    #leaflet::addProviderTiles("Stamen.TerrainBackground") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>%
    leaflet::addRectangles(input$submitWest,input$submitNorth,input$submitEast,input$submitSouth) %>%
    leaflet::fitBounds(input$submitWest,input$submitNorth,input$submitEast,input$submitSouth)
  })

# create confirm modal as function to be called during submit button push event
submitSourceConfirmModal <- function() {
  modalDialog(size = "l",title = "Please review the details and confirm to save, or cancel to go back and edit.",
              fluidRow(
                column(
                  width = 6,
                  shiny::tableOutput('submitSourceConfirmModalDataFrame'),
                  shiny::tableOutput('submitSourceConfirmESVDomainsDataframe')
                ),
                column(
                  width = 6,
                  leaflet::leafletOutput('submitSourceConfirmMap')
                )
              ),
              em("Please Note - Upon clicking submit there may be a delay depending on how large
                 a file you are uploading. It is recommended to wait for the 'Success' window to appear
                 before continuing. This should take no longer than 1 minute."),
              
              footer = tagList(
                modalButton("Cancel"),
                actionButton("confirmSubmitNewDataSource", "Confirm", class = "btn-success")
              )
  )
}

######################
# create results modal as function to be called at the end of submit button push event
# Submission Results following confirmation
userSubmitSourceFeedback <- reactiveVal(NULL)
#
#output$createNewSourceResultTable <- DT::renderDT(createNewSourceResult())
output$createNewSourceResultTable <- DT::renderDT(userSubmitSourceFeedback()[,c('Title','File','UUID')],
                                                  rownames = FALSE,
                                                  options = list(pageLength = 1,
                                                                 lengthChange = FALSE, # disable result length change
                                                                 searching = FALSE, # disable search field
                                                                 paging = FALSE, # disable paging menu
                                                                 info = FALSE,
                                                                 ordering = FALSE)
)

submitSourceResultModal <- function() {
  modalDialog(size = "l",title = "Thank you for your submission!",
              h4("The table below shows a summary of your upload."),
              em("This information will appear in your account profile under 'Submit History'"),
              DT::DTOutput('createNewSourceResultTable'),
              footer = tagList(
                modalButton("OK")
              )
  )
}
######################

######################
# The following observers are to change the background colour of required fields back to white once the user starts to fill them in
# The fields are initially white on load, but will turn red if the user tries to submit data with missing required fields
# see observer for submitNewDataSourceBody/submitNewDataSourceSidebar for lines that turn the background red
observeEvent(input$sourceTitle,{shinyjs::runjs('document.getElementById("sourceTitle").style.backgroundColor = "white";')})
observeEvent(input$sourceAbstract,{shinyjs::runjs('document.getElementById("sourceAbstract").style.backgroundColor = "white";')})
observeEvent(input$sourceCreator,{shinyjs::runjs('document.getElementById("sourceCreator").style.backgroundColor = "white";')})
observeEvent(input$sourceCreatorEmail,{shinyjs::runjs('document.getElementById("sourceCreatorEmail").style.backgroundColor = "white";')})
observeEvent(input$sourceOrganisation,{shinyjs::runjs('document.getElementById("sourceOrganisation").style.backgroundColor = "white";')})




observeEvent(input$submitNewDataSourceBody | input$submitNewDataSourceSidebar, {
  # note two submit buttons exist that invoke the routine here, the req line is needed so that it doesn't trigger when app loads initially
  req(input$submitNewDataSourceBody!=0 | input$submitNewDataSourceSidebar!=0)
  if(is.null(sessionUUID())){
    sessionUUID(uuid::UUIDgenerate())
  }
  # check if UUID already exists or not
  results <- neo4r::call_neo4j(paste("MATCH (n:Metadata{metadataUUID:'",sessionUUID(),"'}) RETURN n;",sep = ""),neo_con,type = 'row',include_stats = T,include_meta = T)
  if(is.null(results$stats)){ # case UUID does not already exist
    # confirm all required fields have information in them
    # If user has missed out any inputs that are classed as required, show them a modal window and colour the blank required inputs red until they input text
    if(input$sourceTitle == "" || input$sourceAbstract == "" || input$sourceCreator == "" || input$sourceCreatorEmail == "" || input$sourceOrganisation == ""){
      showModal(modalDialog(title = "Required Information Missing",p("Please ensure that none of the following fields are blank before submitting:"),
                            p("Title"),p("Abstract"),p("Primary Contact Name"),p("Primary Contact Email"),p("Organisation"),
                            easyClose = TRUE, footer = NULL))
      if(input$sourceTitle == ""){shinyjs::runjs('document.getElementById("sourceTitle").style.backgroundColor = "OrangeRed";')}
      if(input$sourceAbstract == ""){shinyjs::runjs('document.getElementById("sourceAbstract").style.backgroundColor = "OrangeRed";')}
      if(input$sourceCreator == ""){shinyjs::runjs('document.getElementById("sourceCreator").style.backgroundColor = "OrangeRed";')}
      if(input$sourceCreatorEmail == ""){shinyjs::runjs('document.getElementById("sourceCreatorEmail").style.backgroundColor = "OrangeRed";')}
      if(input$sourceOrganisation == ""){shinyjs::runjs('document.getElementById("sourceOrganisation").style.backgroundColor = "OrangeRed";')}
    }else{
      # If the DOI/URN entered does not exist in the framework already, show confirmation modal
      showModal(submitSourceConfirmModal())
    }
  }else{ # otherwise show some detail for the user
    showModal(modalDialog(
      title = "This DOI/URN Already Exists",
      sessionUUID(),
      p("This DOI/URN matches an object that already exists in the database."),
      p("Data returned from the database:"),
      results$n$metadataTitle,
      results$n$metadataAbstract,
      easyClose = TRUE,
      footer = NULL
    ))
  }

})

# TODO Some duplication occurs between these two (above and below) observeEvents that could be trimmed down
observeEvent(input$confirmSubmitNewDataSource, {
  # disable modal
  # this avoids multiple 'Confirm' presses if file upload is large
  shinyjs::disable(id = 'shiny-modal')
  # Generate a unique Identifier for this data source UNLESS it already exists (expected condition if user has loaded KNB metadata)
  # TODO: Confirm that this logic works, is there a condition where sessionUUID is not NULL but also not loaded from KNB?
  if(is.null(sessionUUID())){
    sessionUUID(uuid::UUIDgenerate())
  }
  # Capture filename or NULL
  if(!is.null(sessionFile())){
    filename <- sessionFile()$name
  }else{
    filename <- "NA"
  }
  
  #Create template results tibble for reporting (creates a standard results tibble with 0 values)
  # also doubles as test for existing metadata with same UUID
  #TODO: this is duplicate of test above.. just sayin'
  #TODO: this is too simple a test meaning that duplicates are quite likely to occur
  results <- neo4r::call_neo4j(paste("MATCH (n:Metadata{metadataUUID:'",sessionUUID(),"'}) RETURN n;",sep = ""),neo_con,type = 'row',include_stats = T,include_meta = T)
  if(is.null(results$stats)){
    # Create metadata node based on user input
    # Quick center point calcs
    lngCenter <- (input$submitWest+input$submitEast)/2
    latCenter <- (input$submitSouth+input$submitNorth)/2
    

    # Create query that creates a metadata node with all the properties supplied in the form
    # Also creates a relationship to the logged on user
    # NOTE: Creates as many new metadata nodes as there are users with matching email!
    # Ensure Person matching is unique to the logged on user
    # user_info() reactive value is created from the function checkUserCredentials which already verifies the user and contains unique id
    # WHERE id(p) = ",user_info()$user_info$id,"
    metadataNodeCreateQuery <- paste("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id,
                                     " CREATE (p)-[:HAS_SUBMITTED{created:'",Sys.time(),
                                     "',lastModified:'",Sys.time(),
                                     "',status:'pendingQC'}]->(:Metadata{metadataTitle:'",sanitiseFreeTextInputs(input$sourceTitle),
                                     "',metadataCreator:'",sanitiseFreeTextInputs(input$sourceCreator),
                                     "',metadataKNBURI:'",input$sourceURI,
                                     "',metadataAltURI:'",sanitiseFreeTextInputs(input$sourceALTURI),
                                     "',metadataOrganisation:'",sanitiseFreeTextInputs(input$sourceOrganisation),
                                     "',metadataAbstract:'",sanitiseFreeTextInputs(input$sourceAbstract),
                                     "',metadataAvailableOnline:",input$sourceAvailableOnline,
                                     ",metadataEmbargoed:",input$embargoEndToggle,
                                     ",metadataEmbargoEnd:'",sanitiseFreeTextInputs(input$embargoEnd),
                                     "',metadataGeographicDescription:'",sanitiseFreeTextInputs(input$sourceGeographicDescription),
                                     "',metadataCreatorEmail:'",sanitiseFreeTextInputs(input$sourceCreatorEmail),
                                     "',metadataCreatorORCID:'",sanitiseFreeTextInputs(input$sourceCreatorORCID),
                                     "',metadataCoverageStartYear:",sanitiseFreeTextInputs(input$sourceStartYear),
                                     ",metadataCoverageEndYear:",sanitiseFreeTextInputs(input$sourceEndYear),
                                     ",metadataCoverageMonthsOfYear:'",paste(input$monthsOfYear,collapse = ","),#collapse months of year into csv string
                                     "',metadataCoverageNorth:",sanitiseFreeTextInputs(input$submitNorth),
                                     ",metadataCoverageSouth:",sanitiseFreeTextInputs(input$submitSouth),
                                     ",metadataCoverageEast:",sanitiseFreeTextInputs(input$submitEast),
                                     ",metadataCoverageWest:",sanitiseFreeTextInputs(input$submitWest),
                                     ",metadataMaintenance:'",input$sourceMaintenance,
                                     "',metadataQCCheck:",FALSE,
                                     ",metadataCoverageCentroid:'",paste0("POINT (",lngCenter," ",latCenter,")"),
                                     "',metadataCoverageIntersectICESEcoRegion:'","",
                                     "',metadataCoverageIntersectNAFODivision:'","",
                                     "',metadataCoverageIntersectMigrationRoutes:'","",
                                     "',metadataUUID:'",sessionUUID(),
                                     "',metadataFilename:'",filename,
                                     "',metadataTimestamp:'",Sys.time(),
                                     "'});",sep = "")
    
    resultCreateMetadataNode <- neo4r::call_neo4j(metadataNodeCreateQuery,neo_con,type = 'row',include_stats = T,include_meta = T)
    #results$value <- results$value + resultCreateMetadataNode$value
    

    
    #####################
    # Create relationships to ESV/Variable Class using DOMAIN SPECIFIC RELATIONSHIPS
    #####################
    # initiate a vector to capture the queries
    queryMasterList <- c()
    
    # create base query elements
    queryBase <- c("MATCH (esv:EssentialSalmonVariable{esvTitle:'","'}),(md:Metadata{metadataUUID:'","'}) CREATE (esv)<-[:HAS_ESV{domain:'","'}]-(md);")
    # cycle through all the selected ESV for each domain IS THIS EFFICIENT?
    for(domain in input$domainNodeList){
      # create basic relationship between metadata and domain (capture even if user selects no ESV's)
      addDomainQuery <- paste0("MATCH (d:Domain{domainTitle:'",domain,"'}),(md:Metadata{metadataUUID:'",sessionUUID(),"'}) CREATE (md)-[:HAS_DOMAIN]->(d);")
      queryMasterList <- append(queryMasterList,addDomainQuery)
      
      # build full ESV list for domain (collapse from 3 categories presented in the UI)
      variableClassesForDomain <- c(input[[paste0(domain,"_biological")]],input[[paste0(domain,"_physical")]],input[[paste0(domain,"_salmontrait")]])
      for(esv in variableClassesForDomain){
        # create sub query for single esv-metadata
        querySubMaster <- paste(queryBase[1],esv,queryBase[2],sessionUUID(),queryBase[3],domain,queryBase[4],sep = "")
        # add sub query to master query list
        queryMasterList <- append(queryMasterList,querySubMaster)
      }
    }
    
    #####################^^^^^^^^
    # Create relationships using DOMAIN SPECIFIC RELATIONSHIPS
    #####################
    
    for(query in queryMasterList){
      resultCreateESVRelationships <- neo4r::call_neo4j(query,neo_con,type = 'row',include_stats = T,include_meta = T)
      resultCreateMetadataNode$value <- resultCreateMetadataNode$value + resultCreateESVRelationships$value
    }
    
    
    
  }else{
    # Else no action taken, empty results tibble required
    # create a dummy results with 0 values for display only
    resultCreateMetadataNode <- neo4r::call_neo4j("MATCH (n:Metadata{metadataUUID:'DummyDummy'}) RETURN n;",neo_con,type = 'row',include_stats = T,include_meta = T)
    
  }
  
  # Trigger File Upload
  uploadReactive()
  
  # Create log for submissions
  resultCreateMetadataNode # table of graph database updates
  # requires transposed to fit log file
  resultCreateMetadataNodeTransform <- data.frame(t(resultCreateMetadataNode),row.names = NULL)[2,]
  names(resultCreateMetadataNodeTransform) <- resultCreateMetadataNode$type
  # reduce result columns to required information
  resultCreateMetadataNodeTransform <- resultCreateMetadataNodeTransform[,neo4rResultFields]
  
  # create user feedback table
  userSubmitSourceFeedback(data.frame(date = Sys.time(),user = user_info()$user_info$email, Title = input$sourceTitle,File = filename,UUID = sessionUUID()))

  logFileDataFrame <- dplyr::bind_cols(userSubmitSourceFeedback(),resultCreateMetadataNodeTransform)
  
  # send log to AWS, append to existing file
  # get current logfile
  # TODO - THis process can cause a collision if two people submit at the exact same time, add some collision avoidance
  logfile <- readr::read_csv(aws.s3::get_object(object = "userSubmit.log",bucket = "likelysuspects-datastore/logs"),col_types = 'Tcccccccccccc')
  # append new log item
  logfile <- dplyr::bind_rows(logfile,logFileDataFrame)
  # create temp area in memory to write to
  rc <- rawConnection(raw(0), 'r+')
  # write csv to temp area
  readr::write_csv(logfile,rc)
  # send csv object from temp area to S3
  aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/logs",object = "userSubmit.log")
  # close and remove temp area
  close(rc)

  
  ##############
  # Clear INPUTS
  ##############
  resetAll()
  
  #Close confirm modal
  removeModal()
  
  showModal(submitSourceResultModal())
  
})




################
# DEBUGGING VIEW
################
output$generalTesting <- reactive({
  paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';")
  #paste0("MATCH (p:Person{personEmail:'",user_info()$user_info$email,"'}),(m:Metadata) WHERE id(m) IN [",formatNumericList(sessionUserBookmarks()),"] CREATE (p)-[:HAS_REQUESTED{created:'",Sys.time(),"',lastModified:'",Sys.time(),"',status:'pendingReview'}]->(m);")
})
### CREATE Queries to insert metadata nodes with relevant relationships
### At the moment just outputting as text to view/verify the resulting queries
output$CreateMetadataNodeQueryBuilder <- renderText({
  # define a list to capture the queries
  queryMasterList <- c()
  # define a base query, common elements that all the queries will share
  
  #####################
  # Create relationships using DOMAIN SPECIFIC RELATIONSHIPS
  #####################
  # create base query elements
  queryBase <- c("MATCH (esv:EssentialSalmonVariable{esvTitle:'","'}),(md:Metadata{metadataUUID:'","'}) CREATE (esv)<-[:HAS_ESV{domain:'","'}]-(md);")
  # cycle through all the selected ESV for each domain IS THIS EFFICIENT?
  
  for(domain in input$domainNodeList){
    variableClassesForDomain <- c(input[[paste0(domain,"_biological")]],input[[paste0(domain,"_physical")]],input[[paste0(domain,"_salmontrait")]])
    for(esv in variableClassesForDomain){
      # create sub query for single esv-metadata
      querySubMaster <- paste(queryBase[1],esv,queryBase[2],sessionUUID(),queryBase[3],domain,queryBase[4],sep = "")
      # add sub query to master query list
      queryMasterList <- append(queryMasterList,querySubMaster)
    }
  }
  
  #####################^^^^^^^^
  # Create relationships using DOMAIN SPECIFIC RELATIONSHIPS
  #####################
  
  # collapse queries into a string to view
  paste(queryMasterList,collapse = "")
})



output$showCurrentSelection <- DT::renderDT(tibble(inputID = c(#"icesStockUnit:",
  #"icesStatAreas:",
  #"river:",
  "domainNodeList:",
  "River Rearing:",
  "River Migration Smolt:",
  "River Migration Adult:",
  "Estuary Migration Post-Smolt:",
  "Coastal Migration Post-Smolt:",
  "Ocean Migration:",
  "Coastal Migration Adult:",
  "Estuary Migration Adult:"),
  inputValue = c(#formatCheckboxGroupCategories(input$icesStockUnit),
    #formatCheckboxGroupCategories(input$icesStatAreas),
    #input$river,
    formatCheckboxGroupCategories(input$domainNodeList),
    formatCheckboxGroupCategories(input$`River Rearing`),
    formatCheckboxGroupCategories(input$`River Migration Smolt`),
    formatCheckboxGroupCategories(input$`River Migration Adult`),
    formatCheckboxGroupCategories(input$`Estuary Migration Post-Smolt`),
    formatCheckboxGroupCategories(input$`Coastal Migration Post-Smolt`),
    formatCheckboxGroupCategories(input$`Ocean Migration`),
    formatCheckboxGroupCategories(input$`Coastal Migration Adult`),
    formatCheckboxGroupCategories(input$`Estuary Migration Adult`))))

output$filesPendingUpload <- DT::renderDT(sessionFile())

output$sessionUUIDPrint <- renderText(sessionUUID())
############################
# submitDataSource_server.R END
############################