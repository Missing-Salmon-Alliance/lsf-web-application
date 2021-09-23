############################
# DataSearch_server.R START
############################


############
# SIDEBAR Conditional UI
output$searchRefreshUI <- renderUI(actionButton('searchRefresh',"Refresh"))
output$searchFilterResetUI <- renderUI(actionButton('searchFilterReset',"Reset Filter"))
###########

# Define reactive value for reactive filtering

metadataFilterReactive <- reactiveVal()
metadataFilterReactive(LSFMetadataTibble)

observeEvent(input$sendRequest,{
  shinyBS::addPopover(session, id = 'sendRequest',title = "Send Data Request",
                      content = "This feature is still under development.",
                      placement = 'top',
                      options = list(container = "body")
                      
  )
  shinyjs::disable(id = 'sendRequest') # disable user Send Data Request for now
},ignoreNULL = FALSE, ignoreInit = FALSE)


# bookmarks modal
# TODO: Move all modals defined in UI files to observers in SERVER files
# TODO: Alternatively all modals could reside in a single file such as searchDataSource_modals_server.R for easy access
observeEvent(input$bookmarks,{
  showModal(
    modalDialog(title = "Your Bookmarks", size = "l",
                column(width = 12,
                    DT::DTOutput('bookmarkContentsTable'),
                    column(width = 6,actionButton('clearBookmarks', "Clear All Bookmarked Sources")),
                    column(width = 5,actionButton('clearRows', "Delete Selected Row")),
                    hr()
                ),
                column(
                  width = 12,
                    h4("Request Bookmarked Data"),
                    textInput('requestName', "Name:", value = user_info()$user_info$fullname),
                    textInput('requestOrganisation', "Organisation:", value = user_info()$user_info$affiliation),
                    #selectInput("requestPosition", "Position/Occupation:", choices = c("Researcher","Database Manager","Government Official", "Conservationist", "Student", "Lecturer", "Other")),
                    #selectInput("requestDataUse", "What will the data be used for?", choices = c("Independent Research", "Conservation", "Guidance to Managers", "Other")),
                    #textInput("requestOther","Please describe what is meant, if you selected other"),
                    #selectInput("requestProvision", "Do you intend to provide data to the Central Data Resource?", choices = c("Yes", "No", "In the Future")),
                    textAreaInput('requestIntention', "Please describe the intended use for the data. Please include information on the project, time scale of usage and expected number of users.", width = "1000px", height = "50px"),
                    actionButton('sendRequest', "Send Data Request")

                )
                
    )
  )
})

observeEvent(input$sendRequest,{
  
  # save csv file directly to AWS S3 storage
  # create temp area in memory to write to
  rc <- rawConnection(raw(0), 'r+')
  # write csv to temp area
  write_file(paste(paste(unique(sessionUserBookmarks()),collapse = ','),input$requestIntention,sep = ','),rc)
  # send csv object from temp area to S3
  aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/userRequests",object = paste0("user_",user_info()$user_info$id,"_",as.character(Sys.time()),".txt"))
  # close and remove temp area
  close(rc)
  # create requested source relationships in graph
  neo4r::call_neo4j(paste0("MATCH (p:Person{personEmail:'",user_info()$user_info$email,"'}),(m:Metadata) WHERE id(m) IN [",formatNumericList(sessionUserBookmarks()),"] CREATE (p)-[:HAS_REQUESTED{created:'",Sys.time(),"',lastModified:'",Sys.time(),"',status:'pendingReview'}]->(m)"),con = neo_con,type = 'row')
  # clear bookmarks
  sessionUserBookmarks(c())
  shiny::updateTextAreaInput(session, inputId = 'requestIntention', value = "")
  showModal(modalDialog(title = "Request Received",
                        p("Thank you for submitting a request for data!"),
                        p("The Data Manager will process your request soon.")))
})

#refresh button action
observeEvent(input$searchRefresh,{
  metadataESV <- neo4r::call_neo4j("MATCH (:Metadata)-[r:HAS_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')
  #esvDomain <- neo4r::call_neo4j("MATCH (:EssentialSalmonVariable)-[r:HAS_DOMAIN]->(:Domain) RETURN r;",neo_con,type='graph')
  
  metadataESV$nodes <- metadataESV$nodes %>% neo4r::unnest_nodes('all')
  #metadataESV$relationships <- metadataESV$relationships %>% neo4r::unnest_relationships()
  #esvDomain$nodes <- esvDomain$nodes %>% neo4r::unnest_nodes('all')
  #esvDomain$relationships <- esvDomain$relationships %>% neo4r::unnest_relationships()
  
  LSFMetadataTibble <- metadataESV$nodes[metadataESV$nodes$value == "Metadata",] %>% select(matches("^(id|metadata*)"))
  
  #metadataESVDomainRelationships <- bind_rows(metadataESV$relationships,esvDomain$relationships)
  
  # Conversion to GIS enabled dataframe using pre-calculated centroids (see nightly intersection routine)
  LSFMetadataTibble <- sf::st_as_sf(LSFMetadataTibble, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE)
  
  metadataFilterReactive(LSFMetadataTibble)
  # clear existing markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  # and redraw
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

observeEvent(input$searchFilterReset,{
  metadataFilterReactive(LSFMetadataTibble)
  activeGeographicFilterReactive("No Filter Selected")
  # clear existing markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  # and redraw
  redrawFilteredMarkers(metadataFilterReactive(),session)
})



# TODO: Create some kind of summary of the applied filters
# e.g. "The data is currently filtered by Domain:River Rearing and ESV:Suspended Solids (freshwater)"
filterAppliedInformation <- reactiveValues()
filterAppliedInformation$filterType <- "None"
filterAppliedInformation$filterName <- "None"

#####################
# Load map layers from SQL and CSV
source('./src/server/searchDataSource_MapLayerSource_server.R',local = TRUE)

###################################################
# Write a HTML Legend (As have used HTML ICONS and no gradient)

html_legend <- "<img src='https://img.icons8.com/ios-filled/50/4a90e2/marker.png'style = 'width:30px;height:30px;'>Data Sources</img><br>
<img src='https://img.icons8.com/cotton/64/000000/salmon--v1.png'style = 'width:30px;height:30px;'>Index Rivers</img><br>
<img src='https://img.icons8.com/emoji/48/000000/black-circle-emoji.png'style = 'width:15px;height:15px;'>NASCO Rivers</img>"
# <br>
# <h4>Polygon Overlays</h4> 
# <img src='https://img.icons8.com/emoji/48/26e07f/green-circle-emoji.png'style = 'width:30px;height:30px;'/> <b>ICES Ecoregions<br>
# <img src='https://img.icons8.com/emoji/48/26e07f/purple-circle-emoji.png'style = 'width:30px;height:30px;'/> <b>NAFO Divisions<br>
# <img src='https://img.icons8.com/emoji/48/4a90e2/blue-square-emoji.png'style = 'width:30px;height:30px;'/> <b>Areas of Migration<br/>
# "

# TODO: improve visual information in markers, colour index rivers or use river icon, check out the IYS icons
output$map <- leaflet::renderLeaflet({ 
  leaflet::leaflet (options = leaflet::leafletOptions(minZoom = 3,maxZoom = 10))%>%
    leaflet::setView(lng = 10,lat = 60,zoom = 3) %>% 
    leaflet::setMaxBounds( lng1 = -130
                  , lat1 = -20
                  , lng2 = 210
                  , lat2 = 90 ) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>%
    # NOTE, CHANGES HERE SHOULD ALSO BE MADE TO THE FUNCTION DEFINED FOR THE leafletProxy UPDATES 'redrawFilteredMarkers' in global.R
    leaflet::addMarkers(data = LSFMetadataTibble,
               label = ~metadataTitle,
               layerId = ~id,
               group = 'Data Source',
               popup = ~paste("<h3>More Information</h3>",
                              "<b>Title:</b>",stringr::str_trunc(metadataTitle,width = 90,side = 'right',ellipsis = '...'),"<br>","<br>",
                              "<b>Abstract:</b>",stringr::str_trunc(metadataAbstract,width = 200,side = 'right',ellipsis = '...'),"<br>","<br>",
                              "<b>Organisation:</b>",metadataOrganisation,"<br>","<br>",
                              "<b>URL (if available):</b>",metadataAltURI,"<br>","<br>",
                              "&nbsp;",actionButton("showmodal", "View more...", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
                              sep =" "),
               # enable clustering for spiderfy, set freezeAtZoom so that full clustering does not occur
               clusterOptions = leaflet::markerClusterOptions(
                 showCoverageOnHover = FALSE,
                 zoomToBoundsOnClick = FALSE,
                 spiderfyOnMaxZoom = TRUE,
                 removeOutsideVisibleBounds = TRUE,
                 spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
                 freezeAtZoom = 10)) %>%
    # 
    # On map search box
    # leaflet.extras::addSearchFeatures(
    #   targetGroups = 'Data Source',
    #   options = leaflet.extras::searchFeaturesOptions(zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
    #     autoCollapse = TRUE, hideMarkerOnCollapse = TRUE
    #   )
    # ) %>%
    
    leaflet::addMarkers(data = indexRiversSF,
                        label = ~paste("Salmon Index River: ",rivername),
                        group = "ICES Index Rivers",
                        icon = list(
                          iconUrl = "https://img.icons8.com/cotton/64/000000/salmon--v1.png",
                          iconSize = c(35, 35))) %>%
    # 
    leaflet::addCircleMarkers(data = nascoRiversDBSF,
                              label = ~rivername,
                              group = "NASCO Rivers DB",
                              color = "black",
                              radius = 3,
                              stroke = FALSE,
                              fillOpacity = 1) %>%
    
    # Demonstration - Add WMS Tiles
    # leaflet::addWMSTiles(
    #   "https://gis.ices.dk/gis/services/Mapping_layers/ICES_Statrec_mapto_Ecoregions/MapServer/WMSServer",
    #   layers = "0",
    #   group = "Ecoregions",
    #   options = WMSTileOptions(format = "image/png", transparent = T)
    # ) %>%
    
    leaflet::addPolygons(data = ICES_Ecoregions,
                label = ~ecoregion,
                layerId = paste0("eco_",ICES_Ecoregions$objectid),
                color = "green", group = "ICES Ecoregions", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE))  %>%
    # 
    # leaflet::addPolygons(data = icesStatEcoSF,
    #             label = ~name,
    #             layerId = paste0("sta_",icesStatEcoSF$id),
    #             color = "blue", group = "ICES Stat Squares", weight = 1,
    #             highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
    #                                                 bringToFront = TRUE))  %>%
    #  
    leaflet::addPolygons(data = nafoDivisionsSF,
                label = ~zone,
                layerId = paste0("div_",nafoDivisionsSF$ogc_fid),
                color = "purple", group = "NAFO Divisions", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE)) %>%
    leaflet::addPolygons(data = migrationSF,
                label = ~icesname,
                layerId = paste0("mig_",migrationSF$fid),
                color = "blue", group = "Proposed Outward Migration", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE)) %>%
    
    # leaflet::addPolygons(data = feedingSF,
    #             label = feedingSF$name,
    #             color = "black", group = "Migration Routes", weight = 1,
    #             highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
    #                                                 bringToFront = TRUE)) %>%
    
    leaflet::addLayersControl(position = 'topleft',overlayGroups = c("Data Source",
                                                                     "ICES Index Rivers",
                                                                     "ICES Ecoregions",
                                                                     "NAFO Divisions",
                                                                     #"ICES Stat Squares",
                                                                     "Proposed Outward Migration",
                                                                     "NASCO Rivers DB"),
                     options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("ICES Ecoregions",
                         "NAFO Divisions",
                         #"ICES Stat Squares",
                         "Proposed Outward Migration",
                         "NASCO Rivers DB")) %>%

    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left; font-size:16px;\">Layer Control</label>');
        }
    ")
    # commenting out legend for now, the layer control in a way works as a legend and the screen was a bit cluttered with both
    #leaflet::addControl(position = "bottomright", html = html_legend)
})


output$table <- DT::renderDT({
  sf::st_set_geometry(metadataFilterReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)

  },
  selection = 'single',
  rownames = FALSE,
  editable = FALSE,
  colnames = c('Title','Abstract','Keywords'),
  options = list(pageLength = 20,
                 columnDefs = list(list(visible=FALSE, targets=c(2)))
                 )
  
)


###########################################
# Map metadata node marker filter OBSERVERS
###########################################

###############################################
# Geograpic Filters
source("./src/server/searchDataSource_GeographicFilters_server.R",local = TRUE)$value

###############################################
# Temporal Filters
source("./src/server/searchDataSource_temporalFilters_server.R",local = TRUE)$value


# Observer for Search Map Click - Action: When user clicks a marker add the extents of the marker data source as rectangle to the map
observeEvent(input$map_marker_click,{
    leaflet::leafletProxy("map") %>%
    leaflet::clearGroup(group = 'markerRectangle') %>%
    leaflet::addRectangles(LSFMetadataTibble[LSFMetadataTibble$id == input$map_marker_click[1],]$metadataCoverageWest,
                  LSFMetadataTibble[LSFMetadataTibble$id == input$map_marker_click[1],]$metadataCoverageNorth,
                  LSFMetadataTibble[LSFMetadataTibble$id == input$map_marker_click[1],]$metadataCoverageEast,
                  LSFMetadataTibble[LSFMetadataTibble$id == input$map_marker_click[1],]$metadataCoverageSouth
                  ,group = 'markerRectangle', color = "blue", weight = 1, stroke = TRUE)
  }
)

# Observer for Search Map Click - Action: clear rectangle on background click
observeEvent(input$map_click,{
  leaflet::leafletProxy("map") %>%
    leaflet::clearGroup(group = 'markerRectangle')
})
############################################## 
# Observer for Search Map Click - Action: Modal pop-up on each marker_click

observeEvent(input$button_click, {
  click = input$map_marker_click
  showModal(modalDialog(
    title = "Information on Selected Data Source",
    h4("More Information"),
    em("Title:   "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataTitle),
    br(),br(),
    em("Abstract:   "),paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataAbstract),
    br(), br(),
    em("Organisation:   "),paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataOrganisation),
    br(),br(),
    em("URL (if available):   "), tags$a(href = LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataAltURI,LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataAltURI, target = '_blank'),
    br(),br(),
    em("Update Frequency:   "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataMaintenance),
    br(),br(),
    h4("Spatial Information"),
    em("North:"), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageNorth),br(),
    em("East:"),paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageEast),br(),
    em("South:"), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageSouth),br(),
    em("West:"), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageWest),br(),
    br(),br(),
    em("Geographical Description:"), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataGeographicDescription),
    br(),br(),
    h4("Temporal Information"),
    em("Start Year: "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageStartYear), br(), em("End Year: ", paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageEndYear)),
    br(),br(),
    # dynamic content based on user activity and history
    uiOutput('addToBookmarksUI'),
    easyClose = TRUE
  )
  
  )
})

##############################################
# Bookmark System

output$addToBookmarksUI <- renderUI({
  click = input$map_marker_click
  if(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id %in% sessionUserBookmarks()){
    h4("This resource is in your bookmarks.")
  }else if(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id %in% neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," RETURN id(m) as id;"),con = neo_con, type = 'row')$id$value){
    box(
      headerBorder = F,
      status = 'warning',
      title = "You have requested this resource already.",
      p("Request Date:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id," RETURN r.created as date;"),con = neo_con, type = 'row')$date$value),
      p("Request Status:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id," RETURN r.status as status;"),con = neo_con, type = 'row')$status$value)
    )
  }else{
    actionButton('Request', "Add to Bookmarks")
  }
})

# Bookmark Creation - as a reactive Value
sessionUserBookmarks <- reactiveVal(c())

# Action button uses information from the map_marker_click to update the bookmarks list 
observeEvent(input$Request, {
  # when user adds to bookmark disable request button and change label to indicate action completed
  shinyjs::disable(id = 'Request')
  updateActionButton(session, inputId = 'Request',label = "Added to bookmarks!")
  click = input$map_marker_click
  sourceIDString <- paste0(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id)
  sessionUserBookmarks(append(sessionUserBookmarks(),sourceIDString))
  # update database bookmark list
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
  })

# # A system for Creator Feedback to see the bookmarks - NOT REQUIRED
# output$bookmarkText <- renderText(paste0(unique(sessionUserBookmarks()), collapse = ","))
# 
# # An output which renders both the Title and ID. - NOT REQUIRED
# output$bookmarkContents <- renderText(paste0(unique(LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBookmarks(),]$metadataTitle), 
#                                            "ID:", 
#                                            unique(LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBookmarks(),]$id, collapse = ", "))
#                                     )


# Creation of a DT which shows the contents of the Bookmarks
output$bookmarkContentsTable <- DT::renderDT({
  LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBookmarks(),c("id","metadataTitle","metadataCoverageCentroid")]
},
selection = 'single',
rownames = FALSE,
editable = FALSE,
colnames = c("ID","Title","Centroid"),
options = list(pageLength = 7,
               searching = F,
               lengthChange = F,
               info = FALSE,
               columnDefs = list(list(visible=FALSE, targets=c(2)))
)

)

# Remove a Single Bookmark Item using Tables_Rows_selected

observeEvent(input$clearRows,{
  
  if (!is.null(input$bookmarkContentsTable_rows_selected)) {
    rowToBeRemoved <- input$bookmarkContentsTable_rows_selected
    idToBeRemoved <- LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBookmarks(),]$id[rowToBeRemoved]
    sessionUserBookmarks(sessionUserBookmarks()[sessionUserBookmarks() != idToBeRemoved])
    # update database bookmarks
    neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
  }
})


# Clear the Bookmark using Action Button
observeEvent(input$clearBookmarks, {
  sessionUserBookmarks(c())
  # update database bookmarks
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
})

# Bookmark Count 

# Selected Bookmark Count - NOT REQUIRED with a single selection function in DT
# output$bookmarkCountSelect <- renderText(length(input$bookmarkContentsTable_rows_selected))

# Render UI which includes both an action link, and a count of the Bookmark Length. 

output$bookmarkUI <- renderUI({
  req(user_info())
  if (user_info()$result) {
    dynamicLabel <- paste("Bookmarks:",length(unique(sessionUserBookmarks())))
    actionLink(inputId = "bookmarks", label = dynamicLabel ,icon = icon("bookmark"),style='padding:5px; font-size:120%; color:white;float:right;')
  }
})



###############################################
# Information boxes

output$No_Data <- renderInfoBox({
  infoBox(title = "Available Data",
          icon = icon("database"),
          subtitle = "datasets are recognised as available by the LSF",
          value = nrow(LSFMetadataTibble),
          fill = T, color = "blue")
})
#   
# # output$No_River <- renderInfoBox({
# #   infoBox(title = "River Data",
# #           subtitle = "rivers across the North Atlantic have data assosciated with them",
# #           value = length(unique(LSFMetadataTibble$Index_River)))
# # })

output$NAFO_Div <- renderInfoBox({
  infoBox(title = "NAFO Division Data",
          subtitle = "NAFO Divisions across the North Atlantic are represented by data sources registered in the Central Data Resource",
          value = length(uniqueNAFODivisions),
          fill = T, color = "purple")
})

output$No_Eco <- renderInfoBox({
  infoBox(title = "ICES Eco Regions",
          icon = icon("globe-europe"),
          subtitle = "ICES ecoregions across the North Atlantic are represented by data sources registered in the Central Data Resource",
          value = length(uniqueICESEcoRegions),
          fill = T, color = "green")
})


# Debugging Information
output$clickMarkerOutput <- renderText({paste0("Marker: ",input$map_marker_click,collapse = ",")})
output$clickOutput <- renderText({paste0("Click: ",input$map_click,collapse = ",")})
output$clickShapeOutput <- renderText({paste0("Shape: ",input$map_shape_click,collapse = ",")})
output$clickBoundsOutput <- renderText({paste0("Bounds: ",input$map_bounds,collapse = ",")})
############################
# DataSearch_server.R END
############################