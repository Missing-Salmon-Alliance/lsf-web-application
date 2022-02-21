############################
# DataSearch_server.R START
############################

############
# SIDEBAR Conditional UI
output$searchRefreshUI <- renderUI(actionButton('searchRefresh',"Refresh"))
output$searchFilterResetUI <- renderUI(actionButton('searchFilterReset',"Reset Filter"))
output$onlineOnlyFilterUI <- renderUI(checkboxInput('onlineOnlyFilter',"Show Online Only Resources",value = F))
###########
# MAP Tab Conditional UI
output$searchMapTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    #div(style="float:left",actionLink(inputId = "searchDescript", label = "Help", icon = icon("question-circle"))),
    #br(),
    # hiding until temporal filter works better
    # absolutePanel(id = 'searchFiltersAbsPanel',
    #               top = "110px",
    #               right = "10px",
    #               width = "35%",
    #               style="z-index:1000;",
    #               shinyBS::bsCollapse(id = "mapSearchFilters", open = NULL,
    #                                   # new location to be determined for geographic filters
    #                          #source("./src/ui/searchDataSource_geographicFilters_ui.R",local = TRUE)$value,
    #                          source("./src/ui/searchDataSource_temporalFilters_ui.R",local = TRUE)$value
    #               )
    # ),
    fluidRow(
      shinydashboard::box(
        width = 8,
        status = 'primary',
        solidHeader = FALSE,
        leaflet::leafletOutput('searchTabMap', height = "80vh"),
        conditionalPanel(
          condition = "input.debug",
          textOutput('clickOutput'),
          textOutput('clickMarkerOutput'),
          textOutput('clickShapeOutput'),
          textOutput('clickBoundsOutput'),
          textOutput('clickTableRow'),
          textOutput('intersectVectorOut')
        )
      ),
      column(
        width = 4,
        DT::DTOutput('searchTabTable')
      )
    )
  }else{
    fluidPage(
      h1("Map Explore Area"),
      h3("Please authenticate to access this area")
    )
  }
})

# observe the population of lsfMetadata reactive value and pass to initialise metadataFilterReactive
# metadataFilterReactive is used as a subset of lsfMetadata on the map search tab
observeEvent(lsfMetadata(),{
  req(lsfMetadata())
  metadataFilterReactive(lsfMetadata())
})


# UNDER DEVELOPMENT - Add popover and disable submit button for the sendRequest routine
# NOTE, when no longer under dev, remember to uncomment the sendRequest observer!
observeEvent(input$bookmarks,{
  shinyBS::addPopover(session, id = 'sendRequest',title = "Send Data Request",
                      content = "This feature is under development.",
                      placement = 'top',
                      options = list(container = "body")
                      
  )
  shinyjs::disable(id = 'sendRequest') # disable user Send Data Request for now
},ignoreNULL = FALSE, ignoreInit = FALSE)

# observeEvent(input$sendRequest,{
#   
#   # save csv file directly to AWS S3 storage
#   # create temp area in memory to write to
#   rc <- rawConnection(raw(0), 'r+')
#   # write csv to temp area
#   write_file(paste(paste(unique(sessionUserBookmarks()),collapse = ','),input$requestIntention,sep = ','),rc)
#   # send csv object from temp area to S3
#   aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/userRequests",object = paste0("user_",user_info()$user_info$id,"_",as.character(Sys.time()),".txt"))
#   # close and remove temp area
#   close(rc)
#   # create requested source relationships in graph
#   neo4r::call_neo4j(paste0("MATCH (p:Person{personEmail:'",user_info()$user_info$email,"'}),(m:Metadata) WHERE id(m) IN [",formatNumericList(sessionUserBookmarks()),"] CREATE (p)-[:HAS_REQUESTED{created:'",Sys.time(),"',lastModified:'",Sys.time(),"',status:'pendingReview'}]->(m)"),con = neo_con,type = 'row')
#   # clear bookmarks
#   sessionUserBookmarks(c())
#   shiny::updateTextAreaInput(session, inputId = 'requestIntention', value = "")
#   showModal(modalDialog(title = "Request Received",
#                         p("Thank you for submitting a request for data!"),
#                         p("The Data Manager will process your request soon.")))
# })

#refresh button action
observeEvent(input$searchRefresh,{
  lsfMetadata(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
  lsfMetadata(sf::st_as_sf(lsfMetadata(), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  
  metadataFilterReactive(lsfMetadata())
  # clear existing markers
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  # and redraw
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

observeEvent(input$searchFilterReset,{
  metadataFilterReactive(lsfMetadata())
  activeGeographicFilterReactive("No Filter Selected")
  # clear existing markers
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  # and redraw
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

###################################################
# Write a HTML Legend (As have used HTML ICONS and no gradient)

# TODO: improve visual information in markers, colour index rivers or use river icon, check out the IYS icons
output$searchTabMap <- leaflet::renderLeaflet({ 
  leaflet::leaflet (options = leaflet::leafletOptions(minZoom = 3,maxZoom = 19))%>%
    leaflet::setView(lng = -20,lat = 50,zoom = 3) %>% 
    leaflet::setMaxBounds( lng1 = -180
                           , lat1 = -90
                           , lng2 = 210
                           , lat2 = 90 ) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, options = leaflet::providerTileOptions(minZoom = 3, maxZoom =10)) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, options = leaflet::providerTileOptions(minZoom = 11, maxZoom = 19)) %>%
    
    leaflet::addMarkers(data = lsfMetadata(),
                        label = ~metadataTitle,
                        layerId = ~id,
                        group = 'Data Source',
                        popup = ~paste("<h3>More Information</h3>",
                                       "<b>Title:</b>",stringr::str_trunc(metadataTitle,width = 90,side = 'right',ellipsis = '...'),"<br>","<br>",
                                       "<b>Abstract:</b>",stringr::str_trunc(metadataAbstract,width = 200,side = 'right',ellipsis = '...'),"<br>","<br>",
                                       "<b>Organisation:</b>",metadataOrganisation,"<br>","<br>",
                                       "<b>URL (if available):</b>",metadataAltURI,"<br>","<br>",
                                       "<em>UUID:</em>",metadataUUID,"<br>","<br>",
                                       "<em>Internal ID:</em>",id,"<br>","<br>",
                                       "&nbsp;",actionButton("showmodal", "View more...", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
                                       sep =" "),
                        # enable clustering for spiderfy
                        clusterOptions = leaflet::markerClusterOptions(
                          showCoverageOnHover = TRUE,
                          zoomToBoundsOnClick = TRUE,
                          spiderfyOnMaxZoom = TRUE,
                          removeOutsideVisibleBounds = TRUE,
                          maxClusterRadius = 30,
                          spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5))) %>%
    
    leaflet::addMarkers(data = indexRiversSF,
                        label = ~paste("Salmon Index River: ",rivername),
                        group = "ICES Index Rivers",
                        icon = list(
                          iconUrl = "https://img.icons8.com/cotton/64/000000/salmon--v1.png",
                          iconSize = c(35, 35))) %>%
    
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
    
    leaflet::addPolygons(data = salmosalarRange,
                         label = ~name,
                         layerId = paste0("range_",salmosalarRange$ogc_fid),
                         color = "pink", group = "Commonly Accepted Range", weight = 1,
                         highlightOptions = leaflet::highlightOptions(color = "purple", weight = 3,
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
                                                                     "Commonly Accepted Range",
                                                                     "NASCO Rivers DB"),
                              options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::hideGroup(c("ICES Ecoregions",
                         "NAFO Divisions",
                         "ICES Index Rivers",
                         #"ICES Stat Squares",
                         "Proposed Outward Migration",
                         "Commonly Accepted Range",
                         "NASCO Rivers DB")) %>%
    # Customise layer control title
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left; font-size:16px;\">Layer Control</label>');
        }
    ") %>% 
    
    # assign the leaflet object to variable 'map' for use with custom css
    # assists with click interactions on the table
    htmlwidgets::onRender("
          function(el, x) {
            map = this;
          }"
    )
  # commenting out legend for now, the layer control in a way works as a legend and the screen was a bit cluttered with both
  #leaflet::addControl(position = "bottomright", html = html_legend)
})


# Create standard metadata map marker popup information
redrawFilteredMarkers <- function(filteredTibble,session){
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::addMarkers(data = filteredTibble,
                        label = ~metadataTitle,
                        layerId = ~id,
                        group = 'Data Source',
                        popup = ~paste("<h3>",id," - More Information</h3>",
                                       "<b>Title:</b>",stringr::str_trunc(metadataTitle,width = 90,side = 'right',ellipsis = '...'),"<br>","<br>",
                                       "<b>Abstract:</b>",stringr::str_trunc(metadataAbstract,width = 200,side = 'right',ellipsis = '...'),"<br>","<br>",
                                       "<b>Organisation:</b>",metadataOrganisation,"<br>","<br>",
                                       "<b>URL (if available):</b>",metadataAltURI,"<br>","<br>",
                                       "<em>UUID:</em>",metadataUUID,"<br>","<br>",
                                       "<em>Internal ID:</em>",id,"<br>","<br>",
                                       "&nbsp;",actionButton("showmodal", "View more...", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
                                       sep =" "),
                        # enable clustering for spiderfy
                        clusterOptions = leaflet::markerClusterOptions(
                          showCoverageOnHover = TRUE,
                          zoomToBoundsOnClick = TRUE,
                          spiderfyOnMaxZoom = TRUE,
                          removeOutsideVisibleBounds = TRUE,
                          maxClusterRadius = 30,
                          spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5)))
}

# Geographic Search Tab Table Output
# Note complexity due to relationship with leaflet map viewing pane bounding box.
# The table only displays information on markers that are within the bounding box.
# initialise reactive value for clearer code
intersectVector <- reactiveVal()

output$searchTabTable <- DT::renderDT({
  req(input$searchTabMap_bounds)
  # Create Intersection vector to filter table
  intersectVector(
    as.vector(
      sf::st_intersects(
        sf::st_polygon(
          list(
            rbind( # NOTE - build polygon clockwise AND WKT notation has longitude first, latitude second
              as.numeric(input$searchTabMap_bounds[c(4,1)]),
              as.numeric(input$searchTabMap_bounds[c(2,1)]),
              as.numeric(input$searchTabMap_bounds[c(2,3)]),
              as.numeric(input$searchTabMap_bounds[c(4,3)]),
              as.numeric(input$searchTabMap_bounds[c(4,1)])
            )
          )
        ),
        metadataFilterReactive(),sparse = FALSE
      )
    )
  )
  sf::st_set_geometry(metadataFilterReactive()[intersectVector(),c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
},
selection = 'single',
rownames = FALSE,
editable = FALSE,
colnames = c('Title','Abstract','Keywords'),
options = list(pageLength = 15,
               columnDefs = list(list(visible=FALSE, targets=c(1,2)))
)

)

# Online resource only filter - Display only metadata that has an entry in metadataAltURI
observeEvent(input$onlineOnlyFilter,{
  req(lsfMetadata())# don't activate until lsfMetadata is populated (after user log on)
  if(!input$onlineOnlyFilter){
    metadataFilterReactive(lsfMetadata())
    leaflet::leafletProxy('searchTabMap', session) %>%
      leaflet::clearGroup(group = 'Data Source')
  }else{
    metadataFilterReactive(lsfMetadata()[!(lsfMetadata()$metadataAltURI == ""),])
    leaflet::leafletProxy('searchTabMap', session) %>%
      leaflet::clearGroup(group = 'Data Source')
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})


###########################################
# Map metadata node marker filter OBSERVERS
###########################################

###############################################
# Geograhpic Filters
# Filter on polygon click section

activeGeographicFilterReactive <- reactiveVal("No Filter Selected")

# One observer for searchTabMap_shape_click, search based on layer id
observeEvent(input$searchTabMap_shape_click,{
  # first detect shape layer name and ignore if not one of the searchable layers
  if(input$searchTabMap_shape_click[3] %in% c("ICES Ecoregions","NAFO Divisions")){
    # if TRUE, next step clear all Data Source markers
    leaflet::leafletProxy('searchTabMap', session) %>%
      leaflet::clearGroup(group = 'Data Source')
    # get name of area clicked using id and pass to filter metadata
    if(input$searchTabMap_shape_click[3] == "ICES Ecoregions"){
      layerID <- stringr::str_split(input$searchTabMap_shape_click[1],"_",simplify = T)[,2]
      layerIDname <- ICES_Ecoregions[ICES_Ecoregions$objectid == layerID,]$ecoregion
      metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectICESEcoRegion,layerIDname),])
      activeGeographicFilterReactive(paste0("ICES Ecoregions - ",layerIDname))
    }else{
      layerID <- stringr::str_split(input$searchTabMap_shape_click[1],"_",simplify = T)[,2]
      layerIDname <- nafoDivisionsSF[nafoDivisionsSF$ogc_fid == layerID,]$zone
      metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectNAFODivision,layerIDname),])
      activeGeographicFilterReactive(paste0("NAFO Divisions - ",layerIDname))
    }
    # redraw new filtered data
    redrawFilteredMarkers(metadataFilterReactive(),session)
  }
  
})


output$activeGeographicFilter <- renderText(activeGeographicFilterReactive())


# Eco regions based on pre-calculated intersects
observeEvent(input$ecoregionFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$ecoregionFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectICESEcoRegion,input$ecoregionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})


# NAFO divisions based on pre-calculated intersects
observeEvent(input$nafodivisionFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$nafodivisionFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectNAFODivision,input$nafodivisionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

# migration routes based on pre-calculated intersects
observeEvent(input$migrationRouteFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$migrationRouteFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    # apply regex escape
    filterValue <- str_replace_all(str_replace_all(input$migrationRouteFilter,"\\(","\\\\("),"\\)","\\\\)")
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectMigrationRoutes,filterValue),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

###############################################
# Temporal Filters
# observeEvent(input$temporalSlider,{
#   leaflet::leafletProxy('searchTabMap', session) %>%
#     leaflet::clearGroup(group = 'Data Source')
#   # double slider control - filter resources based on start and end year
#   # filters resources with start date LESS THAN OR EQUAL TO first selection AND end date GREATER THAN OR EQUAL TO second selection
#   metadataFilterReactive(lsfMetadata()[lsfMetadata()$metadataCoverageStartYear <= input$temporalSlider[1] & lsfMetadata()$metadataCoverageEndYear >= input$temporalSlider[2],])
#   redrawFilteredMarkers(metadataFilterReactive(),session)
# })
# 
# observeEvent(input$monthsSelect,{
#   leaflet::leafletProxy('searchTabMap', session) %>%
#     leaflet::clearGroup(group = 'Data Source')
#   if(is.null(input$monthsSelect)){
#     metadataFilterReactive(lsfMetadata())
#   }else{
#     targets <- paste0("[(",paste0(input$monthsSelect,collapse = ")|("),")]")
#     metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageMonthsOfYear,targets),])
#   }
#   redrawFilteredMarkers(metadataFilterReactive(),session)
# },ignoreNULL = FALSE)


# Observer for Search Map Click - Action: When user clicks a marker add the extents of the marker data source as rectangle to the map
observeEvent(input$searchTabMap_marker_click,{
  leaflet::leafletProxy('searchTabMap') %>%
    leaflet::clearGroup(group = 'markerRectangle') %>%
    leaflet::addRectangles(lsfMetadata()[lsfMetadata()$id == input$searchTabMap_marker_click[1],]$metadataCoverageWest,
                           lsfMetadata()[lsfMetadata()$id == input$searchTabMap_marker_click[1],]$metadataCoverageNorth,
                           lsfMetadata()[lsfMetadata()$id == input$searchTabMap_marker_click[1],]$metadataCoverageEast,
                           lsfMetadata()[lsfMetadata()$id == input$searchTabMap_marker_click[1],]$metadataCoverageSouth,
                           group = 'markerRectangle', color = "blue", weight = 1, stroke = TRUE)
}
)

# Observer for Search Map Click - Action: clear rectangle on background click
observeEvent(input$searchTabMap_click,{
  leaflet::leafletProxy('searchTabMap') %>%
    leaflet::clearGroup(group = 'markerRectangle')
})
# Observer for Search Datatable Click - Action: activate popup on map marker
observeEvent(input$searchTabTable_rows_selected,{
  rowIndex <- input$searchTabTable_rows_selected
  metadataFilterReactive()[intersectVector(),]$id[rowIndex]
  shinyjs::js$markerClick(metadataFilterReactive()[intersectVector(),]$id[rowIndex])
})
############################################## 
# Observer for Search Map Click - Action: Modal pop-up on each marker_click

observeEvent(input$button_click, {
  click = input$searchTabMap_marker_click
  showModal(modalDialog(
    title = "Information on Selected Data Source",
    h4("More Information"),
    em("UUID:    "), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataUUID),
    br(),br(),
    em("Title:   "), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataTitle),
    br(),br(),
    em("Abstract:   "),paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataAbstract),
    br(), br(),
    em("Organisation:   "),paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataOrganisation),
    br(),br(),
    em("URL (if available):   "), tags$a(href = lsfMetadata()[lsfMetadata()$id == click[1],]$metadataAltURI,lsfMetadata()[lsfMetadata()$id == click[1],]$metadataAltURI, target = '_blank'),
    br(),br(),
    em("Update Frequency:   "), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataMaintenance),
    br(),br(),
    h4("Spatial Information"),
    em("North:"), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageNorth),br(),
    em("East:"),paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageEast),br(),
    em("South:"), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageSouth),br(),
    em("West:"), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageWest),br(),
    br(),br(),
    em("Geographical Description:"), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataGeographicDescription),
    br(),br(),
    h4("Temporal Information"),
    em("Start Year: "), paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageStartYear), br(), em("End Year: ", paste(lsfMetadata()[lsfMetadata()$id == click[1],]$metadataCoverageEndYear)),
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
  click = input$searchTabMap_marker_click
  if(lsfMetadata()[lsfMetadata()$id == click[1],]$id %in% sessionUserBookmarks()){
    h4("This resource is in your bookmarks.")
  }else if(lsfMetadata()[lsfMetadata()$id == click[1],]$id %in% neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," RETURN id(m) as id;"),con = neo_con, type = 'row')$id$value){
    box(
      headerBorder = F,
      status = 'warning',
      title = "You have requested this resource already.",
      p("Request Date:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",lsfMetadata()[lsfMetadata()$id == click[1],]$id," RETURN r.created as date;"),con = neo_con, type = 'row')$date$value),
      p("Request Status:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",lsfMetadata()[lsfMetadata()$id == click[1],]$id," RETURN r.status as status;"),con = neo_con, type = 'row')$status$value)
    )
  }else{
    actionButton('Request', "Add to Bookmarks")
  }
})

# Action button uses information from the searchTabMap_marker_click to update the bookmarks list 
observeEvent(input$Request, {
  # when user adds to bookmark disable request button and change label to indicate action completed
  shinyjs::disable(id = 'Request')
  updateActionButton(session, inputId = 'Request',label = "Added to bookmarks!")
  click = input$searchTabMap_marker_click
  sourceIDString <- paste0(lsfMetadata()[lsfMetadata()$id == click[1],]$id)
  sessionUserBookmarks(append(sessionUserBookmarks(),sourceIDString))
  # update database bookmark list
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
})


# Debugging Information
output$clickMarkerOutput <- renderText({paste0("Marker: ",paste0(input$searchTabMap_marker_click,collapse = ","))})
output$clickOutput <- renderText({paste0("Click: ",paste0(input$searchTabMap_click,collapse = ","))})
output$clickShapeOutput <- renderText({paste0("Shape: ",input$searchTabMap_shape_click,collapse = ",")})
output$clickBoundsOutput <- renderText({paste0("Bounds: ",paste0(input$searchTabMap_bounds,collapse = ","))})
output$clickTableRow <- renderText({paste0("Row Info: ",input$searchTabTable_rows_selected,collapse = ",")})
output$intersectVectorOut <- renderText({paste0(intersectVector(),collapse = ",")})
############################
# DataSearch_server.R END
############################
