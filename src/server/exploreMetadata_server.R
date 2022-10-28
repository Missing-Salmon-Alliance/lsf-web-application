output$searchMapTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    div(
      column(
        width = 4,
        DT::dataTableOutput('metadataExploreTable')
      ),
      column(
        8,
        shinydashboard::box(
          width = 12,
          status = 'primary',
          solidheader = F,
          leaflet::leafletOutput('metadataExploreMap',height = '45vh')
        ),
        shinydashboard::box(width = 12,
          status = "warning",
          solidHeader = F,
          #height = "45vh",
          column(
            6,
            h5(tags$b('Title:')),
            textOutput('title'),
            tags$i(textOutput('doi')),
            h5(tags$b('Abstract:')),
            textOutput('abstract')
          ),
          column(
            6,
            h5(tags$b('Access Protocol:')),
            textOutput('accessProtocol'),
            h5(tags$b('Organisation:')),
            textOutput('organisation'),
            h5(tags$b('URL:')),
            uiOutput('url'),
            h5(tags$b('Geography and Time:')),
            textOutput('geographicDescription'),
            textOutput('geographicExtents'),
            textOutput('temporalRange')
          )
        )
      ), style = "font-size:80%") # reduce font size in table
  }else{
    fluidPage(
      h1("Metadata Explore Area"),
      h3("Please authenticate to access this area")
    )
  }
})

domainExploreReactive <- reactiveVal()

domainSearchSpace <- reactiveVal()
esvSearchSpace <- reactiveVal()
stockunitSearchSpace <- reactiveVal()

# Observe Filters - Action: Update search space and query database
observeEvent(c(input$menu1,input$domainFilter,input$esvFilter1,input$esvFilter2,input$esvFilter3,input$stockunitFilter),{
  req(input$menu1)
  if(input$menu1 == 'searchlsf'){
    # Set up waiter
    wLoadDB$show()
    leaflet::leafletProxy('metadataExploreMap') %>%
      leaflet::clearGroup(group = 'markerRectangle')
    domainSearchSpace(lsfDomains()$id)
    esvSearchSpace(lsfVariableClasses()$id)
    stockunitSearchSpace(stockUnits)
    # create domainFilter search space
    if(is.null(input$domainFilter)){
      domainSearchSpace(lsfDomains()$id) # selecting zero domains has the effect of adding all domains to the search space (i.e., no domain filter applied)
    }else{
      domainSearchSpace(input$domainFilter)
    }
    # create esvFilter search space
    if(is.null(input$esvFilter1) & is.null(input$esvFilter2) & is.null(input$esvFilter3)){
      esvSearchSpace(lsfVariableClasses()$id) # selecting zero variable classes has the effect of adding all variable classes to the search space (i.e., no variable class filter applied)
    }else{
      esvSearchSpace(c(input$esvFilter1,input$esvFilter2,input$esvFilter3))
    }
    
    # create stockunitFilter search space
    if(is.null(input$stockunitFilter)){
      stockunitSearchSpace(stockUnits) # selecting stock units has the effect of adding all stock units to the search space (i.e., no stock unit filter applied)
    }else{
      stockunitSearchSpace(input$stockunitFilter)
    }
    
    
    # # load metadata with filters applied
    filteredMetadata <- neo4r::call_neo4j(
      paste0(
        "MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",
        formatNumericList(esvSearchSpace()),
        "] AND r.domainID IN [",
        formatNumericList(domainSearchSpace()),
        "] RETURN m;"),
      neo_con,type = 'graph')
    
    # deal with empty results
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all') # if valid graph, unnest nodes
      # apply stockunit search space
      filteredMetadata$x <- list(stockunitSearchSpace())
      filteredMetadata$y <- stringr::str_split(filteredMetadata$metadataStockUnit,",")
      filteredMetadata <- filteredMetadata %>% rowwise() %>% mutate(z = paste0(intersect(x,y),collapse = ","))
      
      filteredMetadata <- filteredMetadata[filteredMetadata$z != "",]
      
      if(nrow(filteredMetadata) > 0){
        domainExploreReactive(filteredMetadata)
      }else{
        domainExploreReactive(NULL)
      }
    }else{
      domainExploreReactive(NULL)
    }
    
    if(!is.null(domainExploreReactive())){
      domainExploreReactive(sf::st_as_sf(domainExploreReactive(), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
      redrawFilteredMarkers(domainExploreReactive(),session)
    }else{
      leaflet::leafletProxy('metadataExploreMap', session) %>%
        leaflet::clearGroup(group = 'Data Source')
    }
    wLoadDB$hide()
  }

  
},ignoreNULL = FALSE, ignoreInit = FALSE)

# load search results into table
output$metadataExploreTable <- DT::renderDT({
  if(!is.null(domainExploreReactive())){
    sf::st_set_geometry(domainExploreReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
  }
},
  selection = 'single',
  rownames = FALSE,
  editable = FALSE,
  colnames = c('Title','Abstract','Keywords'),
  options = list(pageLength = 20,stateSave = TRUE,
    columnDefs = list(list(visible=FALSE, targets=c(1,2)),
      list(
        targets = 0,
        # limit row width by truncating text in the title field
        render = htmlwidgets::JS("function(data, type, row, meta) {return type === 'display' && data.length > 90 ? '<span title=' + data + '>' + data.substr(0, 90) + '...</span>' : data;}"))
    ),
    searching = T,
    lengthChange = FALSE,
    autoWidth = FALSE
  )
)

# download search results - Action: prompt user to save dropped rows
output$downloadSearchResults <- downloadHandler(
  filename = function() {
    paste('SalHub_Search_Results_', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    results <- domainExploreReactive()[,c('metadataUUID','metadataTitle','metadataAbstract','metadataOrganisation','metadataAltURI','metadataAccessProtocol','metadataGeographicDescription','metadataCoverageNorth','metadataCoverageEast','metadataCoverageSouth','metadataCoverageWest')]
    names(results) <- c('UUID','Title','Abstract','Organisation','URL','AccessProtocol','GeographicDescription','lat1','lon1','lat2','lon2','centroid')
    readr::write_csv(results,file)
  }
)

###################################################
# Write a HTML Legend (As have used HTML ICONS and no gradient)

# TODO: improve visual information in markers, colour index rivers or use river icon, check out the IYS icons

output$metadataExploreMap <- leaflet::renderLeaflet({ 
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
      popup = ~metadataTitle,
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
    ")# %>% 
  
  # assign the leaflet object to variable 'map' for use with custom css
  # assists with click interactions on the table
  # htmlwidgets::onRender("
  #       function(el, x) {
  #         map = this;
  #       }"
  # )
  # commenting out legend for now, the layer control in a way works as a legend and the screen was a bit cluttered with both
  #leaflet::addControl(position = "bottomright", html = html_legend)
})


# Create standard metadata map marker popup information
redrawFilteredMarkers <- function(filteredTibble,session){
  leaflet::leafletProxy('metadataExploreMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')

  leaflet::leafletProxy('metadataExploreMap', session) %>%
    leaflet::addMarkers(data = filteredTibble,
      label = ~metadataTitle,
      layerId = ~id,
      group = 'Data Source',
      popup = ~metadataTitle,
      # enable clustering for spiderfy
      clusterOptions = leaflet::markerClusterOptions(
        showCoverageOnHover = TRUE,
        zoomToBoundsOnClick = TRUE,
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        maxClusterRadius = 30,
        spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5)))
}

###########################################
# Map metadata node marker filter OBSERVERS
###########################################

# Observer for Search Map Click - Action: When user clicks a marker add the extents of the marker data source as rectangle to the map
observeEvent(input$metadataExploreMap_marker_click,{
  index <- input$metadataExploreMap_marker_click[1]
  # on marker click, clear existing rectangle and add new one from marker information
  leaflet::leafletProxy('metadataExploreMap') %>%
    leaflet::clearGroup(group = 'markerRectangle') %>%
    leaflet::addRectangles(domainExploreReactive()[domainExploreReactive()$id == index,]$metadataCoverageWest,
      domainExploreReactive()[domainExploreReactive()$id == index,]$metadataCoverageNorth,
      domainExploreReactive()[domainExploreReactive()$id == index,]$metadataCoverageEast,
      domainExploreReactive()[domainExploreReactive()$id == index,]$metadataCoverageSouth,
      group = 'markerRectangle', color = "blue", weight = 1, stroke = TRUE)
  # select relevant row in data table
  DT::dataTableProxy('metadataExploreTable') %>%
    DT::selectRows(which(domainExploreReactive()$id == index)) %>%
    DT::selectPage(which(input$metadataExploreTable_rows_all == which(domainExploreReactive()$id == index)) %/% input$metadataExploreTable_state$length + 1)
})

# Observer for Search Map Click - Action: clear rectangle on background click
observeEvent(input$metadataExploreMap_click,{
  leaflet::leafletProxy('metadataExploreMap') %>%
    leaflet::clearGroup(group = 'markerRectangle')
})

# Observer for Search Datatable Click - Action: pan map to centre on selected row and add rectangle/point
observeEvent(input$metadataExploreTable_rows_selected,{
  leaflet::leafletProxy('metadataExploreMap') %>%
    leaflet::clearGroup(group = 'markerRectangle') %>%
    leaflet::addRectangles(
      domainExploreReactive()$metadataCoverageWest[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageNorth[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageEast[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageSouth[input$metadataExploreTable_rows_selected],
      group = 'markerRectangle') %>%
    leaflet::fitBounds(
      domainExploreReactive()$metadataCoverageWest[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageNorth[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageEast[input$metadataExploreTable_rows_selected],
      domainExploreReactive()$metadataCoverageSouth[input$metadataExploreTable_rows_selected],
      options = leaflet::leafletOptions(maxZoom = 10))
})

#uiOutput('addToBookmarksUI') bookmarks UI TODO add to metadata display panel


##############################################
# Bookmark System

output$addToBookmarksUI <- renderUI({
  click = input$metadataExploreMap_marker_click
  if(domainExploreReactive()[domainExploreReactive()$id == click[1],]$id %in% sessionUserBookmarks()){
    h4("This resource is in your bookmarks.")
  }else if(domainExploreReactive()[domainExploreReactive()$id == click[1],]$id %in% neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," RETURN id(m) as id;"),con = neo_con, type = 'row')$id$value){
    box(
      headerBorder = F,
      status = 'warning',
      title = "You have requested this resource already.",
      p("Request Date:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",domainExploreReactive()[domainExploreReactive()$id == click[1],]$id," RETURN r.created as date;"),con = neo_con, type = 'row')$date$value),
      p("Request Status:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",domainExploreReactive()[domainExploreReactive()$id == click[1],]$id," RETURN r.status as status;"),con = neo_con, type = 'row')$status$value)
    )
  }else{
    actionButton('Request', "Add to Bookmarks")
  }
})

# Action button uses information from the metadataExploreMap_marker_click to update the bookmarks list 
observeEvent(input$Request, {
  # when user adds to bookmark disable request button and change label to indicate action completed
  shinyjs::disable(id = 'Request')
  updateActionButton(session, inputId = 'Request',label = "Added to bookmarks!")
  click = input$metadataExploreMap_marker_click
  sourceIDString <- paste0(domainExploreReactive()[domainExploreReactive()$id == click[1],]$id)
  sessionUserBookmarks(append(sessionUserBookmarks(),sourceIDString))
  # update database bookmark list
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
})

# The section renders all the further information text from the selected data table row
output$title <- renderText({domainExploreReactive()$metadataTitle[input$metadataExploreTable_rows_selected]})
output$abstract <- renderText({domainExploreReactive()$metadataAbstract[input$metadataExploreTable_rows_selected]})
output$doi <- renderText({paste0("doi: ",domainExploreReactive()$metadataUUID[input$metadataExploreTable_rows_selected],"/",domainExploreReactive()$id[input$metadataExploreTable_rows_selected])})
output$organisation <- renderText({domainExploreReactive()$metadataOrganisation[input$metadataExploreTable_rows_selected]})
output$url <- renderUI({HTML(paste0("<a href=",domainExploreReactive()$metadataAltURI[input$metadataExploreTable_rows_selected]," target='_blank'>",domainExploreReactive()$metadataAltURI[input$metadataExploreTable_rows_selected],"</a>"))})
output$accessProtocol <- renderText({domainExploreReactive()$metadataAccessProtocol[input$metadataExploreTable_rows_selected]})
output$geographicDescription <- renderText({domainExploreReactive()$metadataGeographicDescription[input$metadataExploreTable_rows_selected]})
output$geographicExtents <- renderText({
  paste0(
    "North: ",domainExploreReactive()$metadataCoverageNorth[input$metadataExploreTable_rows_selected],
    ", East: ",domainExploreReactive()$metadataCoverageEast[input$metadataExploreTable_rows_selected],
    ", South: ",domainExploreReactive()$metadataCoverageSouth[input$metadataExploreTable_rows_selected],
    ", West: ",domainExploreReactive()$metadataCoverageWest[input$metadataExploreTable_rows_selected]
  )
})
output$temporalRange <- renderText({
  paste0(
    "Start Year: ",domainExploreReactive()$metadataCoverageStartYear[input$metadataExploreTable_rows_selected],
    ", End Year: ",domainExploreReactive()$metadataCoverageEndYear[input$metadataExploreTable_rows_selected]
  )
})
############################
# DataSearch_server.R END
############################
