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

# basket modal
# TODO: Move all modals defined in UI files to observers in SERVER files
# TODO: Alternatively all modals could reside in a single file such as searchDataSource_modals_server.R for easy access
observeEvent(input$basket,{
  showModal(
    modalDialog(title = "Your Basket", size = "l",
                fluidPage(title = "Your Basket",
                          box(width = 12, h4("Basket Contents"),
                              DT::DTOutput('basketContentsTable'),
                              column(width = 6,actionButton('clearBasket', "Clear All Basket Contents")),
                              column(width = 5,actionButton('clearRows', "Delete Selected Row")),
                              column(width = 1, textOutput('basketCountSelect'))
                          ),
                          box(width = 12, h4("Check Out"),
                              textInput('basketName', "Name:", value = user_info()$user_info$fullname),
                              textInput('basketOrganisation', "Organisation:", value = user_info()$user_info$affiliation),
                              #selectInput("basketPosition", "Position/Occupation:", choices = c("Researcher","Database Manager","Government Official", "Conservationist", "Student", "Lecturer", "Other")),
                              #selectInput("basketDataUse", "What will the data be used for?", choices = c("Independent Research", "Conservation", "Guidance to Managers", "Other")),
                              #textInput("basketOther","Please describe what is meant, if you selected other"),
                              #selectInput("basketProvision", "Do you intend to provide data to the Central Data Resource?", choices = c("Yes", "No", "In the Future")),
                              textAreaInput('basketIntention', "Please describe the intended use for the data. Please include information on the project, time scale of usage and expected number of users.", width = "1000px", height = "50px"),
                              actionButton('sendRequest', "Send Data Request")
                              
                          )
                )
    )
  )
})

observeEvent(input$sendRequest,{
  
  # save csv file directly to AWS S3 storage
  # create temp area in memory to write to
  rc <- rawConnection(raw(0), 'r+')
  # write csv to temp area
  write_file(paste(paste(unique(sessionUserBasket()),collapse = ','),input$basketIntention,sep = ','),rc)
  # send csv object from temp area to S3
  aws.s3::put_object(file = rawConnectionValue(rc),bucket = "likelysuspects-datastore/userRequests",object = paste0("user_",user_info()$user_info$id,"_",as.character(Sys.time()),".txt"))
  # close and remove temp area
  close(rc)
  # create requested source relationships in graph
  neo4r::call_neo4j(paste0("MATCH (p:Person{personEmail:'",user_info()$user_info$email,"'}),(m:Metadata) WHERE id(m) IN [",formatNumericList(sessionUserBasket()),"] CREATE (p)-[:HAS_REQUESTED{created:'",Sys.time(),"',lastModified:'",Sys.time(),"',status:'pendingReview'}]->(m)"),con = neo_con,type = 'row')
  # clear basket
  sessionUserBasket(c())
  shiny::updateTextAreaInput(session, inputId = 'basketIntention', value = "")
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
  
  # Convert some columns to numeric
  # TODO: These could be natively numeric in the graph rather than converting them here
  LSFMetadataTibble$metadataCoverageStartYear <- as.numeric(LSFMetadataTibble$metadataCoverageStartYear)
  LSFMetadataTibble$metadataCoverageEndYear <- as.numeric(LSFMetadataTibble$metadataCoverageEndYear)
  LSFMetadataTibble$metadataCoverageWest <- as.numeric(LSFMetadataTibble$metadataCoverageWest)
  LSFMetadataTibble$metadataCoverageNorth <- as.numeric(LSFMetadataTibble$metadataCoverageNorth)
  LSFMetadataTibble$metadataCoverageEast <- as.numeric(LSFMetadataTibble$metadataCoverageEast)
  LSFMetadataTibble$metadataCoverageSouth <- as.numeric(LSFMetadataTibble$metadataCoverageSouth)
  
  # Conversion to GIS enabled dataframe using pre-calculated centroids (see nightly intersection routine)
  LSFMetadataTibble <- sf::st_as_sf(LSFMetadataTibble, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE)
  
  metadataFilterReactive(LSFMetadataTibble)
  
  redrawFilteredMarkers(metadataFilterReactive(),session)
})



# TODO: Create some kind of summary of the applied filters
# e.g. "The data is currently filtered by Domain:River Rearing and ESV:Suspended Solids (freshwater)"
filterAppliedInformation <- reactiveValues()
filterAppliedInformation$filterType <- "None"
filterAppliedInformation$filterName <- "None"

############################################
# Pull data from SQL database using function

icesEcoregions <- loadgeoJSONData("ices_ecoregions","ecoregion")
icesEcoregionsGeoJSON <- convertToGeojsonFeatureCollection(icesEcoregions,"ecoregion")
ICES_Ecoregions <- sf::read_sf(icesEcoregionsGeoJSON)
rm(icesEcoregions,icesEcoregionsGeoJSON)

# neContinents <- loadgeoJSONData("ne_continents","continent")
# neContinentsGeoJSON <- convertToGeojsonFeatureCollection(neContinents,"continent")
# neContinentsSF <- sf::read_sf(neContinentsGeoJSON)
# rm(neContinents,neContinentsGeoJSON)


# nascoRivers <- loadgeoJSONData("nasco_rivers_db","rivername")
# nascoRiversGeoJSON <- convertToGeojsonFeatureCollection(nascoRivers,"rivername")
# nascoRiversSF <- sf::read_sf(nascoRiversGeoJSON)
# rm(nascoRivers,nascoRiversGeoJSON)


nafoDivisions <-loadgeoJSONData("nafo_divisions","zone")
nafoDivisionsGeoJSON <- convertToGeojsonFeatureCollection(nafoDivisions,"zone")
nafoDivisionsSF <- sf::read_sf(nafoDivisionsGeoJSON)
rm(nafoDivisions,nafoDivisionsGeoJSON)


# icesStatEco <- loadgeoJSONData("ices_stat_rect_eco","icesname")
# icesStatEcoGeoJSON <- convertToGeojsonFeatureCollection(icesStatEco,"icesname")
# icesStatEcoSF <- sf::read_sf(icesStatEcoGeoJSON)
# rm(icesStatEco,icesStatEcoGeoJSON)


# migration <- loadgeoJSONData("buffered_seasalar_migration","current")
# migrationGeoJSON <- convertToGeojsonFeatureCollection(migration,"current")
# migrationSF <- sf::read_sf(migrationGeoJSON)
# rm(migration,migrationGeoJSON)
# 
# 
# feeding <- loadgeoJSONData("feeding_zones","name")
# feedingGeoJSON <- convertToGeojsonFeatureCollection(feeding,"name")
# feedingSF <- sf::read_sf(feedingGeoJSON)
# rm(feeding,feedingGeoJSON)

migrationSF <- sf::read_sf("./src/Migration_as_ICES_Squares.shp")

feedingSF <- sf::read_sf("./src/Feeding_Zones.shp")

NASCO_rivers <- read_csv("./src/NASCO_RiversDB.csv",locale = locale(encoding = 'latin1'))
NASCO_rivers <- NASCO_rivers %>% filter(Longitude_Decimal != "") %>%
  filter(Latitude_Decimal != "") %>% filter(RiverName != "") 
NASCO_rivers <- sf::st_as_sf(NASCO_rivers, coords = c("Longitude_Decimal","Latitude_Decimal"), crs = 4326)

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
                              "<b>Title:</b>",metadataTitle,"<br>","<br>",
                              "<b>Abstract:</b>",metadataAbstract,"<br>","<br>",
                              #"<b>Creator:</b>",metadataCreator,"<br>","<br>",
                              "<b>Organisation:</b>", metadataOrganisation,"<br>","<br>",
                              #"<b>Creator Email:</b>",metadataCreatorEmail,"<br>","<br>",
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
    
    leaflet.extras::addSearchFeatures(
      targetGroups = 'Data Source',
      options = leaflet.extras::searchFeaturesOptions(zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE
      )
    ) %>%
    leaflet::addMarkers(data = rivers,label = ~paste("Salmon Index River: ",RiverName), group = "ICES Index Rivers", icon = list(
      iconUrl = "https://img.icons8.com/cotton/64/000000/salmon--v1.png",
      iconSize = c(35, 35))) %>%
    leaflet::addCircleMarkers(data = NASCO_rivers, label = ~RiverName, group = "NASCO River DB", color = "black", radius = 3,
                     stroke = FALSE, fillOpacity = 1) %>%
    # leaflet::addWMSTiles(
    #   "https://gis.ices.dk/gis/services/Mapping_layers/ICES_Statrec_mapto_Ecoregions/MapServer/WMSServer",
    #   layers = "0",
    #   group = "Ecoregions",
    #   options = WMSTileOptions(format = "image/png", transparent = T)
    # ) %>%
    
    leaflet::addPolygons(data = ICES_Ecoregions,
                label = ICES_Ecoregions$name,
                color = "green", group = "Ecoregions", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE))  %>%
    # leaflet::addPolygons(data = icesStatEcoSF,
    #             label = icesStatEcoSF$name,
    #             layerId = ~name, color = "blue", group = "Statistical Squares", weight = 1,
    #             highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
    #                                                 bringToFront = TRUE))  %>%
    
    leaflet::addPolygons(data = nafoDivisionsSF,
                label = nafoDivisionsSF$name,
                color = "purple", group = "NAFO Divisions", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE)) %>%
    leaflet::addPolygons(data = migrationSF,
                label = migrationSF$name,
                color = "blue", group = "Migration Routes", weight = 1,
                highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
                                                    bringToFront = TRUE)) %>%
    
    # leaflet::addPolygons(data = feedingSF,
    #             label = feedingSF$name,
    #             color = "black", group = "Migration Routes", weight = 1,
    #             highlightOptions = leaflet::highlightOptions(color = "yellow", weight = 3,
    #                                                 bringToFront = TRUE)) %>%
    # 
    leaflet::addLayersControl(position = 'topleft',overlayGroups = c("Data Source","ICES Index Rivers","Ecoregions", "NAFO Divisions","Migration Routes","NASCO River DB"),
                     options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("Ecoregions", "NAFO Divisions","Migration Routes", "NASCO River DB")) %>%
    
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

# Eco regions based on pre-calculated intersects
observeEvent(input$ecoregionFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$ecoregionFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion,input$ecoregionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})


# NAFO divisions based on pre-calculated intersects
observeEvent(input$nafodivisionFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$nafodivisionFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectNAFODivision,input$nafodivisionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

# migration routes based on pre-calculated intersects
observeEvent(input$migrationRouteFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$migrationRouteFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    # apply regex escape
    filterValue <- str_replace_all(str_replace_all(input$migrationRouteFilter,"\\(","\\\\("),"\\)","\\\\)")
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes,filterValue),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

###############################################
# Temporal Filters
source("./src/server/searchDataSource_temporalFilters_server.R",local = TRUE)$value


###############################################
# Framework Filters
source("./src/server/searchDataSource_frameworkFilters_server.R",local = TRUE)$value

##############################################
# Polygon Mouse Over

observe(
  {click = input$map_marker_click
  if(is.null(click))
    leaflet::leafletProxy("map") %>%
    leaflet::clearGroup(group = 'markerRectangle')
  else
    leaflet::leafletProxy("map") %>%
    leaflet::clearGroup(group = 'markerRectangle') %>%
    leaflet::addRectangles(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageWest,
                  LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageNorth,
                  LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageEast,
                  LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCoverageSouth
                  ,group = 'markerRectangle', color = "blue", weight = 1, stroke = TRUE)
  }
)

############################################## 
# Modal pop-up on each marker_click

observeEvent(input$button_click, {
  click = input$map_marker_click
  showModal(modalDialog(
    title = "Information on Selected Data Source",
    h4("More Information"),
    em("Title:   "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataTitle),
    br(),br(),
    em("Abstract:   "),paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataAbstract),
    #br(), br(),
    #em("Creator:   "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCreator),
    br(), br(),
    em("Organisation:   "),paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataOrganisation),
    #br(),br(),
    #em("Contact Email:   "), paste(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$metadataCreatorEmail),
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
    uiOutput('addToBasketUI')
  )
  )
})

##############################################
# Basket System

output$addToBasketUI <- renderUI({
  click = input$map_marker_click
  if(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id %in% sessionUserBasket()){
    h4("This resource is in your basket.")
  }else if(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id %in% neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," RETURN id(m) as id;"),con = neo_con, type = 'row')$id$value){
    box(
      headerBorder = F,
      status = 'warning',
      title = "You have requested this resource already.",
      p("Request Date:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id," RETURN r.created as date;"),con = neo_con, type = 'row')$date$value),
      p("Request Status:",neo4r::call_neo4j(paste0("MATCH (p:Person)-[r:HAS_REQUESTED]-(m:Metadata) WHERE id(p) = ",user_info()$user_info$id," AND id(m) = ",LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id," RETURN r.status as status;"),con = neo_con, type = 'row')$status$value)
    )
  }else{
    actionButton('Request', "Add to Basket")
  }
})

# Basket Creation - as a reactive Value
sessionUserBasket <- reactiveVal(c())

# Action button uses information from the map_marker_click to update the Basket 
observeEvent(input$Request, {
  # when user adds to basket disable request button and change label to indicate action completed
  shinyjs::disable(id = 'Request')
  updateActionButton(session, inputId = 'Request',label = "Added to basket!")
  click = input$map_marker_click
  string_basket <- paste0(LSFMetadataTibble[LSFMetadataTibble$id == click[1],]$id)
  sessionUserBasket(append(sessionUserBasket(),string_basket))
  })

# A system for Creator Feedback to see the Basket - NOT REQUIRED
output$baskettext <- renderText(paste0(unique(sessionUserBasket()), collapse = ","))

# An output which renders both the Title and ID. - NOT REQUIRED
output$basketContents <- renderText(paste0(unique(LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBasket(),]$metadataTitle), 
                                           "ID:", 
                                           unique(LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBasket(),]$id, collapse = ", "))
                                    )


# Creation of a DT which shows the contents of the Basket
output$basketContentsTable <- DT::renderDT({
  LSFMetadataTibble[LSFMetadataTibble$id %in% sessionUserBasket(),c("id","metadataTitle")]
}, selection = "single", rownames = FALSE, caption = "Basket Contents")

# Remove a Single Basket Item using Tables_Rows_selected

observeEvent(input$clearRows,{
  
  if (!is.null(input$basketContentsTable_rows_selected)) {
    
    sessionUserBasket(sessionUserBasket()[sessionUserBasket() != sessionUserBasket()[input$basketContentsTable_rows_selected]]) #Need to somehow subset it to ID of selected rows
  }
})


# Clear the Basket using Action Button
observeEvent(input$clearBasket, {
  sessionUserBasket(c())
})

# Basket Count 

# Selected Basket Count - NOT REQUIRED with a single selection function in DT
# output$basketCountSelect <- renderText(length(input$basketContentsTable_rows_selected))

# Render UI which includes both an action link, and a count of the Basket Length. 

output$basketUi <- renderUI({
  req(user_info())
  if (user_info()$result) {
    dynamicLabel <- paste("Basket:",length(unique(sessionUserBasket())))
    actionLink(inputId = "basket", label = dynamicLabel ,icon = icon("shopping-cart"),style='padding:5px; font-size:120%; color:white;float:right;')
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

############################
# DataSearch_server.R END
############################