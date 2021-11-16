### Global Section

require(shiny)
require(shinydashboard) # css styles
require(shinydashboardPlus) # advanced features
#require(neo4r)# interact with neo4j graph db
#require(visNetwork)# view and manipulate networks
#require(shinyWidgets)# create different input styles
#require(dplyr)
#require(DT)
#require(readr)
#require(RPostgres)
#require(DBI)#interact with database
#require(leaflet)# create maps
#require(rgdal)# map calcs
#require(sf)
#require(dataone)# interact with knb/dataone datasets
#require(stringr)# used to interact with dataone/knb API
require(shinyBS)# create modals
#require(aws.s3)# s3 storage R api
#require(uuid)
#require(crosstalk)
#require(shinyjs)
require(tidyverse)
#require(plotly)
#require(xml2)
#require(shinybusy)
#require(data.table)
#require(leaflet.extras)

source("./src/secrets.R",local = TRUE)

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

# Custom functions
source("./src/custom_functions.R",local = TRUE)

# General website text
source("./src/general_copy.R",local = TRUE)

## Create NEO4J Connection object

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
                         user = NEO4J_USER,
                         password = NEO4J_PASSWD)  
# neo4r query results are sometimes passed to log files. This is a list of the desired columns/fields that need captured
neo4rResultFields <- c('contains_updates','nodes_created','nodes_deleted','properties_set','relationships_created','relationship_deleted','labels_added','labels_removed')
###################################################
# Pull information from graph
# TODO: This process adds a few seconds to the initial load time of the application. If the graph queries can be integrated in to the
# filters rather than using an R tibble object this would slow the filters down a tiny bit but speed up the application as a whole

metadataESV <- neo4r::call_neo4j("MATCH (:Metadata)-[r:HAS_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')
esvDomain <- neo4r::call_neo4j("MATCH (:EssentialSalmonVariable)-[r:HAS_DOMAIN]->(:Domain) RETURN r;",neo_con,type='graph')
hypothesisSubhypothesis <- neo4r::call_neo4j("MATCH (:Hypothesis)-[r:HAS_SUBHYPOTHESIS]->(:SubHypothesis) RETURN r;",neo_con,type='graph')
subhypothesisESV <- neo4r::call_neo4j("MATCH (:SubHypothesis)-[r:REQUIRES_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')

metadataESV$nodes <- metadataESV$nodes %>% neo4r::unnest_nodes('all')
metadataESV$relationships <- metadataESV$relationships %>% neo4r::unnest_relationships()
esvDomain$nodes <- esvDomain$nodes %>% neo4r::unnest_nodes('all')
esvDomain$relationships <- esvDomain$relationships %>% neo4r::unnest_relationships()
hypothesisSubhypothesis$nodes <- hypothesisSubhypothesis$nodes %>% neo4r::unnest_nodes('all')
hypothesisSubhypothesis$relationships <- hypothesisSubhypothesis$relationships %>% neo4r::unnest_relationships()
subhypothesisESV$nodes <- subhypothesisESV$nodes %>% neo4r::unnest_nodes('all')
subhypothesisESV$relationships <- subhypothesisESV$relationships %>% neo4r::unnest_relationships()

LSFDomainTibble <- esvDomain$nodes[esvDomain$nodes$value == "Domain",] %>% dplyr::select(matches("^(id|domain*)")) %>% dplyr::arrange(domainOrder)
LSFEssentialSalmonVariableTibble <- esvDomain$nodes[esvDomain$nodes$value == "EssentialSalmonVariable",] %>% dplyr::select(matches("^(id|esv*)"))
LSFMetadataTibble <- metadataESV$nodes[metadataESV$nodes$value == "Metadata",] %>% dplyr::select(matches("^(id|metadata*)"))
LSFMetadataTibble <- LSFMetadataTibble %>% dplyr::arrange(id) # order by ID for QC area
LSFHypothesisTibble <- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "Hypothesis",] %>% dplyr::select(matches("^(id|hypothesis*)"))
LSFSubHypothesisTibble <- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "SubHypothesis",] %>% dplyr::select(matches("^(id|subHypothesis*)"))


metadataESVDomainRelationships <- dplyr::bind_rows(metadataESV$relationships,esvDomain$relationships)
hypothesisSubHypothesisESVRelationships <- dplyr::bind_rows(hypothesisSubhypothesis$relationships,subhypothesisESV$relationships)
#tidyup
rm(metadataESV,esvDomain,hypothesisSubhypothesis,subhypothesisESV)

# Conversion to GIS enabled dataframe using pre-calculated centroids (see nightly intersection routine)
LSFMetadataTibble <- sf::st_as_sf(LSFMetadataTibble, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE)

# Grabbing some counts for reporting purposes
uniqueNAFODivisions <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectNAFODivision[LSFMetadataTibble$metadataCoverageIntersectNAFODivision != ""],collapse = ','),pattern = ",")[[1]])
uniqueICESEcoRegions <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion[LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion != ""],collapse = ','),pattern = ",")[[1]])
uniqueMigrationRoutes <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes[LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes != ""],collapse = ','),pattern = ",")[[1]])
uniqueMonths <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageMonthsOfYear[LSFMetadataTibble$metadataCoverageMonthsOfYear != ""], collapse = ','),pattern = ",")[[1]])

###############################################

# Create standard map marker popup information
# NOTE, CHANGES TO THE POPUP HERE SHOULD ALSO BE MADE TO THE INITIAL leaflet() CALL IN SEARCH TAB
redrawFilteredMarkers <- function(filteredTibble,session){
  leaflet::leafletProxy("map", session) %>%
    leaflet::addMarkers(data = filteredTibble,
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
               # enable clustering for spiderfy, set freezeAtZoom so that actual clustering does not occur
               clusterOptions = leaflet::markerClusterOptions(
                 showCoverageOnHover = FALSE,
                 zoomToBoundsOnClick = FALSE,
                 spiderfyOnMaxZoom = TRUE,
                 removeOutsideVisibleBounds = TRUE,
                 spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
                 freezeAtZoom = 10))
}


###########################
# Misc functions
# function used to create a csv formatted string from shiny multi-select inputs (converts R vector to single csv string)
formatCheckboxGroupCategories <- function(categories){
  x <- paste("'",paste(categories,collapse = "', '"),"'",sep = '')
  return(x)
}

formatNumericList <- function(categories){
  x <- paste(categories,collapse = ",")
  return(x)
}

# Check box group button icon options
checkboxGroupButtonsIcons <- list(yes = icon("ok",lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
# Alternative design - Keep for an example
#checkboxGroupButtonsIcons <- list(yes = tags$i(class = "fa fa-check-square",style = "color: steelblue"),no = tags$i(class = "fa fa-square-o",style = "color: steelblue"))


### Global Section End
