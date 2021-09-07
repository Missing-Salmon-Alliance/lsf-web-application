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
               #label = ~metadataTitle,
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


##########################
# COPY Section
# Some text is duplicated across different parts of the application
# This can be a central location for them so edits can be made in one place

generalIntroCopyPara1 <- "In response to the dramatic declines of Atlantic salmon populations, the 
Missing Salmon Alliance has developed the Likely Suspects Framework. This programme hopes to improve 
our understanding of the factors driving salmon population trends to help provision salmon managers 
with new tools to support their future decisions to help stabilise or reverse these declines in salmon."

generalIntroCopyPara2 <- "Salmon management is frequently impeded by poor access to data that may support 
changing their approaches. The Likely Suspects Framework is working to mobilise biological, physical and 
salmon specific information from freshwater and marine environments located around the North Atlantic."

carouselGeneralIntro <- "The Central Data Resource is an online portal intending to provide a unique
platform for Atlantic salmon knowledge mobilisation. The portal consists of an area inviting users
to register salmon knowledge through a simplified metadata submission form, and a set of search areas
to guide researchers to the data they need to test hypotheses. Registered knowledge sources are
indexed within the resource by how they relate to the Atlantic salmon by Life-Stage via the use of
temporal detail, spatial detail, and specially defined salmon domains, as well as information on the
classification of variables within the data. All knowledge in the resource is present because it is
relevant to Atlantic salmon ecology, be that through Primary Observation, Derived Output, or Modelled Output."

searchIntroCopy <- "The search areas allow exploration of the knowledge sources that have been
registered in the Central Data Resource. These searches are presented in ways to facilitate different
use cases, from a location-based map search, free text based tabular search, to predefined filters
based on Mortality Hypotheses or Atlantic salmon Life-Stage."

searchIntroCopyPara1 <- "This interface allows exploration of the knowledge resources we have registered. Our system 
categorises data regarding both its temporal and spatial extents and additionally how it may be applicable to Atlantic salmon. 
We use sets of biological, physical and salmon specific variable classes to index observations. By tagging the collection 
with these classes, we hope to improve your ability to efficiently find relevant data regarding many ecological factors 
across a range of specific dates and geographical locations."

searchIntroCopyPara2 <- "The interface contains a range of tools to help focus your data search on several levels. Filtering the 
data will return descriptions of data (metadata) relevant to your search terms. This metadata will help you to discover relevant 
knowledge sources and allow you to gain access via either primary sources or from within the Central Data Resource."


submitIntroCopy <- "Through this portal you may submit knowledge sources via an easy-to-follow metadata
form. The metadata captured ties in to existing metadata standards to ensure the descriptions have
longevity and portability across other data repositories. The metadata form has a minimum set of fields
to capture at least the existence of a dataset along with owner contact details, plus more expansive
fields to capture detail on location, time and attributes, among other information. The more detail
that can be supplied at this stage then the more utility the resource can add to the search areas."


submitIntroCopyPara1 <- "This interface allows you to submit data along with a thorough description (metadata) into the Central Data 
Resource. This data will be used to provision both managers and researchers in their fight to reduce declining salmon 
abundance. Our interface will also allow you to categorise your data into salmon life-stage domains and variable classes. The variable classes 
are a range of biological, physical and salmon-specific categories. By labelling your data with these classes, we hope to improve the 
ability for people to efficiently find relevant data regarding various ecological factors across a range of specific dates and 
geographical locations. The life-stage domains refer to labelling your data regarding a specific habitat the salmon experience during their 
lifecycle e.g. river or ocean."

submitIntroCopyPara2_OLDKNB <- "This interface ensures the data submitted into the Likely Suspects Framework database is labelled to both 
spatial and temporal extents and linked to how it may be applicable to Atlantic salmon. If the data is already within the KNB database 
(The Knowledge Network for Biocomplexity) then you may load the meta information via the URN or DOI always tagged to KNB 
metadata. This will automatically fill the fields of this page as long as the equivalent field is present on KNB."

submitIntroCopyPara2 <- "This interface ensures the data submitted into the Central Data Resource is labelled to both 
spatial and temporal extents and linked to how it may be applicable to Atlantic salmon."

domainsDescriptionCopy <- "Consideration of the range of space-time domains occupied by salmon as they
transition life-stages and change habitats is important when discussing the range of changing conditions,
possible pressures and the potential drivers of the survival patterns we see in populations through time.
The conditions and factors influencing salmon in these series of domains will vary and may result in a
direct response (e.g. death or poor growth) be expressed cumulatively with responses in subsequent domain
or act together in more complex (synergistic) ways on salmon survival chances."

domainsDescriptionCopyx <- "Life-Stage Domains refer to the various environments Atlantic salmon move through
during their lifecycle, including freshwater, transition and marine. The salmon experience different
conditions and pressures at different domains and stages in their lifecycle. The domains are further
split to represent the life-stages of the salmon."

esvDescriptionCopy <- "The variable classes we use
are a list of general groupings and specific
variables into which salmon related knowledge
can be classified. These classes are themselves grouped
into three primary categories: Physical environment,
Biological processes, and Salmon Traits (observation or assessment specific to the salmon).
Knowledge resources may originate from empirical (direct 
measurement), derived (simulated outputs) or expert 
(opinion) sources."


### Global Section End