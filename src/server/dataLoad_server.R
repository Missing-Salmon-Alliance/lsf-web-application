############
# replacement global section
# dummy tibbles and lists to put in global if required
# LSFDomainTibble <- tibble(id=character(0),domainEnvironment=character(0),domainLabel=character(0),domainTitle=character(0),domainDescription=character(0))
# LSFEssentialSalmonVariableTibble <- tibble(id=character(0),esvTitle=character(0),esvCategory=character(0),esvLabel=character(0),esvDescription=character(0))
# 
# LSFMetadataTibble <- tibble(id=character(0),metadataAccessLevel=character(0),metadataCreatorORCID=character(0),metadataGeographicDescription=character(0),metadataCoverageEast=numeric(0),metadataAbstract=character(0),
#                             metadataCoverageNorth=numeric(0),metadataKNBURI=character(0),metadataOrganisation=character(0),metadataCoverageSouth=numeric(0),metadataCoverageWest=numeric(0),metadataUUID=character(0),metadataCreator=character(0),
#                             metadataAvailableOnline=logical(0),metadataQCCheck=logical(0),metadataCoverageStartYear=character(0),metadataCoverageEndYear=character(0),metadataAltURI=character(0),metadataCreatorEmail=character(0),
#                             metadataTimestamp=character(0),metadataTitle=character(0),metadataCoverageIntersectMigrationRoutes=character(0),metadataCoverageIntersectNAFODivision=character(0),
#                             metadataCoverageIntersectICESEcoRegion=character(0),metadataCoverageCentroid=character(0),metadataEmbargoed=logical(0),metadataMaintenance=character(0),metadataCoverageMonthsOfYear=character(0),
#                             metadataAltURI=character(0),metadataEmbargoEnd=character(0))
# 
# LSFHypothesisTibble <- tibble(id=character(0),hypothesisEnvironment=character(0),hypothesisTitle=character(0),hypothesisLabel=character(0))  
# 
# LSFSubHypothesisTibble <- tibble(id=character(0),subHypothesisLabel=character(0),subHypothesisEnvironment=character(0),subHypothesisTitle=character(0))
# 
# uniqueNAFODivisions <- c('1F','1D','1E','2G','2H','2J','4R','3K','4S','4T','3L','3M','3Ps','4Vn','3Pn','3O','3N','4X','4W','4Vs','5Y','5Ze','5Zw','6A','6B','6D','6E','6F','6G','6H','6C','0A','1A','1B','0B','1C')
# uniqueICESEcoRegions <- c('Greenland Sea','Bay of Biscay and the Iberian Coast','Azores','Western Mediterranean Sea','Ionian Sea and the Central Mediterranean Sea','Black Sea','Adriatic Sea','Aegean-Levantine Sea','Celtic Seas','Baltic Sea','Greater North Sea','Arctic Ocean','Icelandic Waters','Barents Sea','Faroes','Norwegian Sea','Oceanic Northeast Atlantic')
# uniqueMigrationRoutes <- c('Atlantic Shelf Edge Current (April)','Atlantic Shelf Edge Current (May)','Atlantic Shelf Edge Current (June)','North Shelf Edge Current (June)','Atlantic Shelf Edge Current (July)','South Norway Coastal Current (June)','South Norway Coastal Current (NA)','South Norway Coastal Current (July)','West Bothnia Bay Current (August)','NA (August)','North Norway Shelf Edge Current (August)','North Norway Shelf Edge Current (September)','South Barents Sea Shelf Current (September)','West Iceland Current (September)')
# uniqueMonths <- c('Apr','May','Jun','Mar','Jan','Feb','Jul','Aug','Sep','Oct','Nov','Dec')
##############


# DataONE settings
# SET ENVIRONMENT
# Production
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")
d1c <- D1Client(cn, mn)
# Staging
# cn <- CNode("STAGING2")
# mn <- getMNode(cn, "urn:node:mnTestKNB") # note command 'unlist(lapply(listNodes(cn), function(x) x@subject))' from dataone-federation vignette
# d1c <- D1Client(cn, mn)
am <- AuthenticationManager()

###################################################
# Pull information from graph
# TODO: This process adds a few seconds to the initial load time of the application. If the graph queries can be integrated in to the
# filters rather than using an R tibble object this would slow the filters down a tiny bit but speed up the application as a whole

metadataESV <- call_neo4j("MATCH (:Metadata)-[r:HAS_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')
esvDomain <- call_neo4j("MATCH (:EssentialSalmonVariable)-[r:HAS_DOMAIN]->(:Domain) RETURN r;",neo_con,type='graph')
hypothesisSubhypothesis <- call_neo4j("MATCH (:Hypothesis)-[r:HAS_SUBHYPOTHESIS]->(:SubHypothesis) RETURN r;",neo_con,type='graph')
subhypothesisESV <- call_neo4j("MATCH (:SubHypothesis)-[r:REQUIRES_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')

metadataESV$nodes <- metadataESV$nodes %>% unnest_nodes('all')
metadataESV$relationships <- metadataESV$relationships %>% unnest_relationships()
esvDomain$nodes <- esvDomain$nodes %>% unnest_nodes('all')
esvDomain$relationships <- esvDomain$relationships %>% unnest_relationships()
hypothesisSubhypothesis$nodes <- hypothesisSubhypothesis$nodes %>% unnest_nodes('all')
hypothesisSubhypothesis$relationships <- hypothesisSubhypothesis$relationships %>% unnest_relationships()
subhypothesisESV$nodes <- subhypothesisESV$nodes %>% unnest_nodes('all')
subhypothesisESV$relationships <- subhypothesisESV$relationships %>% unnest_relationships()

LSFDomainTibble <- esvDomain$nodes[esvDomain$nodes$value == "Domain",] %>% select(matches("^(id|domain*)"))
LSFEssentialSalmonVariableTibble <- esvDomain$nodes[esvDomain$nodes$value == "EssentialSalmonVariable",] %>% select(matches("^(id|esv*)"))
LSFMetadataTibble <- metadataESV$nodes[metadataESV$nodes$value == "Metadata",] %>% select(matches("^(id|metadata*)"))
LSFHypothesisTibble <- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "Hypothesis",] %>% select(matches("^(id|hypothesis*)"))
LSFSubHypothesisTibble <- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "SubHypothesis",] %>% select(matches("^(id|subHypothesis*)"))


metadataESVDomainRelationships <- bind_rows(metadataESV$relationships,esvDomain$relationships)
hypothesisSubHypothesisESVRelationships <- bind_rows(hypothesisSubhypothesis$relationships,subhypothesisESV$relationships)
#tidyup
rm(metadataESV,esvDomain,hypothesisSubhypothesis,subhypothesisESV)

# Conversion to GIS enabled dataframe using pre-calculated centroids (see nightly intersection routine)
LSFMetadataTibble <- sf::st_as_sf(LSFMetadataTibble, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE)

# Grabbing some counts for reporting purposes
uniqueNAFODivisions <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectNAFODivision[LSFMetadataTibble$metadataCoverageIntersectNAFODivision != ""],collapse = ','),pattern = ",")[[1]])
uniqueICESEcoRegions <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion[LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion != ""],collapse = ','),pattern = ",")[[1]])
uniqueMigrationRoutes <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes[LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes != ""],collapse = ','),pattern = ",")[[1]])
uniqueMonths <- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageMonthsOfYear[LSFMetadataTibble$metadataCoverageMonthsOfYear != ""], collapse = ','),pattern = ",")[[1]])


rivers <- read_csv("./src/Index_Rivers_DB.csv")
rivers <- sf::st_as_sf(rivers, coords = c("Longitude_Decimal","Latitude_Decimal"), crs = 4326)
riversSF <- rivers

