# neo4r query results are sometimes passed to log files. This is a list of the desired columns/fields that need captured
neo4rResultFields <<- c('contains_updates','nodes_created','nodes_deleted','properties_set','relationships_created','relationship_deleted','labels_added','labels_removed')
###################################################
# Pull information from graph
# TODO: This process adds a few seconds to the initial load time of the application. If the graph queries can be integrated in to the
# filters rather than using an R tibble object this would slow the filters down a tiny bit but speed up the application as a whole

## Deal with that TODO above
# To move towards integrated graph queries I'm changing the values used by drop-downs in the UI to simple lists directly from the graph
# Domains list is used in the Submit area and in the domain explorer
lsfDomains(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
# Metadata list is used in headerLogon_server.R and all search areas
lsfMetadata(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
lsfMetadata(sf::st_as_sf(lsfMetadata(), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
##

# # Initial DB calls - require cleaning and organising
# metadataESV <- neo4r::call_neo4j("MATCH (:Metadata)-[r:HAS_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')
# esvDomain <- neo4r::call_neo4j("MATCH (:EssentialSalmonVariable)-[r:HAS_DOMAIN]->(:Domain) RETURN r;",neo_con,type='graph')
# hypothesisSubhypothesis <- neo4r::call_neo4j("MATCH (:Hypothesis)-[r:HAS_SUBHYPOTHESIS]->(:SubHypothesis) RETURN r;",neo_con,type='graph')
# subhypothesisESV <- neo4r::call_neo4j("MATCH (:SubHypothesis)-[r:REQUIRES_ESV]->(:EssentialSalmonVariable) RETURN r;",neo_con,type='graph')
# 
# # Cleaning
# metadataESV$nodes <- metadataESV$nodes %>% neo4r::unnest_nodes('all')
# metadataESV$relationships <- metadataESV$relationships %>% neo4r::unnest_relationships()
# esvDomain$nodes <- esvDomain$nodes %>% neo4r::unnest_nodes('all')
# esvDomain$relationships <- esvDomain$relationships %>% neo4r::unnest_relationships()
# hypothesisSubhypothesis$nodes <- hypothesisSubhypothesis$nodes %>% neo4r::unnest_nodes('all')
# hypothesisSubhypothesis$relationships <- hypothesisSubhypothesis$relationships %>% neo4r::unnest_relationships()
# subhypothesisESV$nodes <- subhypothesisESV$nodes %>% neo4r::unnest_nodes('all')
# subhypothesisESV$relationships <- subhypothesisESV$relationships %>% neo4r::unnest_relationships()
# 
# # Final variables to be passed to reactiveVals
# LSFDomainTibble <<- esvDomain$nodes[esvDomain$nodes$value == "Domain",] %>% dplyr::select(matches("^(id|domain*)")) %>% dplyr::arrange(domainOrder)
# LSFEssentialSalmonVariableTibble <<- esvDomain$nodes[esvDomain$nodes$value == "EssentialSalmonVariable",] %>% dplyr::select(matches("^(id|esv*)"))
# LSFMetadataTibble <<- metadataESV$nodes[metadataESV$nodes$value == "Metadata",] %>% dplyr::select(matches("^(id|metadata*)"))
# LSFMetadataTibble <<- LSFMetadataTibble %>% dplyr::arrange(id) # order by ID for QC area
# LSFHypothesisTibble <<- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "Hypothesis",] %>% dplyr::select(matches("^(id|hypothesis*)"))
# LSFSubHypothesisTibble <<- hypothesisSubhypothesis$nodes[hypothesisSubhypothesis$nodes$value == "SubHypothesis",] %>% dplyr::select(matches("^(id|subHypothesis*)"))
# 
# 
# metadataESVDomainRelationships <<- dplyr::bind_rows(metadataESV$relationships,esvDomain$relationships)
# hypothesisSubHypothesisESVRelationships <<- dplyr::bind_rows(hypothesisSubhypothesis$relationships,subhypothesisESV$relationships)
# #tidyup
# rm(metadataESV,esvDomain,hypothesisSubhypothesis,subhypothesisESV)
# 
# # Conversion to GIS enabled dataframe using pre-calculated centroids (see nightly intersection routine)
# LSFMetadataTibble <<- sf::st_as_sf(LSFMetadataTibble, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE)
# 
# # Grabbing some counts for reporting purposes
# uniqueNAFODivisions <<- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectNAFODivision[LSFMetadataTibble$metadataCoverageIntersectNAFODivision != ""],collapse = ','),pattern = ",")[[1]])
# uniqueICESEcoRegions <<- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion[LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion != ""],collapse = ','),pattern = ",")[[1]])
# uniqueMigrationRoutes <<- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes[LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes != ""],collapse = ','),pattern = ",")[[1]])
# uniqueMonths <<- unique(str_split(paste0(LSFMetadataTibble$metadataCoverageMonthsOfYear[LSFMetadataTibble$metadataCoverageMonthsOfYear != ""], collapse = ','),pattern = ",")[[1]])
# 
# ###########################################
# # NEW METHOD - Possible after simplifying SQL tables
# 
# ICES_Ecoregions <<- loadFullWKBData("ices_ecoregions_simplified")
# nafoDivisionsSF <<- loadFullWKBData("nafo_divisions")
# migrationSF <<- loadFullWKBData("proposed_migration_routes")
# riversDBSF <<- loadFullWKBData('rivers_db')
# nascoRiversDBSF <<- riversDBSF[riversDBSF$nasco_rivers_db == TRUE,] # subset of rivers that were sourced from the original NASCO DB
# indexRiversSF <<- riversDBSF[riversDBSF$ices_index == TRUE,] # subset of rivers that are ICES index rivers
salmosalarRange <<- loadFullWKBData('atlantic_salmon_range')
salmosalarExtents <<- sf::st_bbox(salmosalarRange)

#icesStatEcoSF <- loadFullWKBData("ices_stat_rect_eco")
#feedingSF <- loadFullWKBData("feeding_zones")