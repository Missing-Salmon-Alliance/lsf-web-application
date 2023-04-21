###################################################
# Pull information from graph

# Load values from graph used directly by drop-downs and maps in the UI

# # Metadata list is used in headerLogon_server.R and all search areas
# lsfMetadata(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
# lsfMetadata(sf::st_as_sf(lsfMetadata(), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
# # Hypotheses list is used in the search hypotheses tab
# lsfHypotheses(neo4r::call_neo4j("MATCH (h:Hypothesis) RETURN h;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
# # Domains list is used in the Submit area and in the domain explorer
# lsfDomains(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder))
# # Variable Classes list is used in the domain explorer
# lsfVariableClasses(neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle))

# ###########################################
# # NEW METHOD - Possible after simplifying SQL tables
# 
ICES_Ecoregions <<- loadFullWKBData("ices_ecoregions_simplified")
nafoDivisionsSF <<- loadFullWKBData("nafo_divisions")
riversDBSF <<- loadFullWKBData('rivers_db')
nascoRiversDBSF <<- riversDBSF[riversDBSF$nasco_rivers_db == TRUE,] # subset of rivers that were sourced from the original NASCO DB
indexRiversSF <<- riversDBSF[riversDBSF$ices_index == TRUE,] # subset of rivers that are ICES index rivers
salmosalarRange <<- loadFullWKBData('atlantic_salmon_range')
salmosalarExtents <<- sf::st_bbox(salmosalarRange)