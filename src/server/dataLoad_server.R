###################################################
# Pull information from graph

# Load values from graph used directly by drop-downs and maps in the UI

# Metadata list is used in headerLogon_server.R and all search areas

# Hypotheses list is used in the search hypotheses tab

# Domains list is used in the Submit area and in the domain explorer

# Variable Classes list is used in the domain explorer

# Load databases
initDataLoad_Metadata$show()
# domains
if (lsf_cache$exists("lsfdomaincache")){
  # if data is cached load into lsfDomains object
  lsfDomains(lsf_cache$get("lsfdomaincache"))
} else {
  # If data is not in the cache, load it from the database
  lsfDomains(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder))
  # Cache the data
  lsf_cache$set("lsfdomaincache", isolate({lsfDomains()}))  # Cache data
}

if (lsf_cache$exists("lsfhypothesescache")){
  # if data is cached load into lsfHypotheses object
  lsfHypotheses(lsf_cache$get("lsfhypothesescache"))
} else {
  # If data is not in the cache, load it from the database
  lsfHypotheses(neo4r::call_neo4j("MATCH (h:Hypothesis) RETURN h;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
  # Cache the data
  lsf_cache$set("lsfhypothesescache", isolate({lsfHypotheses()}))  # Cache data
}


if (lsf_cache$exists("lsfvariablescache")){
  # if data is cached load into lsfVariableClasses object
  lsfVariableClasses(lsf_cache$get("lsfvariablescache"))
} else {
  # If data is not in the cache, load it from the database
  lsfVariableClasses(neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle))
  # Cache the data
  lsf_cache$set("lsfvariablescache", isolate({lsfVariableClasses()}))  # Cache data
}

if (lsf_cache$exists("lsfmetadatacache")){
  # if data is cached load into lsfMetadata object
  lsfMetadata(lsf_cache$get("lsfmetadatacache"))
} else {
  # If data is not in the cache, load it from the database
  lsfMetadata(sf::st_as_sf(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  # Cache the data
  lsf_cache$set("lsfmetadatacache", isolate({lsfMetadata()}))  # Cache data
}

initDataLoad_Metadata$hide()

initDataLoad_GIS_ICES$show()
ICES_Ecoregions <<- loadFullWKBData("ices_ecoregions_simplified")
initDataLoad_GIS_ICES$hide()
# 
initDataLoad_GIS_NAFO$show()
nafoDivisionsSF <<- loadFullWKBData("nafo_divisions")
initDataLoad_GIS_NAFO$hide()
# 
initDataLoad_GIS_Rivers$show()
riversDBSF <<- loadFullWKBData('rivers_db')
nascoRiversDBSF <<- riversDBSF[riversDBSF$nasco_rivers_db == TRUE,] # subset of rivers that were sourced from the original NASCO DB
indexRiversSF <<- riversDBSF[riversDBSF$ices_index == TRUE,] # subset of rivers that are ICES index rivers
initDataLoad_GIS_Rivers$hide()
# 
initDataLoad_GIS_LSF$show()
salmosalarRange <<- loadFullWKBData('atlantic_salmon_range')
salmosalarExtents <<- sf::st_bbox(salmosalarRange)
initDataLoad_GIS_LSF$hide()