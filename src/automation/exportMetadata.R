#Export Metadata Parameters

export <- neo4r::call_neo4j("MATCH (m:Metadata) WHERE m.metadataKeywords CONTAINS 'WKSALMON' RETURN m;",neo_con,type = 'graph')
export <- neo4r::call_neo4j("MATCH (m:Metadata) WHERE m.metadataTimestamp > '2022-05-19' RETURN m;",neo_con,type = 'graph')
export <- neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type = 'graph')

#Transform
export <- neo4r::unnest_nodes(export$nodes)

export <- export[,c("id","metadataWksalmonSection","metadataTitle","metadataAbstract","metadataKeywords",
                    "metadataOrganisation","metadataCreator","metadataCreatorEmail","metadataCreatorORCID",
                    "metadataMaintenance","metadataCoverageStartYear","metadataCoverageEndYear","metadataCoverageMonthsOfYear",
                    "metadataGeographicDescription","metadataCoverageNorth","metadataCoverageEast","metadataCoverageSouth","metadataCoverageWest","metadataCoverageCentroid",
                    "metadataAccessProtocol","metadataFilename","metadataAvailableOnline","metadataAltURI","metadataKNBURI","metadataUUID","metadataEmbargoed","metadataEmbargoEnd",
                    "metadataQCCheck","metadataTimestamp","metadataLastModified","metadataCoverageIntersectMigrationRoutes","metadataCoverageIntersectNAFODivision","metadataCoverageIntersectICESEcoRegion")]

#Export
readr::write_csv(export,'wksalmonData.csv')
readr::write_csv(export,'CDR_Data.csv')


# Export Variable classes and domains
export <- neo4r::call_neo4j("MATCH (m:Metadata)-[r:HAS_ESV]-(esv:EssentialSalmonVariable) RETURN m,r,esv;",neo_con,type = 'graph')
export <- neo4r::unnest_graph(export)
# transform to extract variable classes with domain name appended as a comma sep string for each metadata node id
# start by joining export$nodes$esvLabel into export$relationships based on endNode+id match

export <- dplyr::left_join(export$relationships,export$nodes[,c('id','esvTitle')],by = c('endNode' = 'id'))
# concatenate domain and esvLabel column
export$esvTitle <- paste(export$domain,export$esvTitle,sep = "_")
export <- dplyr::arrange(export,startNode)
# transform into unique metadata rows with esvTitle condensed into comma sep string
export <- export %>% dplyr::group_by(startNode) %>% dplyr::summarize(variableClasses = paste0(esvTitle, collapse = ", "))


#EXPORT Variable Classes with Class physical, biological and salmon trait
export <- neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type = 'graph')
export <- neo4r::unnest_nodes(export$nodes)
readr::write_csv(export[,c('esvTitle','esvCategory')],'esv_data.csv')


# Export Hypotheses and related metadata
# Grab all the pathways from metadata through ESV's to hypotheses

query <- "MATCH (m:Metadata)-[r:HAS_ESV]-(:EssentialSalmonVariable)
          MATCH (hyp:Hypothesis)
          WHERE (m)-[:HAS_ESV]-(:EssentialSalmonVariable)-[:REQUIRES_ESV]-(:SubHypothesis)-[:HAS_SUBHYPOTHESIS]-(hyp:Hypothesis)
          MATCH (d:Domain) WHERE d.domainTitle = r.domain
          AND d.domainEnvironment = hyp.hypothesisEnvironment
          RETURN DISTINCT[id(m),m.metadataWksalmonSection,hyp.hypothesisTitle,m.metadataTitle,m.metadataAltURI,m.metadataFilename,m.metadataAccessProtocol]
          AS output;"

export <- neo4r::call_neo4j(query,neo_con,type = 'row')
export <- export$output
names(export) <- c("id","metadataWksalmonSection","hypothesisTitle","metadataTitle","metadataAltURI","metadataFilename","metadataAccessProtocol")


readr::write_csv(export,'metadataHypotheses.csv')

#########################

#Load, selected column update

loadData <- readr::read_csv('CDR_Data_IndexRivers_ModifiedForLoad.csv')


for(i in 1:nrow(loadData)){
  id = loadData$id[i]
  title = loadData$metadataTitle[i]
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",id," SET m.metadataTitle = '",title,"';")
  print(query)
  neo4r::call_neo4j(query,neo_con,type = 'row')
}

for(i in 1:nrow(loadData)){
  id = loadData$id[i]
  abstract = loadData$metadataAbstract[i]
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",id," SET m.metadataAbstract = '",abstract,"';")
  print(query)
  neo4r::call_neo4j(query,neo_con,type = 'row')
}

#load, full update

updates <- readr::read_csv('CDR_Data_Updates.csv')
updates <- updates %>% replace(is.na(.),"") # replace NA with ""

for(i in 1:nrow(updates)){
  id = updates$id[i]
  for(x in 2:length(names(updates))){
    query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",id," SET m.",names(updates)[x]," = '",updates[i,names(updates)[x]],"';")
    #print(query)
    neo4r::call_neo4j(query,neo_con,type = 'row')
  }
  
}

