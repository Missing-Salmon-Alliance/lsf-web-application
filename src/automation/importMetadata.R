library(magrittr)
library(neo4j)

input_data <- readr::read_csv("./INSERTCSVNAMEHERE.csv")
stockUnits <- c("LB","NFLD","QB","GF","SF","US","IC.SW","SC.W","SC.E","IR.N","IR","EW","FR","GY","SP","RU","FI","NO","SWD","IC.NE","DK")

uuidlist <- c()
for(i in 1:nrow(input_data)){
  uuid_temp <- uuid::UUIDgenerate()
  uuidlist <- append(uuidlist,uuid_temp)
  query <- paste0("MATCH (p:Person) WHERE id(p) = 582",
        " CREATE (p)-[:HAS_SUBMITTED{created:'",Sys.time(),
        "',lastModified:'",Sys.time(),
        "',status:'startedQC'}]->(:Metadata{metadataTitle:'",input_data[i,]$Title,
        "',metadataCreator:'",input_data[i,]$Creator,
        "',metadataKNBURI:'","",
        "',metadataAltURI:'",input_data[i,]$AltURI,
        "',metadataOrganisation:'",input_data[i,]$Organisation,
        "',metadataAbstract:'",input_data[i,]$Abstract,
        "',metadataAvailableOnline:",input_data[i,]$AvailableOnline,
        ",metadataEmbargoed:",input_data[i,]$Embargoed,
        ",metadataEmbargoEnd:'",input_data[i,]$EmbargoEnd,
        "',metadataAccessProtocol:'",input_data[i,]$AccessProtocol,
        "',metadataStockUnit:'",paste0(stockUnits,collapse = ','), # add all Stock Units to be confirmed at QC
        "',metadataGeographicDescription:'",input_data[i,]$GeographicDescription,
        "',metadataCreatorEmail:'",input_data[i,]$CreatorEmail,
        "',metadataCreatorORCID:'",input_data[i,]$CreatorORCID,
        "',metadataCoverageStartYear:",input_data[i,]$CoverageStartYear,
        ",metadataCoverageEndYear:",input_data[i,]$CoverageEndYear,
        ",metadataCoverageMonthsOfYear:'",input_data[i,]$CoverageMonthsOfYear,#collapse months of year into csv string
        "',metadataCoverageNorth:",input_data[i,]$CoverageNorth,
        ",metadataCoverageSouth:",input_data[i,]$CoverageSouth,
        ",metadataCoverageEast:",input_data[i,]$CoverageEast,
        ",metadataCoverageWest:",input_data[i,]$CoverageWest,
        ",metadataMaintenance:'",input_data[i,]$Maintenance,
        "',metadataQCCheck:",TRUE,
        ",metadataCoverageCentroid:'",paste0("POINT (",input_data[i,]$CoverageNorth," ",input_data[i,]$CoverageEast,")"),
        "',metadataCoverageIntersectICESEcoRegion:'","",
        "',metadataCoverageIntersectNAFODivision:'","",
        "',metadataCoverageIntersectMigrationRoutes:'","",
        "',metadataUUID:'",uuid_temp,
        "',metadataFilename:'",input_data[i,]$Filename,
        "',metadataTimestamp:'",Sys.time(),
        "',metadataKeywords:'",input_data[i,]$Keywords,"'});",sep = "")
  
  neo4r::call_neo4j(query,neo_con,type = 'row',include_stats = T,include_meta = T)
}

#####################
# Create relationships to ESV/Variable Class using DOMAIN SPECIFIC RELATIONSHIPS
#####################
# initiate a vector to capture the queries
queryMasterList <- c()
lsfDomains <- neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder)
lsfVariableClasses <- neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle)
domainNodeList <- c(589,591,593,583)
# build full ESV list for domain (collapse from 3 categories presented in the UI)
variableClassesForDomain <- c("Genetic Sampling","Growth Rate From Scales")
# create base query elements
queryBase <- c("MATCH (esv:EssentialSalmonVariable{esvTitle:'","'}),(md:Metadata{metadataUUID:'","'}) CREATE (esv)<-[:HAS_ESV{domain:'","',domainID:","}]-(md);")
# cycle through all the selected ESV for each domain IS THIS EFFICIENT?

for(uuid_target in uuidlist){
  for(domainID in domainNodeList){
    domainName <- lsfDomains[lsfDomains$id == domainID,]$domainTitle
    # create basic relationship between metadata and domain (capture even if user selects no ESV's)
    addDomainQuery <- paste0("MATCH (d:Domain),(md:Metadata{metadataUUID:'",uuid_target,"'}) WHERE id(d) = ",domainID," CREATE (md)-[:HAS_DOMAIN]->(d);")
    queryMasterList <- append(queryMasterList,addDomainQuery)
    for(esv in variableClassesForDomain){
      # create sub query for single esv-metadata
      querySubMaster <- paste(queryBase[1],esv,queryBase[2],uuid_target,queryBase[3],domainName,queryBase[4],domainID,queryBase[5],sep = "")
      # add sub query to master query list
      queryMasterList <- append(queryMasterList,querySubMaster)
    }
  }
}

for(query in queryMasterList){
  neo4r::call_neo4j(query,neo_con,type = 'row',include_stats = T,include_meta = T)
}

