# Automation script for synchronizing the LSF Central Data Resource with the Knowledge Network for Biocomplexity
# Goals - For each entry in the CDR, create an entry in the KNB and maintain a sync between updates and UUID's
# Gotchas:
#   - Try to be mindful of KNB's version control system which creates a new UUID each time an object is updated
#   - 

#############
# libraries
#############

require(dataone)# interact with knb/dataone datasets
require(stringr)# used to interact with dataone/knb API
require(neo4r)
library(datapack) # used for creating datapackages to upload to KNB
library(uuid) # used to generate new PackageId for datapackages
library(xml2) # used for manipulating EML xml files

#################
# Variables setup
#################

source("./src/secrets.R",local = TRUE)

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
                                user = NEO4J_USER,
                                password = NEO4J_PASSWD)

lsfMetadata <- neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all')
lsfMetadata$metadataUUID <- paste0('urn:uuid:',lsfMetadata$metadataUUID) # add string to match KNB format

#DataONE settings
#SET ENVIRONMENT
#Production
cn <- dataone::CNode("PROD")
mn <- dataone::getMNode(cn, "urn:node:KNB")
d1c <- dataone::D1Client(cn, mn)

#Staging
# cn <- dataone::CNode("STAGING2")
# mn <- dataone::getMNode(cn, "urn:node:mnTestKNB") # note command 'unlist(lapply(listNodes(cn), function(x) x@subject))' from dataone-federation vignette
# d1c <- dataone::D1Client(cn, mn)

# check Dataone Authentication Token (manual update required from KNB profile page get R token script)
#am <- dataone::AuthenticationManager()
#dataone::getTokenInfo(am)

#############################
# Query KNB for LSF data sets
#############################
# Uses the fact that I am a rights holder for all of them and so searches on my ORCID which is not ideal
queryParamList <- list(q="rightsHolder:http\\:\\/\\/orcid.org\\/0000\\-0003\\-1023\\-4700", rows="1000",fl="id,title,archived,obsoletedBy")
result <- dataone::query(mn,solrQuery=queryParamList,as="data.frame")
# remove obsoleted entries
result <- dplyr::filter(result,is.na(obsoletedBy))
# remove those with no title (TODO: WHAT Are they?)
result <- dplyr::filter(result,!is.na(title))
result <-  tibble::as_tibble(result)

#############
# Compare KNB objects with LSF CDR Objects by UUID
#############
# Gotchas: Check CDR for duplicates and fix
lsfMetadata[duplicated(lsfMetadata$metadataUUID),]
# Check KNB for existing UUID's that are not on LSF, this should not happen so is probably a UUID typo/mistake
onKNBbutnotLSF <- dplyr::setdiff(result$id,lsfMetadata$metadataUUID)
# Create list of new entries
onLSFbutnotKNB <- dplyr::setdiff(lsfMetadata$metadataUUID,result$id)
newEntries <- lsfMetadata[!(lsfMetadata$metadataUUID %in% result$id),]

####################
# Create EML files from CDR entries
####################

# This requires translation of CDR fields to EML fields
# Use dictionary to translate CDR metadata node properties to EML nodes
emlNodes <- list(metadataTitle = "/eml:eml/dataset/title",
                 metadataAltURI = "/eml:eml/dataset/alternateIdentifier",
                 metadataAbstract = "/eml:eml/dataset/abstract/para",
                 metadataTimestamp = "/eml:eml/dataset/pubDate",
                 metadataCreator = "/eml:eml/dataset/creator/individualName/surName",
                 metadataCreatorEmail = "/eml:eml/dataset/creator/electronicMailAddress",
                 metadataCreatorORCID = "/eml:eml/dataset/creator/userId",
                 metadataOrganisation = "/eml:eml/dataset/creator/organizationName",
                 metadataGeographicDescription = "/eml:eml/dataset/coverage/geographicCoverage/geographicDescription",
                 metadataCoverageWest = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate",
                 metadataCoverageEast = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate",
                 metadataCoverageNorth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate",
                 metadataCoverageSouth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate",
                 metadataCoverageStartYear = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/beginDate/calendarDate",
                 metadataCoverageEndYear = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/endDate/calendarDate",
                 metadataMaintenance = "/eml:eml/dataset/maintenance/maintenanceUpdateFrequency",
                 metadataKeywords = "/eml:eml/dataset/keywordSet/keyword",
                 # the following are not kept within the CDR metadata and at present at static or manual values
                 givenName = "/eml:eml/dataset/creator/individualName/givenName", # CDR does not have first.last names separated
                 rights = "/eml:eml/dataset/intellectualRights/para",
                 metaProviderGivenName = "/eml:eml/dataset/metadataProvider/individualName/givenName",
                 metaProviderSurName = "/eml:eml/dataset/metadataProvider/individualName/surName",
                 metaProvidereMail = "/eml:eml/dataset/metadataProvider/electronicMailAddress",
                 metaProviderUserID = "/eml:eml/dataset/metadataProvider/userId",
                 metaProviderOrg = "/eml:eml/dataset/metadataProvider/organizationName",
                 taxonDescription = "/eml:eml/dataset/coverage/taxonomicCoverage/generalTaxonomicCoverage",
                 taxonRankName = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankName",
                 taxonRankValue = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankValue",
                 maintDescription = "/eml:eml/dataset/maintenance/description",
                 contactID = "/eml:eml/dataset/contact/references")


# Add static or manual values to CDR tibble
newEntries$givenName = ""
newEntries$rights = "This work is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/."
newEntries$metaProviderGivenName = "Graeme"
newEntries$metaProviderSurName = "Diack"
newEntries$metaProvidereMail = "graeme@atlanticsalmontrust.org"
newEntries$metaProviderUserID = "https://orcid.org/0000-0003-1023-4700"
newEntries$metaProviderOrg = "Atlantic Salmon Trust"
newEntries$taxonDescription = "Data relates to specific target species (prey, predator, competition or environment)"
newEntries$taxonRankName = "Species"
newEntries$taxonRankValue = "Salmo salar"
newEntries$maintDescription = ""
newEntries$contactID = "https://orcid.org/0000-0003-1023-4700"


# Pass new entries to KNB
# Create EML files from LSF new entries

# Create data packages from EML files and upload
# FUNCTION: The following FOR loop creates and imports multiple metadata descriptions on to KNB and in turn MSA DataONE portal
# INPUT: newEntries tibble (Extract from CDR with existing KNB objects removed)
# INPUT: DataONE EML Template.xml
# OUTPUT: eml.xml file with unique ID incorporated into filename
# OUTPUT: create datapackage and upload to KNB

# REQUIRED FILES: DataONE EML Template.xml

# TODO: There is no duplication check at the moment, adding a search query to KNB for each row before file creation to flag potential duplicate
# TODO: Incorporate Essential Salmon Variables into Keywords

## Check for NA values, they look rubbish on KNB, replace with empty string
# Needs some manual input if any columns as set to lgl change them to chr
# Note a way to do this is if all values in lgl column are known to be NA, just replace whole column manually with ""
# NOTE Some EML fields must be non-empty string if they exist
# Sample Code
# newEntries$metadataGeographicDescription <- c("Atlantic Salmon Commonly Accepted Range")
# newEntries$taxonDescription <- c("Non-Specific")
# newEntries$taxonRankName <- c("NA")
# newEntries$taxonRankValue <- c("NA")
# newEntries$metadataCreatorORCID <- c("") # UserID is OK with empty strings

# This line will replace any remaining NA value with empty string once lgl columns are dealt with
newEntries <- newEntries %>% replace(is.na(.), "")

#Capture keywords in separate tibble
newEntries_keywords <- newEntries[, c("metadataKeywords")]
# and remove them from the main tibble
newEntries <- dplyr::select(newEntries, -c("metadataKeywords"))

#IMPORT EML Template
x <- read_xml("./src/automation/DataONE EML Template.xml")

# Create report tibble with UUID, KNB URL and Dataset Title

report <- tibble::tibble(uuid = character(),knbURI = character(), datasetTitle = character())


###
#Main FOR loop - FOR EACH TIBBLE ROW: Set new xml node values based on row, export to file and create/upload knb datapackage from file
###
for(i in 1:nrow(newEntries)){ #rows ROWS CONTAIN NODE VALUES
  
  # REMOVE ALL KEYWORDS BEFORE MOVING TO NEXT ROW - AT BEGINNING OF ROUTINE TO CATCH AND REMOVE TEMPLATE KEYWORD
  xml_remove(xml_find_all(x, "//keyword"))
  
  for(z in 1:length(emlNodes)){ #EML Dictionary information
    xpath_full <- emlNodes[[z]] #dictionary value
    node_value <- toString(newEntries[[i,names(emlNodes)[z]]]) # cdr field value
    xml_set_text(xml_find_all(x, xpath = xpath_full,xml_ns(x)),node_value)
  }
  # timestamp to pubDate fixup, reduce timestamp to date CCYY-MM-DD only, remove time
  xml_set_text(xml_find_all(x, xpath = "/eml:eml/dataset/pubDate",xml_ns(x)),stringr::str_trunc(newEntries$metadataTimestamp[i],10,'right',ellipsis = ''))
  # keyword routine
  # Create keyword_vector of all keywords for ROW
  keyword_vector <- str_trim(str_split(newEntries_keywords$metadataKeywords[i],",")[[1]])
  
  #CREATE keyword NODES, add the same number as there are keywords (length of keyword_vector)
  for(kw in 1:length(keyword_vector)){
    xml_add_child(xml_find_all(x, xpath = "/eml:eml/dataset/keywordSet", xml_ns(x)), "keyword") # dummy initial value "keyword"
  }
  # APPLY keyword values to new nodes
  xml_find_all(x, "//keyword") %>% xml_set_text(keyword_vector)
  
  ### Valid EML xml object has now been created
  ### It will now be given a unique ID, exported to a file which will then be used to create KNB datapackage
  
  # Create and Set PID
  id <- newEntries$metadataUUID[i]
  id_part <- stringr::str_split(id,pattern = ":",simplify = TRUE)[3] # use for filename, remove colons
  xml_set_attr(x, "packageId", id)
  
  # set metadataProvider node id attribute so that CONTACT/REFERENCES node works
  xml_set_attr(xml_find_all(x, "//metadataProvider"),"id",newEntries[i,]$metaProviderUserID)
  # export to xml file, with custom file name based on generated uuid
  filename <- paste("./src/automation/eml/eml_",id_part,".xml",sep = "")
  write_xml(x,filename)
  
  # File creation complete
  
  # Create KNB Datapackage and upload
  
  dp <- new("DataPackage")
  #x <- read_xml(filename)
  id <- xml_attr(x, "packageId")
  
  metadataObj <- new("DataObject", id, format="https://eml.ecoinformatics.org/eml-2.2.0", filename=filename)
  dp <- addMember(dp, metadataObj)
  
  # with a metadata object added the package can now be uploaded
  
  packageId <- uploadDataPackage(d1c, dp, public=FALSE, quiet=FALSE)
  
  ###
  # Create upload report tibble
  
  report <- dplyr::bind_rows(report,tibble::tibble(uuid = id_part,knbURI = paste0("https://knb.ecoinformatics.org/view/urn:uuid:",id_part), datasetTitle = newEntries$metadataTitle[i]))
}

readr::write_csv(report,paste0("./src/automation/eml/report_",Sys.Date()))


######################
# Sync info between existing CDR/KNB objects
######################
# grab relevant info from KNB (set up to capture Keywords at the moment but this could be anything)
keywordsVEC <- list()
for(i in lsfMetadata$metadataUUID){
  if(!(i %in% onLSFbutnotKNB)){
    knbObject <- getObject(d1c@mn,i)
    keywords <- xml_text(xml_find_all(read_xml(knbObject), "/eml:eml/dataset/keywordSet/keyword"))
    keywordsVEC[[i]] <- keywords
  }
}

# write changes to LSF
for(i in names(keywordsVEC)){
  query <- paste0("MATCH (m:Metadata) WHERE m.metadataUUID CONTAINS '",i,"' SET m.metadataKeywords = '",paste(keywordsVEC[[i]],collapse = ","),"';")
  result <- call_neo4j(query,neo_con,type='row')
  #print(query)
}

