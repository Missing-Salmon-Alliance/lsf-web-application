# Automation script for scynchronising the LSF Central Data Resource with the Knowledge Network for Biocomplexity
# Goals - For each entry in the CDR, create and entry in the KNB and maintain a sync between updates and UUID's
# Restrictions:
#   - Try to be mindful of KNB's version control system which creates a new UUID each time an object is updated
#   - 
require(dataone)# interact with knb/dataone datasets
require(stringr)# used to interact with dataone/knb API
require(neo4r)
library(datapack) # used for creating datapackages to upload to KNB
library(uuid) # used to generate new PackageId for datapackages
library(xml2) # used for manipulating EML xml files

source("./src/secrets.R",local = TRUE)

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
                                user = NEO4J_USER,
                                password = NEO4J_PASSWD)

lsfMetadata <- neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all')
lsfMetadata$metadataUUID <- paste0('urn:uuid:',lsfMetadata$metadataUUID) # add string to match KNB format
## DATAONE CODE REQUIRED FOR FUTURE USE
# FROM GLOBAL.R
#DataONE settings
#SET ENVIRONMENT
#Production
cn <- dataone::CNode("PROD")
mn <- dataone::getMNode(cn, "urn:node:KNB")
d1c <- dataone::D1Client(cn, mn)
#Staging
cn <- dataone::CNode("STAGING2")
mn <- dataone::getMNode(cn, "urn:node:mnTestKNB") # note command 'unlist(lapply(listNodes(cn), function(x) x@subject))' from dataone-federation vignette
d1c <- dataone::D1Client(cn, mn)
# check auth
am <- dataone::AuthenticationManager()
#
# FROM infoPopOvers_server.R
shinyBS::addPopover(session, id = 'sourceKNBURIUI',title = "Enter a KNB URN or DOI",
                    content = HTML(paste0(em("If the data you would like to register is already within the "),a(href="https://knb.ecoinformatics.org/","Knowledge Network for Biocomplexity"),em(" (KNB) then this can be loaded into the form via the URN or DOI of the KNB data package."),
                                          p(),em("This will automatically populate the fields within 'Data Source Details', 'Temporal Coverage' and 'Geographic Coverage', as far as the equivalent information exists in KNB."),
                                          p(),em("For example enter a URN:UUID:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx or DOI:10.5063/xxxxxx and press 'Load...'"),
                                          p(),em("If the KNB object is set to private, you may use your KNB access token below."))),
                    placement = 'top',
                    options = list(container = "body")
)
#
# FROM submitDataSource_Server.R
# define all EML nodes relevant to our metadata
emlNodes <- list(title = "/eml:eml/dataset/title",
                 altURL = "/eml:eml/dataset/alternateIdentifier",
                 abstract = "/eml:eml/dataset/abstract/para",
                 pubDate = "/eml:eml/dataset/pubDate",
                 rights = "/eml:eml/dataset/intellectualRights/para",
                 metaProviderGivenName = "/eml:eml/dataset/metadataProvider/individualName/givenName",
                 metaProviderSurName = "/eml:eml/dataset/metadataProvider/individualName/surName",
                 metaProvidereMail = "/eml:eml/dataset/metadataProvider/electronicMailAddress",
                 metaProviderUserID = "/eml:eml/dataset/metadataProvider/userId",
                 metaProviderOrg = "/eml:eml/dataset/metadataProvider/organizationName",
                 creatorGivenName = "/eml:eml/dataset/creator/individualName/givenName",
                 creatorSurName = "/eml:eml/dataset/creator/individualName/surName",
                 creatoreMail = "/eml:eml/dataset/creator/electronicMailAddress",
                 creatorUserID = "/eml:eml/dataset/creator/userId",
                 creatorOrg = "/eml:eml/dataset/creator/organizationName",
                 geogDescription = "/eml:eml/dataset/coverage/geographicCoverage/geographicDescription",
                 geogWest = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate",
                 geogEast = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate",
                 geogNorth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate",
                 geogSouth = "/eml:eml/dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate",
                 dateStart = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/beginDate/calendarDate",
                 dateEnd = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates/endDate/calendarDate",
                 taxonDescription = "/eml:eml/dataset/coverage/taxonomicCoverage/generalTaxonomicCoverage",
                 taxonRankName = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankName",
                 taxonRankValue = "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification/taxonRankValue",
                 maintDescription = "/eml:eml/dataset/maintenance/description",
                 maintFrequency = "/eml:eml/dataset/maintenance/maintenanceUpdateFrequency",
                 contactID = "/eml:eml/dataset/contact/references",
                 keywordSet = "/eml:eml/dataset/keywordSet/keyword")
##
## Observe event refreshKNBToken actionButton
## pop-up to capture user input new KNB token
## updates token_info reactiveVal
observeEvent(input$refreshKNBToken,
             {
               showModal(modalDialog(title = "Refresh KNB Token",
                                     textInput('KNBTokenValue', "Token"),
                                     easyClose = FALSE, footer = tagList(
                                       modalButton("Cancel"),
                                       actionButton('submitKNBToken', "Submit"))
               ))


             })

observeEvent(input$submitKNBToken, {

  options(dataone_token = input$KNBTokenValue)
  # if no token available, create dummy info data.frame
  tryCatch(token_info(dataone::getTokenInfo(am)),error = function(e){token_info(data.frame(expired=TRUE))})
  removeModal()
})
##
##

# Display KNB Token
output$expiryDatetimeKNBToken <- renderText({
  req(token_info())
  if(token_info()$expired){
    infoOut <- 'Token Expired or Not Valid'

  }else{
    infoOut <- 'Token Validated!'
  }
  infoOut
})

#output$expiryDatetimeKNBToken <- DT::renderDT(token_info())

# Observer for actionButton Load From KNB
observeEvent(input$loadKNB,{

  # Load information from KNB based on UUID
  # Pull data via xml
  # Check for: EMPTY STRING (bad) - ELSE modal invalid UUID
  if(input$sourceKNBURI != ""){
    # Check for: getObject error (bad) - Set sessionXML(NULL)
    tryCatch(sessionXML(dataone::getObject(d1c@mn,input$sourceKNBURI)),error = function(e){sessionXML(NULL)})
    # Check for: sessionXML(NULL) (bad) - ELSE modal invalid UUID OR invalid KNB Token
    if(!is.null(sessionXML())){
      xml_doc <- xml2::read_xml(sessionXML())

      # Update the text areas with results
      updateTextAreaInput(session,inputId = 'sourceTitle',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$title)))
      updateTextInput(session,inputId = 'sourceCreator',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorSurName)))
      updateTextInput(session,inputId = 'sourceOrganisation',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorOrg)))
      updateTextAreaInput(session,inputId = 'sourceAbstract',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$abstract)))
      updateTextInput(session,inputId = 'sourceCreatorEmail',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatoreMail)))
      updateTextInput(session,inputId = 'sourceURI',value = paste0("https://knb.ecoinformatics.org/view/",input$sourceKNBURI)) #TODO Improve this population
      updateTextInput(session,inputId = 'sourceCreatorORCID',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$creatorUserID)))
      updateTextInput(session,inputId = 'sourceALTURI',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$altURL)))

      updateNumericInput(session,inputId = 'sourceStartYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateStart)), format = "%Y")))
      updateNumericInput(session,inputId = 'sourceEndYear',value = as.integer(format(xml_text(xml2::xml_find_first(xml_doc, emlNodes$dateEnd)), format = "%Y")))

      updateTextAreaInput(session,inputId = 'sourceGeographicDescription',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogDescription)))
      updateNumericInput(session,inputId = 'submitNorth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogNorth)))
      updateNumericInput(session,inputId = 'submitEast',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogEast)))
      updateNumericInput(session,inputId = 'submitSouth',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogSouth)))
      updateNumericInput(session,inputId = 'submitWest',value = xml_text(xml2::xml_find_first(xml_doc, emlNodes$geogWest)))


      # set session UUID to match the KNB URI/DOI
      # TODO: This will create a mismatch between non-KNB and KNB sourced metadata as the ID will start with DOI: or URN:UUID:
      # TODO: How to stop this being overwritten at the start of the actionButton submitNewDataSource process
      sessionUUID(input$sourceKNBURI)
      # set sessionXML to NULL to avoid spill over to new load
      sessionXML(NULL)
    }else{
      #TODO: regex to detect correct UUID or DOI
      showModal(modalDialog(
        title = "Please check KNB ID or KNB Token expiry, the supplied ID returned no KNB object.",
        sessionUUID(),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  }else{
    showModal(modalDialog(
      title = "Invalid URI or Blank string",
      easyClose = TRUE,
      footer = NULL
    ))
  }

})
#

# Query KNB for LSF data sets
# Uses the fact that I am a rightsholder for all of them and so searches on my ORCID which is not ideal
queryParamList <- list(q="rightsHolder:http\\:\\/\\/orcid.org\\/0000\\-0003\\-1023\\-4700", rows="1000",fl="id,title,archived,obsoletedBy")
result <- dataone::query(mn,solrQuery=queryParamList,as="data.frame")
# remove obsoleted entries
result <- dplyr::filter(result,is.na(obsoletedBy))
# remove those with no title (TODO: WHAT Are they?)
result <- dplyr::filter(result,!is.na(title))
result <-  tibble::as_tibble(result)

# Compare KNB objects with LSF CDR Objects by UUID
# Gotchas: Check CDR for duplicates and fix
lsfMetadata[duplicated(lsfMetadata$metadataUUID),]
# Check KNB for existing UUID's that are not on LSF, this should not happen so is probably a UUID typo/mistake
onKNBbutnotLSF <- dplyr::setdiff(result$id,lsfMetadata$metadataUUID)
# Create list of new entries
onLSFbutnotKNB <- dplyr::setdiff(lsfMetadata$metadataUUID,result$id)
newEntries <- lsfMetadata[!(lsfMetadata$metadataUUID %in% result$id),]

# Pass new entries to KNB
# Create EML files from LSF new entries
# Use dictionary to translate CDR metadata node properties to EML nodes
emlNodes <- list(metadataTitle = "/eml:eml/dataset/title",
                 metadataAltURI = "/eml:eml/dataset/alternateIdentifier",
                 metadataAbstract = "/eml:eml/dataset/abstract/para",
                 metadataTimestamp = "/eml:eml/dataset/pubDate",
                 metadataCreator = "/eml:eml/dataset/creator/individualName/givenName",
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
                 # the following are not kept within the CDR metadata and at present at static or manual values
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
                 contactID = "/eml:eml/dataset/contact/references",
                 keywordSet = "/eml:eml/dataset/keywordSet/keyword")
# Create data packages from EML files and upload

# FUNCTION: This script helps create and import multiple metadata descriptions on to KNB and in turn MSA DataONE portal
# INPUT: csv of metadata
# INPUT: DataONE EML Template.xml
# OUTPUT: eml.xml file with new unique ID incorporated into filename
# OUTPUT: create datapackage and upload to KNB

# REQUIRED FILES: Fresh metadata csv file ("KNBTranslations_For_R.csv") from Data Sources folder on AST drive, based on the template

# TODO: There is no duplication check at the moment, adding a search query to KNB for each row before file creation to flag potential duplicate
# TODO: Incorporate Essential Salmon Variables into Keywords

#IMPORT METADATA as TIBBLE

## Check for NA values, they look rubbish on KNB, replace with empty string
# Needs some manual input if any columns as set to lgl change them to chr
# Note a way to do this is if all values in lgl column are known to be NA, just replace whole column manually with ""
# NOTE Some eml fields must be non-empty string if they exist and so "NA" is most appropriate
# Sample Code
# y$`coverage/taxonomicCoverage/generalTaxonomicCoverage` <- c("Non-Specific")
# y$`coverage/taxonomicCoverage/taxonomicClassification/taxonRankName` <- c("NA")
# y$`coverage/taxonomicCoverage/taxonomicClassification/taxonRankValue` <- c("NA")
# y$`creator/userId` <- c("") # UserID is OK with empty strings

# This line will replace any remaining NA value with empty string once lgl columns are dealt with
y <- y %>% replace(is.na(.), "")

#Capture keywords in separate tibble
y_keywords <- y[, c("commonKeywords","additionalKeywords")]
# and remove them from the main tibble
y <- select(y, -c("commonKeywords","additionalKeywords"))

#IMPORT EML Template
x <- read_xml("DataONE EML Template.xml")

#SET BASE XML NODE PATH
xpath_base <- "/eml:eml/dataset/"

# Create report tibble with UUID, KNB URL and Dataset Title

report <- tibble::tibble(uuid = character(),knbURI = character(), datasetTitle = character())


###
#Main FOR loop - FOR EACH TIBBLE ROW: Set new xml node values based on row, export to file and create/upload knb datapackage from file
###
for(i in 1:nrow(y)){ #rows ROWS CONTAIN NODE VALUES
  
  # REMOVE ALL KEYWORDS BEFORE MOVING TO NEXT ROW - AT BEGINNING OF ROUTINE TO CATCH AND REMOVE TEMPLATE KEYWORD
  xml_remove(xml_find_all(x, "//keyword"))
  
  for(z in 1:ncol(y)){ #cols COL NAMES ARE NODE PATH (TAIL only)
    xpath_tail <- names(y[z])
    xpath_full <- paste(xpath_base,xpath_tail,sep = "")
    node_value <- toString(y[i,z])
    xml_set_text(xml_find_all(x, xpath = xpath_full,xml_ns(x)),node_value)
  }
  # keyword routine
  # Create keyword_vector of all keywords for ROW (TWO COLUMNS of KEYWORDS)
  # COLUMN 1
  commonKeywords_vector <- str_trim(str_split(y_keywords$commonKeywords[i],",")[[1]])
  # COLUMN 2
  additionalKeywords_vector <- str_trim(str_split(y_keywords$additionalKeywords[i],",")[[1]])
  # MERGE into keyword_vector
  keyword_vector <- c(commonKeywords_vector,additionalKeywords_vector)
  
  #CREATE keyword NODES, add the same number as there are keywords (length of keyword_vector)
  for(kw in 1:length(keyword_vector)){
    xml_add_child(xml_find_all(x, xpath = "/eml:eml/dataset/keywordSet", xml_ns(x)), "keyword") # dummy initial value "keyword"
  }
  # APPLY keyword values to new nodes
  xml_find_all(x, "//keyword") %>% xml_set_text(keyword_vector)
  
  ### Valid EML xml object has now been created
  ### It will now be given a unique ID, exported to a file which will then be used to create KNB datapackage
  
  # Create and Set PID
  id_part <- UUIDgenerate()
  id <- paste("urn:uuid:", id_part, sep="")
  xml_set_attr(x, "packageId", id)
  
  # set metadataProvider node id attribute so that CONTACT/REFERENCES node works
  xml_set_attr(xml_find_all(x, "//metadataProvider"),"id",y[i,]$`metadataProvider/userId`)
  # export to xml file, with custom file name based on generated uuid
  filename <- paste("eml/eml_",id_part,".xml",sep = "")
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
  
  report <- dplyr::bind_rows(report,tibble::tibble(uuid = id_part,knbURI = paste0("https://knb.ecoinformatics.org/view/urn:uuid:",id_part), datasetTitle = y$title[i]))
}

write_csv(report,paste0("report_",Sys.Date()))


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

