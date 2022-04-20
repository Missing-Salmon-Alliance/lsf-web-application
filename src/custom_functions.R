######################
# General functions used throughout the app
######################
# required to capture checkboxGroupInput output cleanly, as a comma sep string
formatLSFCategories <- function(categories){
  x <- paste(categories,collapse = ", ")
  return(x)
}
# SQL GIS Loader function

loadFullWKBData <- function(tblname) {
  
  con <- DBI::dbConnect(RPostgres::Postgres(),dbname = "gisdb", 
                   host = PSQL_HOST,
                   port = PSQL_PORT,
                   user = PSQL_USER,
                   password = PSQL_PASSWD)
  res <- DBI::dbSendQuery(con, paste("SELECT * FROM ",tblname," ORDER BY ogc_fid;",sep=''))
  
  result <- DBI::dbFetch(res)
  
  DBI::dbClearResult(res)
  DBI::dbDisconnect(con)
  
  result$wkb_geometry <- sf::st_as_sfc(result$wkb_geometry)
  result <- sf::st_as_sf(result,crs = 4326)
  
  return(result)
  
}

# Sanitise User Free Text Inputs Function

sanitiseFreeTextInputs <- function(userInput){
  safeString <- stringr::str_replace_all(string = userInput,pattern = "'",replacement = "_") # remove single quotes
  safeString <- stringr::str_replace_all(string = safeString,pattern = '"',replacement = "_") # remove double quotes
  return(safeString)
}

#############################################
# NEO4J USER FUNCTIONS
checkUserCredentials <- function(user,pw){
  # function that takes user password, connects to database and tests it
  # TODO: password is entered, encrypted with public key and sent to database
  # TODO: database is encrypted with same public key, private key is used server side (sql server side) to decrypt both user password and
  # database. decrypted passwords are compared and if TRUE user is authorised to view the application
  # TODO: Affiliation is a property of the Person node, it would be great to make this a relationship to an organisation node
  # some code below does capture this relationship if it exists but it is not used yet
  
  # connect to neo4j and return person node that matches both username and password combination
  # return Person node and ALL directly connected nodes (e.g. requested metadata, submitted metadata and affiliated organisation)
  res <- neo4r::call_neo4j(paste0("MATCH (p:Person{personEmail:'",sanitiseFreeTextInputs(user),
                           "',personPassword:'",sanitiseFreeTextInputs(pw),
                           "'})-[r]-(x) RETURN p,r,x;"),
                    neo_con,
                    type = 'graph')
  
  # use purrr to confirm if the res is not empty
  # if not empty carry out some wrangling procedures to pull out user info and relationships
  if(!purrr::is_empty(res)){
    results <- neo4r::unnest_graph(res)
    result <- results$nodes[results$nodes$value == "Person",] %>% select(matches("^(id|person*)"))
    requested <- results$relationships[results$relationships$type == "HAS_REQUESTED",]$endNode
    submitted <- results$relationships[results$relationships$type == "HAS_SUBMITTED",]$endNode
    affiliation <- results$relationships[results$relationships$type == "REPRESENTS",]$endNode
    # web app has a Account Information section that shows user history of metadata requests and submissions
    # if user has no requested metadata yet pass in an empty tibble
    if(length(requested > 0)){
      requestedTibble = results$nodes[results$nodes$id %in% requested,]
    }else{requestedTibble = tibble(metadataTitle = character(),metadataAbstract = character())}
    # if user has no submitted metadata yet pass in an empty tibble
    if(length(submitted > 0)){
      submittedTibble = results$nodes[results$nodes$id %in% submitted,]
    }else{submittedTibble = tibble(metadataTitle = character(),metadataAbstract = character())}
    # this is a quick double check that only 1 user was returned
    # TODO: add some error feedback if this is not true
    if(nrow(result) == 1){
      # build a list to return to reactive user_info value
      auth <- list(
        result = TRUE,
        admin = result$personAdmin,
        user_info = list(id = result$id, fullname = result$personName,email = result$personEmail,affiliation = result$personAffiliation,
                         requested = requestedTibble,
                         submitted = submittedTibble,
                         bookmarks = result$personBookmarks))
    }else{
      # build a dummy NULL list to return to reactive user_info value if the above conditions are false
      auth <- list(
        result = FALSE,
        admin = FALSE,
        user_info = NULL)
    }
  }else{
    # build a dummy NULL list to return to reactive user_info value if the above conditions are false
    auth <- list(
      result = FALSE,
      admin = FALSE,
      user_info = NULL)
  }
  
  return(auth)
  
}

# Admin area create new user function
adminCreateNewUser <- function(fullname,pw,email,affiliation,acceptDSA,promoteORG,admin){
  # Create a new user in the database
  # TODO: work out how to get rid of workaroundnode, this is used because check credentials function requires a node returned with at least 1 relationship
  # TODO: user feedback success/failure
  res <- neo4r::call_neo4j(paste0("MATCH (w:WorkaroundNode) CREATE (p:Person{created:'",Sys.time(),"',personName:'",fullname,
                    "',personPassword:'",pw,
                    "',personEmail:'",email,
                    "',personAffiliation:'",affiliation,
                    "',personAcceptDSA:",acceptDSA,
                    ",personPromoteOrg:",promoteORG,
                    ",personAdmin:",admin,
                    ",personBookmarks:'", # empty on initial creation
                    "'})-[:NEWUSER]->(w)"),
                    neo_con,
                    type = 'row')
  
}



# save data to local csv function
# https://shiny.rstudio.com/articles/persistent-data-storage.html
saveData <- function(activity,person,theme,focus) {
  conn <- neo4r::neo4j_api$new(url = paste0("http://",NEO4J_HOST,":",NEO4J_PORT),user = NEO4J_USER,password = NEO4J_PASSWD)
  # process user submitted data in to graph
  # create new Activity node and pass in properties
  newActivityQuery <- paste0("CREATE ",
                             neo4r::vec_to_cypher_with_var(as.list(activity),'Activity','a'),
                             " SET a.activityLabel = 'temporaryLabel', a.activityCreated = '",
                             format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                             "';")
  neo4r::call_neo4j(newActivityQuery,conn,type='row')
  ####
  # create relationships to new Activity node and existing themes
  for(i in theme){
    query <- stringr::str_replace("MATCH (a:Activity{activityLabel:'temporaryLabel'}),(t:Theme{themeLabel:'xyz'}) CREATE (a)-[:HAS_THEME]->(t);",
                                  "xyz",
                                  i)
    neo4r::call_neo4j(query,conn,type='row')
  }
  ####
  # create relationships to new Activity node and existing focus
  for(i in focus){
    query <- stringr::str_replace("MATCH (a:Activity{activityLabel:'temporaryLabel'}),(f:Focus{focusLabel:'xyz'}) CREATE (a)-[:HAS_FOCUS]->(f);",
                                  "xyz",
                                  i)
    neo4r::call_neo4j(query,conn,type='row')
  }
  ####
  # create new person (will create duplicates but these can be cleaned up by a human later)
  newPersonQuery <- paste0("CREATE ",
                           neo4r::vec_to_cypher_with_var(as.list(person),'Person','p'),
                           " SET p.personSource = 'researchInv', p.created = '",
                           format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                           "', p.personNew = true;")
  neo4r::call_neo4j(newPersonQuery,conn,type='row')
  # relate person and activity AND remove unique references activityLabel and personNew properties
  neo4r::call_neo4j("MATCH (p:Person{personNew:true}),(a:Activity{activityLabel:'temporaryLabel'}) CREATE (a)-[:HAS_PERSON]->(p) SET a.activityLabel = '' REMOVE p.personNew;",
                    conn,
                    type = 'row')
  
  # close connection
  rm(conn)
}

# load data from local csv function
# https://shiny.rstudio.com/articles/persistent-data-storage.html
loadData <- function() {
  
  conn <- neo4r::neo4j_api$new(url = paste0("http://",NEO4J_HOST,":",NEO4J_PORT),user = NEO4J_USER,password = NEO4J_PASSWD)
  
  dataDF <- neo4r::call_neo4j("MATCH (a:Activity) RETURN a;",conn,type='row')$a
  
  return(dataDF)
  
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

#######################################################################
# Functions no longer in use
#######################################################################

# loadgeoJSONData <- function(tblname,nameColumn) {
#   
#   con <- DBI::dbConnect(RPostgres::Postgres(),dbname = "gisdb", 
#                         host = PSQL_HOST,
#                         port = PSQL_PORT,
#                         user = PSQL_USER,
#                         password = PSQL_PASSWD)
#   res <- DBI::dbSendQuery(con, paste("SELECT ogc_fid,",nameColumn,",geojson FROM ",tblname," ORDER BY ogc_fid;",sep=''))
#   result <- DBI::dbFetch(res)
#   DBI::dbClearResult(res)
#   DBI::dbDisconnect(con)
#   return(result)
#   
# }
# 
# convertToGeojsonFeatureCollection <- function(data,name){
#   
#   #Takes a list of geojson features (for example the geojson column from a tibble) and converts them into a single
#   #gosJSON feature collection.
#   #INPUT: geom, list of geojson features
#   #OUTPUT: string, single string geoJSON feature collection
#   ## USE CASE: Data from PostgreSQL database is in WKB format, easily converted to GeoJSON but as a list of single features
#   ## Leaflet takes in the geoJSON featureCollection easily as one object
#   ## TODO: Encapsulate information from other columns into properties for each feature
#   featureCollectionHead <- '{"type":"FeatureCollection","features":['
#   featureCollectionTail <- ']}'
#   
#   data$featureCollectionFeatures <- paste('{"type":"Feature","properties":{"id":"',data$ogc_fid,'","name":"',data[[name]],'"},"geometry":',data$geojson,'}',sep = "")
#   
#   featureCollectionBody <- paste0(data$featureCollectionFeatures,collapse = ',')
#   
#   featureCollection <- paste0(featureCollectionHead,featureCollectionBody,featureCollectionTail)
#   
#   
#   return(featureCollection)
# }