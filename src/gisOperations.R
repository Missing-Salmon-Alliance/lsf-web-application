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
  result <- subset(result, select = -c(geojson))
  
  return(result)
  
}


loadgeoJSONData <- function(tblname,nameColumn) {
  
  con <- DBI::dbConnect(RPostgres::Postgres(),dbname = "gisdb", 
                   host = PSQL_HOST,
                   port = PSQL_PORT,
                   user = PSQL_USER,
                   password = PSQL_PASSWD)
  res <- DBI::dbSendQuery(con, paste("SELECT ogc_fid,",nameColumn,",geojson FROM ",tblname," ORDER BY ogc_fid;",sep=''))
  result <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  DBI::dbDisconnect(con)
  return(result)
  
}

convertToGeojsonFeatureCollection <- function(data,name){
  
  #Takes a list of geojson features (for example the geojson column from a tibble) and converts them into a single
  #gosJSON feature collection.
  #INPUT: geom, list of geojson features
  #OUTPUT: string, single string geoJSON feature collection
  ## USE CASE: Data from PostgreSQL database is in WKB format, easily converted to GeoJSON but as a list of single features
  ## Leaflet takes in the geoJSON featureCollection easily as one object
  ## TODO: Encapsulate information from other columns into properties for each feature
  featureCollectionHead <- '{"type":"FeatureCollection","features":['
  featureCollectionTail <- ']}'
  
  data$featureCollectionFeatures <- paste('{"type":"Feature","properties":{"id":"',data$ogc_fid,'","name":"',data[[name]],'"},"geometry":',data$geojson,'}',sep = "")
  
  featureCollectionBody <- paste0(data$featureCollectionFeatures,collapse = ',')
  
  featureCollection <- paste0(featureCollectionHead,featureCollectionBody,featureCollectionTail)
  
  
  return(featureCollection)
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
  res <- neo4r::call_neo4j(paste0("MATCH (p:Person{personEmail:'",user,
                           "',personPassword:'",pw,
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
      requestedTibble = results$nodes[results$nodes$id %in% requested,c('metadataTitle','metadataAbstract')]
    }else{requestedTibble = tibble(metadataTitle = character(),metadataAbstract = character())}
    # if user has no submitted metadata yet pass in an empty tibble
    if(length(submitted > 0)){
      submittedTibble = results$nodes[results$nodes$id %in% submitted,c('metadataTitle','metadataAbstract')]
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
                         submitted = submittedTibble))
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
adminCreateNewUser <- function(fullname,pw,email,affiliation,admin){
  # Create a new user in the database
  # TODO: work out how to get rid of workaroundnode, this is used because check credentials function requires a node returned with at least 1 relationship
  # TODO: user feedback success/failure
  res <- neo4r::call_neo4j(paste0("MATCH (w:WorkaroundNode) CREATE (p:Person{created:'",Sys.time(),"',personName:'",fullname,
                    "',personPassword:'",pw,
                    "',personEmail:'",email,
                    "',personAffiliation:'",affiliation,
                    "',personAdmin:",admin,
                    "})-[:NEWUSER]->(w)"),
                    neo_con,
                    type = 'row')
  
}