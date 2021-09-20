require(tidyverse)
require(sf)
require(leaflet)
require(crosstalk)
require(DT)
require(shiny)
require(shinydashboard)
require(RPostgres)
require(DBI)
require(neo4r)

source("./src/secrets.R",local = TRUE)

# Custom functions
source("./src/custom_functions.R",local = TRUE)

#########################
# Pull data from SQL database using function from gisOperations

ICES_Ecoregions <- loadFullWKBData("ices_ecoregions")

nafoDivisionsSF <- loadFullWKBData("nafo_divisions")

icesStatEcoSF <- loadFullWKBData("ices_stat_rect_eco")

migrationSF <- loadFullWKBData("proposed_migration_routes")

feedingSF <- loadFullWKBData("feeding_zones")
##########################

##############################
# IMPORT METADATA FROM GRAPH
neo_con <- neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
                         user = NEO4J_USER,
                         password = NEO4J_PASSWD) 
LSFMetadata <- neo4r::call_neo4j("MATCH (m:Metadata) RETURN m,id(m) as id ORDER BY id;",neo_con,type = 'row')
LSFMetadataTibble <- LSFMetadata$m
LSFMetadataTibble$id <- LSFMetadata$id$value

##############################

#############################
# This section does two things:
# Create polygons for each row in LSFMetadataTibble for use in intersection code in the next step
# Create calculated centroids for each polygon for delivery back to metadataCoverageCentroid in the DB and use in the web application map
# Development note: Is it better to store the polygons in the graph as WKT? WOuld this make operations faster on the web application?

#set up a geometry column with dummy polygon to house the soon to be created real polygons
LSFMetadataTibble$geometry <- st_sfc(st_polygon(list(rbind(c(0,0),c(1,0),c(1,-1),c(0,-1),c(0,0)))))
LSFMetadataTibble$centre <- "POINT (0,0)" # create centre point column to accept as WKT
# for loop to create individual polygons per tibble row
for(i in 1:nrow(LSFMetadataTibble)){
  # rbind list created from the 4 lon/lat columns in the data
  # note polygon standard clockwise, 5 points for 4 sided polygon WestNorth, EastNorth, EastSouth, WestSouth, WestNorth
  thislist <- list(rbind(c(LSFMetadataTibble$metadataCoverageWest[i],LSFMetadataTibble$metadataCoverageNorth[i]),
             c(LSFMetadataTibble$metadataCoverageEast[i],LSFMetadataTibble$metadataCoverageNorth[i]),
             c(LSFMetadataTibble$metadataCoverageEast[i],LSFMetadataTibble$metadataCoverageSouth[i]),
             c(LSFMetadataTibble$metadataCoverageWest[i],LSFMetadataTibble$metadataCoverageSouth[i]),
             c(LSFMetadataTibble$metadataCoverageWest[i],LSFMetadataTibble$metadataCoverageNorth[i])))
  
  # use st_polygon to convert to simple feature polygon object
  thispolygon <- st_polygon(thislist)
  
  # pass in to replace dummy entry in geometry column
  LSFMetadataTibble$geometry[i] <- thispolygon
  LSFMetadataTibble$centre[i] <- st_as_text(st_centroid(thispolygon)) # Calculate centroids
  # Return calculated centroids to graph (note polygons are not written back to the graph db)
  #query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",LSFMetadataTibble$id[i]," SET m.metadataCoverageCentroid = '",LSFMetadataTibble$centre[i],"';")
  #call_neo4j(query,neo_con,type = 'row')
}

# convert full tibble to sf object and set CRS - by default this operation uses the 'geometry' column as the sf geometry column
LSFMetadataTibble <- sf::st_as_sf(LSFMetadataTibble,crs=4326)

######################
######################
# Calculate intersects
# st_intersects returns a Sparse geometry binary predicate list
# This is basically a list as long as the first argument passed
# and as wide as the second, with varying row width based on how many intersects occur
ecoregionIntersects <- sf::st_intersects(LSFMetadataTibble,ICES_Ecoregions)
nafodivisionIntersects <- sf::st_intersects(LSFMetadataTibble,nafoDivisionsSF)
#icesrectanglesIntersects <- sf::st_intersects(LSFMetadataTibble,icesStatEcoSF)
migrationrouteIntersects <- sf::st_intersects(LSFMetadataTibble,migrationSF)

######################
######################
# NOTE ST_DISTANCE REQUIRED SOME NEW PACKAGES - liblwgeom-2.5-0 base and lwgeom R
# Calculate nearest polygon where intersect length = 0
# NOTES: nearest polygons for us/canada and greenland rivers should all be NAFO divisions, the rest should be ICES EcoRegions
# This can be done by separating the metadata by longitude -44 degree East

# set up a tibble to capture results
nonIntersectingMetadata <- dplyr::tibble(nodeID = integer(0),nearestEco = character(0),nearestNafo = character(0))

for(i in 1:nrow(LSFMetadataTibble)){
  # for loop to identify non intersecting metadata i.e. 0 length results in the intersect lists above
  if(length(ecoregionIntersects[[i]]) == 0 & length(nafodivisionIntersects[[i]]) == 0){
    if(LSFMetadataTibble$metadataCoverageEast[i] <= -44){ # assign all metadata that is west of -44 to a nafo division
      test <- sf::st_distance(LSFMetadataTibble[i,],nafoDivisionsSF)
      nonIntersectingMetadata <- bind_rows(nonIntersectingMetadata,dplyr::tibble(nodeID = LSFMetadataTibble$id[i],nearestEco = "",nearestNafo = nafoDivisionsSF$zone[which.min(test)]))
    }else{ # assign all the rest to a ecoregion
      test <- sf::st_distance(LSFMetadataTibble[i,],ICES_Ecoregions)
      nonIntersectingMetadata <- bind_rows(nonIntersectingMetadata,dplyr::tibble(nodeID = LSFMetadataTibble$id[i],nearestEco = ICES_Ecoregions$ecoregion[which.min(test)],nearestNafo = ""))
    }
  }
}# pass these back to the graph AFTER the intersects, otherwise intersects will overwrite


###############################
# Pass these intersects back to the graph
#ECOREGIONS
for(i in 1:nrow(ecoregionIntersects)){
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",
                  LSFMetadataTibble$id[i],
                  " SET m.metadataCoverageIntersectICESEcoRegion = '",
                  paste0(ICES_Ecoregions$ecoregion[ecoregionIntersects[[i]]],collapse = ','),
                  "';")
  #print(query)
  call_neo4j(query,neo_con,type = 'row')
}
#NAFODIVISIONS
for(i in 1:nrow(nafodivisionIntersects)){
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",
                  LSFMetadataTibble$id[i],
                  " SET m.metadataCoverageIntersectNAFODivision = '",
                  paste0(nafoDivisionsSF$zone[nafodivisionIntersects[[i]]],collapse = ','),
                  "';")
  #print(query)
  call_neo4j(query,neo_con,type = 'row')
}

#MIGRATIONROUTES_BUFFERED - ogc_fid only
for(i in 1:nrow(migrationrouteIntersects)){
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",
                  LSFMetadataTibble$id[i],
                  " SET m.metadataCoverageIntersectMigrationRoutes = '",
                  paste0(migrationSF$ogc_fid[migrationrouteIntersects[[i]]],collapse = ','),
                  "';")
  #print(query)
  call_neo4j(query,neo_con,type = 'row')
}

for(i in 1:nrow(nonIntersectingMetadata)){
  query <- paste0("MATCH (m:Metadata) WHERE id(m) = ",
                  nonIntersectingMetadata$nodeID[i],
                  " SET m.metadataCoverageIntersectNAFODivision = '",
                  nonIntersectingMetadata$nearestNafo[i],
                  "', m.metadataCoverageIntersectICESEcoRegion = '",
                  nonIntersectingMetadata$nearestEco[i],
                  "';")
  #print(query)
  call_neo4j(query,neo_con,type = 'row')
}

