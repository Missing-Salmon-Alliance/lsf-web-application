### Global Section

require(shiny)
require(shinydashboard) # css styles
require(shinydashboardPlus) # advanced features
#require(neo4r)# interact with neo4j graph db
#require(visNetwork)# view and manipulate networks
#require(shinyWidgets)# create different input styles
#require(dplyr)
#require(DT)
#require(readr)
#require(RPostgres)
#require(DBI)#interact with database
#require(leaflet)# create maps
#require(rgdal)# map calcs
#require(sf)
#require(dataone)# interact with knb/dataone datasets
#require(stringr)# used to interact with dataone/knb API
require(shinyBS)# create modals
#require(aws.s3)# s3 storage R api
#require(uuid)
#require(crosstalk)
#require(shinyjs)
require(tidyverse)
#require(plotly)
#require(xml2)
#require(shinybusy)
#require(data.table)
#require(leaflet.extras)

source("./src/secrets.R",local = TRUE)
source("./src/custom_functions.R",local = TRUE)
source("./src/general_copy.R",local = TRUE)
## Create NEO4J Connection object

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
                                db = NEO4J_DB,
                                user = NEO4J_USER,
                                password = NEO4J_PASSWD)

# neo4r query results are sometimes passed to log files. This is a list of the desired columns/fields that need captured
neo4rResultFields <<- c('contains_updates','nodes_created','nodes_deleted','properties_set','relationships_created','relationship_deleted','labels_added','labels_removed')

# stock units
stockUnits <- c("LB","NFLD","QB","GF","SF","US","IC.SW","SC.W","SC.E","IR.N","IR","EW","FR","GY","SP","RU","FI","NO","SWD","IC.NE","DK")
# Check box group button icon options
#checkboxGroupButtonsIcons <- list(yes = icon("ok",lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
# Alternative design - Keep for an example
checkboxGroupButtonsIcons <- list(yes = tags$i(class = "fa fa-check-square",style = "color: steelblue"),no = tags$i(class = "fa fa-square-o",style = "color: steelblue"))

# Set up waiter that covers search map during db updates
wLoadDB <- waiter::Waiter$new(id = 'metadataExploreMap',
  html = HTML("
              <div class='container--box'>
                <div class='boxxy'>
                  <div class='spinner spinner--3'>
                  </div>
                </div>
              </div>
              <div>
                <h4 style='color:black;'>Please wait while the map refreshes...</h4>
              </div>
              "),
  color = waiter::transparent(.7)
)

initDataLoad_Metadata <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading the SalHub Metadata Database...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_ICES <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading EcoRegion GIS Data from ICES...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_NAFO <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading Division GIS Data from NAFO...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_Rivers <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading River GIS Data from ICES and NASCO...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_LSF <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading final GIS Data... Almost Done! :)</h4></div>"),
  color = waiter::transparent(.7)
)