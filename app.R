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
## Create NEO4J Connection object

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
  user = NEO4J_USER,
  password = NEO4J_PASSWD)

# Set up waiter that covers search map during db updates
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

### User Interface created using shinydashboard package
ui <- dashboardPage(skin = "blue",
  title = "Missing Salmon Alliance", # This is the title that appears in the browser tab name
  header = dashboardHeader(
    # Set MSA logo as title and clickable link to MSA website
    # https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
    # title = tags$a(href='https://missingsalmonalliance.org',
    #                tags$img(src='The_missing_salmon_type-394x100.png'), # note, logo stored in www folder
    #                target="_blank"), # opens the link in a new tab/window
    title = tags$img(class = 'logo-lg',src = './images/SalHub_full_394x100.png',width = 394),
    titleWidth = 394,
    # tag$li for header based user logon, logout and profile buttons
    source("./src/ui/headerItems_ui.R",local = TRUE)$value
  ),
  sidebar = dashboardSidebar(
    disable = FALSE,
    minified = FALSE, # shinydashboardPlus feature
    collapsed = FALSE,
    
    # three breaks needed to bring buttons lower than enlarged header bar height
    br(),
    br(),
    br()
  ),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyjs::extendShinyjs(script = 'custom.js', functions = c('markerClick')),
    includeCSS('www/custom.css'),
    shinyBS::bsPopover("test","test") # bit of a bodge, server defined popovers don't seem to work without this line
  ), # dashboardBody close

  controlbar = dashboardControlbar(collapsed = FALSE,    # three breaks needed to bring buttons lower than enlarged header bar height
    br(),
    br(),
    br(),
    skinSelector()) # RIGHT Hand Sidebar
) # dashboardPage close

server <- function(input, output, session) {
  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)
  
  # Load databases
  initDataLoad_Metadata$show()
  lsfDomains <- reactiveVal(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder))
  lsfMetadata <- reactiveVal(sf::st_as_sf(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  lsfHypotheses <- reactiveVal(neo4r::call_neo4j("MATCH (h:Hypothesis) RETURN h;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
  lsfVariableClasses <- reactiveVal(neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle))
  initDataLoad_Metadata$hide()

  initDataLoad_GIS_ICES$show()
  ICES_Ecoregions <<- loadFullWKBData("ices_ecoregions_simplified")
  initDataLoad_GIS_ICES$hide()
  
  initDataLoad_GIS_NAFO$show()
  nafoDivisionsSF <<- loadFullWKBData("nafo_divisions")
  initDataLoad_GIS_NAFO$hide()
  
  
  initDataLoad_GIS_Rivers$show()
  riversDBSF <<- loadFullWKBData('rivers_db')
  nascoRiversDBSF <<- riversDBSF[riversDBSF$nasco_rivers_db == TRUE,] # subset of rivers that were sourced from the original NASCO DB
  indexRiversSF <<- riversDBSF[riversDBSF$ices_index == TRUE,] # subset of rivers that are ICES index rivers
  initDataLoad_GIS_Rivers$hide()
  
  initDataLoad_GIS_LSF$show()
  salmosalarRange <<- loadFullWKBData('atlantic_salmon_range')
  salmosalarExtents <<- sf::st_bbox(salmosalarRange)
  initDataLoad_GIS_LSF$hide()

}

shinyApp(ui, server)