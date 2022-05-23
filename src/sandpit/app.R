library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(sf)
library(DT)
library(shinyjs)

testData <- readr::read_csv('CDR_Data.csv')

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = 'Sandpit'
  ),
  shinydashboard::dashboardSidebar(
  ),
  shinydashboard::dashboardBody(
    
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = '../../www/custom.js', functions = c('markerClick')),
    includeCSS('../../www/custom.css'),
    column(
      width = 4,
      DT::dataTableOutput('searchTabTable')
    ),
    column(
      width = 8,
      leaflet::leafletOutput('searchTabMap'),
      br(),
      shinydashboard::box(
        width = 12,
        title = "Selected Data Source Information"#,
        # ~paste("<h3>More Information</h3>",
        #        "<b>Title:</b>",stringr::str_trunc(metadataTitle,width = 90,side = 'right',ellipsis = '...'),"<br>","<br>",
        #        "<b>Abstract:</b>",stringr::str_trunc(metadataAbstract,width = 200,side = 'right',ellipsis = '...'),"<br>","<br>",
        #        "<b>Organisation:</b>",metadataOrganisation,"<br>","<br>",
        #        "<b>URL (if available):</b>",metadataAltURI,"<br>","<br>",
        #        "<em>UUID:</em>",metadataUUID,"<br>","<br>",
        #        "<em>Internal ID:</em>",id,"<br>","<br>",
        #        "&nbsp;",actionButton("showmodal", "View more...", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
        #        sep =" ")
      )
    )
  )
)

server <- function(input,output,session){
  lsfMetadata <- shiny::reactiveVal()
  intersectVector <- shiny::reactiveVal()
  lsfMetadata(sf::st_as_sf(testData, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  
  output$searchTabTable <- DT::renderDataTable({
    req(input$searchTabMap_bounds)
    # Create Intersection vector to filter table
    intersectVector(
      as.vector(
        sf::st_intersects(
          sf::st_polygon(
            list(
              rbind( # NOTE - build polygon clockwise AND WKT notation has longitude first, latitude second
                as.numeric(input$searchTabMap_bounds[c(4,1)]),
                as.numeric(input$searchTabMap_bounds[c(2,1)]),
                as.numeric(input$searchTabMap_bounds[c(2,3)]),
                as.numeric(input$searchTabMap_bounds[c(4,3)]),
                as.numeric(input$searchTabMap_bounds[c(4,1)])
              )
            )
          ),
          lsfMetadata(),sparse = FALSE
        )
      )
    )
    sf::st_set_geometry(lsfMetadata()[intersectVector(),c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
    },
    selection = 'single',
    rownames = FALSE,
    editable = FALSE,
    colnames = c('Data Source Title','Abstract','Keywords'),
    options = list(pageLength = 15,
                   columnDefs = list(list(visible=FALSE, targets=c(1,2)),
                                     list(
                                       targets = 0,
                                       render = JS("function(data, type, row, meta) {return type === 'display' && data.length > 50 ? '<span title=' + data + '>' + data.substr(0, 50) + '...</span>' : data;}"))
                                     ),
                   searching = FALSE,
                   lengthChange = FALSE,
                   autoWidth = TRUE
    )
  )
  
  output$searchTabMap <- leaflet::renderLeaflet({ 
    leaflet::leaflet (options = leaflet::leafletOptions(minZoom = 3,maxZoom = 19))%>%
      leaflet::setView(lng = -20,lat = 50,zoom = 3) %>% 
      leaflet::setMaxBounds( lng1 = -180
                             , lat1 = -90
                             , lng2 = 210
                             , lat2 = 90 ) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, options = leaflet::providerTileOptions(minZoom = 3, maxZoom =10)) %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, options = leaflet::providerTileOptions(minZoom = 11, maxZoom = 19)) %>%
      
      leaflet::addMarkers(data = lsfMetadata(),
                          label = ~metadataTitle,
                          layerId = ~id,
                          group = 'Data Source',
                          popup = ~metadataTitle,
                          # enable clustering for spiderfy
                          clusterOptions = leaflet::markerClusterOptions(
                            showCoverageOnHover = TRUE,
                            zoomToBoundsOnClick = TRUE,
                            spiderfyOnMaxZoom = TRUE,
                            removeOutsideVisibleBounds = TRUE,
                            maxClusterRadius = 30,
                            spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5)))
  })
  
}

shinyApp(ui = ui, server = server)