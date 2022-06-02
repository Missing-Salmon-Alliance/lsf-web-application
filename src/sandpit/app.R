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
    column(width = 4,
      h3('Title:'),
      textOutput('title'),
      h3('Abstract:'),
      textOutput('abstract'),
      h3('Organisation:'),
      textOutput('organisation'),
      h3('URL:'),
      textOutput('url'),
      h3('Access Protocol:'),
      textOutput('accessProtocol'),
      h3('Geography and Time:'),
      textOutput('geographicDescription'),
      textOutput('geographicExtents'),
      textOutput('temporalRange')
    ),
    column(
      width = 4,
      leaflet::leafletOutput('searchTabMap',height = '90vh')
      # br(),
      # shinydashboard::box(
      #   width = 12,
      #   title = "Selected Data Source Information",
      #   column(width = 6,
      #     h3('Title:'),
      #     textOutput('title'),
      #     h3('Abstract:'),
      #     textOutput('abstract')
      #   ),
      #   column(width = 3,
      #     h3('Organisation:'),
      #     textOutput('organisation'),
      #     h3('URL:'),
      #     textOutput('url'),
      #     h3('Access Protocol:'),
      #     textOutput('accessProtocol')
      #   ),
      #   column(width = 3,
      #     h3('Geography and Time:'),
      #     textOutput('geographicDescription'),
      #     textOutput('geographicExtents'),
      #     textOutput('temporalRange')
      #   )
      # )
    )
  )
)

server <- function(input,output,session){
  lsfMetadata <- shiny::reactiveVal()
  intersectVector <- shiny::reactiveVal()
  lsfMetadata(sf::st_as_sf(testData, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  
  output$searchTabTable <- DT::renderDataTable({
    # req(input$searchTabMap_bounds)
    # # Create Intersection vector to filter table
    # intersectVector(
    #   as.vector(
    #     sf::st_intersects(
    #       sf::st_polygon(
    #         list(
    #           rbind( # NOTE - build polygon clockwise AND WKT notation has longitude first, latitude second
    #             as.numeric(input$searchTabMap_bounds[c(4,1)]),
    #             as.numeric(input$searchTabMap_bounds[c(2,1)]),
    #             as.numeric(input$searchTabMap_bounds[c(2,3)]),
    #             as.numeric(input$searchTabMap_bounds[c(4,3)]),
    #             as.numeric(input$searchTabMap_bounds[c(4,1)])
    #           )
    #         )
    #       ),
    #       lsfMetadata(),sparse = FALSE
    #     )
    #   )
    # )
    sf::st_set_geometry(lsfMetadata()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
    },
    selection = 'single',
    rownames = FALSE,
    editable = FALSE,
    colnames = c('Data Source Title','Abstract','Keywords'),
    options = list(pageLength = 15,
                   columnDefs = list(list(visible=FALSE, targets=c(1,2)),
                                     list(
                                       targets = 0,
                                       # limit row width by truncating text in the title field
                                       render = JS("function(data, type, row, meta) {return type === 'display' && data.length > 80 ? '<span title=' + data + '>' + data.substr(0, 80) + '...</span>' : data;}"))
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
  
  observeEvent(input$searchTabTable_rows_selected,{
    leaflet::leafletProxy('searchTabMap') %>%
      leaflet::clearGroup(group = 'selectedRectangle') %>%
      leaflet::addRectangles(
        lsfMetadata()$metadataCoverageWest[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageNorth[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageEast[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageSouth[input$searchTabTable_rows_selected],
        group = 'selectedRectangle') %>%
      leaflet::fitBounds(
        lsfMetadata()$metadataCoverageWest[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageNorth[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageEast[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageSouth[input$searchTabTable_rows_selected])
  })
  output$title <- renderText({lsfMetadata()$metadataTitle[input$searchTabTable_rows_selected]})
  output$abstract <- renderText({lsfMetadata()$metadataAbstract[input$searchTabTable_rows_selected]})
  output$organisation <- renderText({lsfMetadata()$metadataOrganisation[input$searchTabTable_rows_selected]})
  output$url <- renderText({lsfMetadata()$metadataAltURI[input$searchTabTable_rows_selected]})
  output$accessProtocol <- renderText({lsfMetadata()$metadataAccessProtocol[input$searchTabTable_rows_selected]})
  output$geographicDescription <- renderText({lsfMetadata()$metadataGeographicDescription[input$searchTabTable_rows_selected]})
  # output$geographicExtents <- renderText({lsfMetadata()$metadata[input$searchTabTable_rows_selected]})
  # output$temporalRange <- renderText({lsfMetadata()$metadata[input$searchTabTable_rows_selected]})
  
}

shinyApp(ui = ui, server = server)