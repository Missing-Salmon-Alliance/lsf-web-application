library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(tidyverse)

testData <- readr::read_csv('CDR_Data.csv')
testData$metadataAccessProtocol <- "Testing Testing 1,2,3"

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
      8,
      column(
        width = 12,
        leaflet::leafletOutput('searchTabMap',height = '45vh')
      ),
      shinydashboard::box(width = 12,
        title = "Summary",
        status = "warning",
        solidHeader = F,
        height = "45vh",
        column(
          6,
          h3('Title:'),
          textOutput('title'),
          h3('Abstract:'),
          textOutput('abstract'),
          h3('Access Protocol:'),
          textOutput('accessProtocol')
        ),
        column(
          6,
          h3('Organisation:'),
          textOutput('organisation'),
          h3('URL:'),
          textOutput('url'),
          h3('Geography and Time:'),
          textOutput('geographicDescription'),
          textOutput('geographicExtents'),
          textOutput('temporalRange')
        )
      )
    )
  )
)

server <- function(input,output,session){
  lsfMetadata <- shiny::reactiveVal()
  intersectVector <- shiny::reactiveVal()
  lsfMetadata(sf::st_as_sf(testData, wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  
  output$searchTabTable <- DT::renderDataTable({
    sf::st_set_geometry(lsfMetadata()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
    },
    selection = 'single',
    rownames = FALSE,
    editable = FALSE,
    colnames = c('Data Source Title','Abstract','Keywords'),
    options = list(pageLength = 20,
                   columnDefs = list(list(visible=FALSE, targets=c(1,2)),
                                     list(
                                       targets = 0,
                                       # limit row width by truncating text in the title field
                                       render = htmlwidgets::JS("function(data, type, row, meta) {return type === 'display' && data.length > 80 ? '<span title=' + data + '>' + data.substr(0, 80) + '...</span>' : data;}"))
                                     ),
                   searching = T,
                   lengthChange = FALSE,
                   autoWidth = FALSE
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
      leaflet::clearGroup(group = 'markerRectangle') %>%
      leaflet::addRectangles(
        lsfMetadata()$metadataCoverageWest[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageNorth[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageEast[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageSouth[input$searchTabTable_rows_selected],
        group = 'markerRectangle') %>%
      leaflet::fitBounds(
        lsfMetadata()$metadataCoverageWest[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageNorth[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageEast[input$searchTabTable_rows_selected],
        lsfMetadata()$metadataCoverageSouth[input$searchTabTable_rows_selected],
        options = leaflet::leafletOptions(maxZoom = 10))
  })
  output$title <- renderText({lsfMetadata()$metadataTitle[input$searchTabTable_rows_selected]})
  output$abstract <- renderText({lsfMetadata()$metadataAbstract[input$searchTabTable_rows_selected]})
  output$organisation <- renderText({lsfMetadata()$metadataOrganisation[input$searchTabTable_rows_selected]})
  output$url <- renderText({lsfMetadata()$metadataAltURI[input$searchTabTable_rows_selected]})
  output$accessProtocol <- renderText({lsfMetadata()$metadataAccessProtocol[input$searchTabTable_rows_selected]})
  output$geographicDescription <- renderText({lsfMetadata()$metadataGeographicDescription[input$searchTabTable_rows_selected]})
  output$geographicExtents <- renderText({
                                          paste0(
                                            "North: ",lsfMetadata()$metadataCoverageNorth[input$searchTabTable_rows_selected],
                                            ", East: ",lsfMetadata()$metadataCoverageEast[input$searchTabTable_rows_selected],
                                            ", South: ",lsfMetadata()$metadataCoverageSouth[input$searchTabTable_rows_selected],
                                            ", West: ",lsfMetadata()$metadataCoverageWest[input$searchTabTable_rows_selected]
                                          )
                                        })
  output$temporalRange <- renderText({
                                      paste0(
                                        "Start Year: ",lsfMetadata()$metadataCoverageStartYear[input$searchTabTable_rows_selected],
                                        ", End Year: ",lsfMetadata()$metadataCoverageEndYear[input$searchTabTable_rows_selected]
                                        )
                                    })
  
}

shinyApp(ui = ui, server = server)