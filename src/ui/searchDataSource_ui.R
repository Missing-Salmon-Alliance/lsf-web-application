################ Search


tabItem( # tabItem 1
  tabName = "searchlsf",
  conditionalPanel(
    condition = "output.logonTrue",
    #div(style="float:left",actionLink(inputId = "searchDescript", label = "Help", icon = icon("question-circle"))),
    #br(),
    absolutePanel(id = 'searchFiltersAbsPanel',
                  top = "110px",
                  right = "10px",
                  width = "35%",
                  style="z-index:1000;",
                  shinyBS::bsCollapse(id = "mapSearchFilters", open = NULL,
                                      # new location to be determined for geographic filters
                             #source("./src/ui/searchDataSource_geographicFilters_ui.R",local = TRUE)$value,
                             source("./src/ui/searchDataSource_temporalFilters_ui.R",local = TRUE)$value
                  )
    ),
    fluidRow(
      shiny::tabsetPanel(id = "maptable",selected = "Map View",
                         tabPanel(title = "Map View",
                                  leaflet::leafletOutput("map", height = "85vh"),
                                  textOutput('clickOutput'),
                                  textOutput('clickMarkerOutput'),
                                  textOutput('clickShapeOutput'),
                                  textOutput('clickBoundsOutput')
                                  
                         ),
                         tabPanel(title = "Table View",
                                  column(
                                    width = 7,
                                    DT::DTOutput('table')
                                  )
                          )
      )
    )
  ),
  conditionalPanel(
    condition = "!output.logonTrue",
    h1("Map Explore Area"),
    h3("Please authenticate to access this area")

  )
)