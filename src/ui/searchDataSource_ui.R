################ Search


tabItem( # tabItem 1
  tabName = "searchlsf",
  conditionalPanel(
    condition = "output.logonTrue",
    #div(style="float:left",actionLink(inputId = "searchDescript", label = "Help", icon = icon("question-circle"))),
    #br(),
    # hiding until temporal filter works better
    # absolutePanel(id = 'searchFiltersAbsPanel',
    #               top = "110px",
    #               right = "10px",
    #               width = "35%",
    #               style="z-index:1000;",
    #               shinyBS::bsCollapse(id = "mapSearchFilters", open = NULL,
    #                                   # new location to be determined for geographic filters
    #                          #source("./src/ui/searchDataSource_geographicFilters_ui.R",local = TRUE)$value,
    #                          source("./src/ui/searchDataSource_temporalFilters_ui.R",local = TRUE)$value
    #               )
    # ),
    fluidRow(
      shiny::tabsetPanel(id = "maptable",selected = "Map View",
                         tabPanel(title = "Map View",
                                  leaflet::leafletOutput("map", height = "85vh"),
                                  conditionalPanel(
                                    condition = "input.debug",
                                    textOutput('clickOutput'),
                                    textOutput('clickMarkerOutput'),
                                    textOutput('clickShapeOutput'),
                                    textOutput('clickBoundsOutput')
                                  )
                                  
                         ),
                         tabPanel(title = "Table View",
                                  column(
                                    width = 12,
                                    p("This is a more searchable alternative to the map view. Use the search
                                    box at the top right of the table for free-text searches of the resources.
                                    This search covers resource Title and Abstract, plus other fields such as
                                    tags that may exist in the database but do not appear in the table.",
                                    tags$b("Functionality is limited at the moment, development is underway
                                    to integrate this view more closely with the map view to allow cross
                                    over of search results."))
                                  ),
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