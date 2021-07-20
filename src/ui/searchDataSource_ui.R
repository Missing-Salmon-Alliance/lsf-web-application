################ Search


tabItem( # tabItem 1
  tabName = "searchlsf",
  conditionalPanel(
    condition = "output.logonTrue",
    #div(style="float:left",actionLink(inputId = "searchDescript", label = "Help", icon = icon("question-circle"))),
    #br(),
    absolutePanel(id = 'searchFiltersAbsPanel',
                  top = "170px",
                  right = "10px",
                  width = "35%",
                  style="z-index:1000;",
                  shinyBS::bsCollapse(id = "mapSearchFilters", open = NULL,
                             #source("./src/ui/searchDataSource_frameworkFilters_ui.R",local = TRUE)$value,
                             #source("./src/ui/searchDataSource_geographicFilters_ui.R",local = TRUE)$value,
                             source("./src/ui/searchDataSource_temporalFilters_ui.R",local = TRUE)$value
                  )
    ),
    fluidRow(
      shiny::tabsetPanel(id = "maptable",selected = "Map View",
                         tabPanel(title = "Map View",
                                  leaflet::leafletOutput("map", height = "85vh")
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
    h1("Central Data Resource Knowledge Search Area"),
    h3("Please authenticate to access this area")

  )
)