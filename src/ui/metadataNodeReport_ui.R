tabItem(# tabItem metadataNodeReport OPEN
  tabName = 'metadataNodeReport',
  conditionalPanel(
    condition = "output.logonTrue",
    fluidPage(
      h4("Metadata Node Report"),
      box(
        width = 12,
        title = "Temporal Information Stats:",
        column(
          width = 4,
          plotly::plotlyOutput('monthsOfYearStats')
        ),
        column(
          width = 4,
          plotly::plotlyOutput('startYearStats')
        ),
        column(
          width = 4,
          plotly::plotlyOutput('endYearStats')
        )
      ),
      column(
        width = 8,
        DT::DTOutput('metadataTable')
      ),
      column(
        width = 4,
        plotly::plotlyOutput('esv_plot')
      )
    )
  ),
  conditionalPanel(
    condition = "!output.logonTrue",
    h1("Central Data Resource Statistics Area"),
    h3("Please authenticate to access this area")
  )
)# tabItem metadataNodeReport Close