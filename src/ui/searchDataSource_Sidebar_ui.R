conditionalPanel(
  condition = "input.menu1 == 'searchlsf' && output.logonTrue",
  column(
    width = 12,
    hr(),
    column(
      width = 12,
      h4("Active Filter:"),
      textOutput('activeGeographicFilter')
      
    ),
    uiOutput('searchFilterResetUI'),
    uiOutput('onlineOnlyFilterUI'),
    hr(),
    uiOutput('searchRefreshUI')
  )
)