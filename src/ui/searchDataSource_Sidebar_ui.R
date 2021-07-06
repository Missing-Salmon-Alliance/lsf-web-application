conditionalPanel(
  condition = "input.menu1 == 'searchlsf' && output.logonTrue",
  column(
    width = 12,
    hr(),
    uiOutput('searchRefreshUI'),
    uiOutput('searchFilterResetUI')
  )
)