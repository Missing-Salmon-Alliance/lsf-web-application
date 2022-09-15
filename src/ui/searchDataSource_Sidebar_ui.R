conditionalPanel(
  condition = "input.menu1 == 'searchlsf' && output.logonTrue",
  column(
    width = 12,
    hr(),
    uiOutput('searchSidebarFilters'),
    downloadButton('downloadSearchResults',"Download Search Results", class = 'btn-primary')
  )
)