conditionalPanel(
  condition = "input.menu1 == 'searchlsf'",
  column(
    width = 12,
    hr(),
    uiOutput('searchSidebarFilters'),
    downloadButton('downloadSearchResults',"Download Search Results", class = 'btn-primary')
  )
)