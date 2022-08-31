##############################################
# Submit Data Source Conditional Sidebar Items
##############################################
conditionalPanel(
  condition = "input.menu1 == 'domainExplore' && output.logonTrue",
  column(
    width = 12,
    hr(),
    h4("Filters"),
    # Sidebar filter area
    # using uiOutput/renderUI here to prevent the inputs appearing for a moment on application reload 
    uiOutput('domainExploreFiltersUI'),
  )
)
##############################################
# Submit Data Source Conditional Sidebar Items
##############################################