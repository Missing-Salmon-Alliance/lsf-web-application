##############################################
# Submit Data Source Conditional Sidebar Items
##############################################
conditionalPanel(
  condition = "input.menu1 == 'newsource' && output.logonTrue",
  column(
    width = 12,
    hr(),
    h4("Add New Data Source"),
    # Sidebar file drop area
    # using uiOutput/renderUI here to prevent the inputs appearing for a moment on application reload 
    uiOutput('uploadDataFileUI'),
    # sidepanel action button - submit data source
    uiOutput('submitNewDataSourceSidebarUI'),
    hr(),
    uiOutput('clearSubmitFormUI')
    
    # h4("Load from KNB"),
    # uiOutput('sourceKNBURIUI'),
    # uiOutput('loadKNBUI'),
    # uiOutput('refreshKNBTokenUI'),
    # uiOutput('expiryDatetimeKNBToken')
  )
)
##############################################
# Submit Data Source Conditional Sidebar Items
##############################################