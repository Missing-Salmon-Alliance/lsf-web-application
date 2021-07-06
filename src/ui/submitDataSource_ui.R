############################
#submitDataSource_ui.R START
############################
# tabItem 3
tabItem( # tabItem 3
  tabName = "newsource",
  conditionalPanel(
    condition = "output.logonTrue",
    fluidRow(
      column(12,
             #div(style="float:right",actionLink(inputId = "submitDescript", label = "Help", icon = icon("question-circle"))),
             #shinyBS::bsCollapse(id = "submitDataSourceFields", open = c("Data Source Details","Geographic Coverage and Temporal Coverage"),
                                 #multiple = TRUE,
                                 source("./src/ui/submitUI/submitDataSource_metadataFields_ui.R",local = TRUE)$value,
                                 source("./src/ui/submitUI/submitDataSource_geoTemporal_ui.R",local = TRUE)$value,
                                 source("./src/ui/submitUI/submitDataSource_domainsESV_ui.R",local = TRUE)$value
             #)
      )
    ),
      
    ##############################
    #### DEBUG INFORMATION SECTION
    ##############################
    conditionalPanel(
      condition = "input.debug",
      box(
        width = 12,
        textOutput('CreateMetadataNodeQueryBuilder')
      ),
      box(
        width = 12,
        DT::DTOutput('showCurrentSelection')
      ),
      box(
        width = 12,
        # sidepanel display dropped file(s)
        DT::DTOutput('filesPendingUpload')
      ),
      box(
        width = 12,
        textOutput('sessionUUIDPrint')
      ),
      box(
        width = 12,
        textOutput('generalTesting')
      )
      
    )
    ##############################
    #### DEBUG INFORMATION SECTION
    ##############################
  ),
  conditionalPanel(
    condition = "!output.logonTrue",
    h1("Framework Knowledge Submit Area"),
    h3("Please authenticate to access this area")
  )
) # tabItem 3 close
############################
#submitDataSource_ui.R END
############################