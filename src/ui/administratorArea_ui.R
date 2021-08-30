############################
#administratorArea_ui.R START
############################
tabItem( # tabItem 
  tabName = 'adminZone',
  tabsetPanel(id = "useradmin",
              selected = "User Administration",
              tabPanel(
                id = 'useradmintabNewUser',
                title = "User Administration",
                       box(title = "New User Form",
                           status = 'warning',
                           textInput('useradminFullname',label = "Fullname"),
                           textInput('useradminEmail',label = "Email"),
                           textInput('useradminAffiliation',label = "Affiliation"),
                           passwordInput('useradminPassword',label = "Password"),
                           checkboxInput('useradminAcceptDSA',label = "DSA Accepted?",value = FALSE),
                           checkboxInput('useradminPromoteOrg',label = "Promote Organisation as DSG Member?",value = FALSE),
                           checkboxInput('useradminAdmin',label = "Administrator?",value = FALSE)
                           ),
                       actionButton('addNewUser',"Add New User", icon = icon('user-check'))
              ),
              tabPanel(
                id = 'useradmintabQC',
                title = "Quality Control",
                fluidRow(
                  source("./src/ui/qualityControlUI/qcDataSource_metadataFields_ui.R",local = TRUE)$value,
                  source("./src/ui/qualityControlUI/qcDataSource_geoTemporal_ui.R",local = TRUE)$value,
                  source("./src/ui/qualityControlUI/qcDataSource_domainsESV_ui.R",local = TRUE)$value
                )
              )
  ) # close tabsetPanel
)

############################
#administratorArea_ui.R START
############################