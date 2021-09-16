### User Interface created using shinydashboard package
ui <- dashboardPage(skin = "blue",
                    title = "Missing Salmon Alliance", # This is the title that appears in the browser tab name
                    header = dashboardHeader(
                      # Set MSA logo as title and clickable link to MSA website
                      # https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
                      title = tags$a(href='https://missingsalmonalliance.org',
                                     tags$img(src='The_missing_salmon_type-394x100.png'), # note, logo stored in www folder
                                     target="_blank"), # opens the link in a new tab/window
                      titleWidth = 394,
                      # tag$li for header based user logon, logout and profile buttons
                      source("./src/ui/headerItems_ui.R",local = TRUE)$value
                    ),
                    sidebar = dashboardSidebar(
                      disable = FALSE,
                      minified = TRUE, # shinydashboardPlus feature
                      collapsed = FALSE,
                      
                      # three breaks needed to bring buttons lower than enlarged header bar height
                      br(),
                      br(),
                      br(),
                      
                      # Build sidebar menu
                      # Conditional sidebar menu items controlled from server file sidebar_server.R
                      uiOutput('menu'),
                      
                      ##############################################
                      # Introduction Conditional Sidebar Items
                      ##############################################
                      source("./src/ui/introductionTab_Sidebar_ui.R",local = TRUE)$value,
                      ##############################################
                      # Introduction Conditional Sidebar Items
                      ##############################################
                      
                      ##############################################
                      # Submit Data Source Conditional Sidebar Items
                      ##############################################
                      source("./src/ui/submitDataSource_Sidebar_ui.R",local = TRUE)$value,
                      ##############################################
                      # Submit Data Source Conditional Sidebar Items
                      ##############################################

                      ##############################################
                      # Search Data Source Conditional Sidebar Items
                      ##############################################
                      source("./src/ui/searchDataSource_Sidebar_ui.R",local = TRUE)$value,
                      ##############################################
                      # Search Data Source Conditional Sidebar Items
                      ##############################################
                      
                      conditionalPanel(
                        condition = "input.menu1 == 'adminZone' && output.logonTrue",
                        column(
                          width = 12,
                          br(),
                          h4("Load and review"),
                          selectizeInput('QCidSelector','Dropdown',choices = LSFMetadataTibble$id, width = '100%'),
                          column(
                            width = 6,
                            actionLink('QCpreviousID',"Prev")
                          ),
                          column(
                            width = 6,
                            actionLink('QCnextID',"Next")
                          ),
                          br(),
                          h6("...OR...",align = 'center'),
                          textInput('QCcustomID','Manual input:'),
                          actionButton('QCcustomIDButton',"Load from ID")
                        )
                      )
                      

                    ),
                    body = dashboardBody(
                      shinyjs::useShinyjs(),
                      includeCSS('www/custom.css'),
                      
                      # EXPERIMENTAL changing background colours and picture - don't work too well!
                      #setBackgroundImage(src="https://www.transparenttextures.com/patterns/dark-mosaic.png", shinydashboard = TRUE),
                      #setBackgroundColor(c('ghostwhite','#3c8dbc'),gradient = 'radial',direction = "top", shinydashboard = TRUE),

                      # Fixed panel to display DEMO text
                      # fixedPanel(style="z-index:10000;",
                      #            top="0%", left="30%", width="50%",
                      #            h1(span("DEMO",style="color:red")," - This interface is currently in development - ",span("DEMO",style="color:red")),
                      #            p("For more information please contact: graeme@atlanticsalmontrust.org")
                      # ),

                      # tabItems open
                      tabItems(
                        source("./src/ui/introductionTab_ui.R", local = TRUE)$value, # tabItem intro
     
                        source("./src/ui/newUserRegistrationTab_ui.R", local = TRUE)$value, # HIDDEN TAB ITEM
                        
                        source("./src/ui/submitDataSource_ui.R",local = TRUE)$value, # tabItem submit data

                        source("./src/ui/searchDataSource_ui.R", local = TRUE)$value, # tabItem general search view
                        
                        source("./src/ui/hypothesisUI/hypothesisExplore_ui.R", local = TRUE)$value, #tabItem hypothesis explore
                        
                        source("./src/ui/domainExploreUI/domainExplore_ui.R", local = TRUE)$value, # tabItem domain explore
                        
                        source("./src/ui/metadataNodeReport_ui.R", local = TRUE)$value, # tabItem metadataNodeReport
                        
                        source("./src/ui/researchInventoryUI/submitResearchActivity_ui.R",local = TRUE)$value, #tabItem submit research
                        
                        source("./src/ui/administratorArea_ui.R",local = TRUE)$value #tabItem Admin Area
                      ),# tabItems close
                      bsPopover("test","test")# tabItems close
                    ) # dashboardBody close
                    
                    # Some example code, footer and right hand sidebar
                    # footer = dashboardFooter(
                    #   left = "Missing Salmon Alliance",
                    #   right = "2021"
                    # )
                    #controlbar = dashboardControlbar(collapsed = FALSE, skinSelector()) # RIGHT Hand Sidebar
) # dashboardPage close

