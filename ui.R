### User Interface created using shinydashboard package

ui <- dashboardPage(skin = "blue",
                    title = "Missing Salmon Alliance", # This is the title that appears in the browser tab name
                    header = dashboardHeader(
                      # Set MSA logo as title and clickable link to MSA website
                      # https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
                      title = tags$a(href='https://missingsalmonalliance.org',
                                     tags$img(src='msa-logo-small_whitebg.png'), # note, logo stored in www folder
                                     target="_blank"), # opens the link in a new tab/window
                      titleWidth = 295,
                      # tag$li for header based user logon, logout and profile buttons
                      source("./src/ui/headerItems_ui.R",local = TRUE)$value
                    ),
                    sidebar = dashboardSidebar(
                      disable = FALSE,
                      minified = FALSE, # shinydashboardPlus feature
                      collapsed = FALSE,
                      
                      # three breaks needed to bring buttons lower than enlarged header bar height
                      br(),
                      br(),
                      br(),
                      
                      # Build sidebar menu
                      uiOutput('menu'),

                      ##############################################
                      # Search Data Source Conditional Sidebar Items
                      ##############################################
                      source("./src/ui/searchDataSource_Sidebar_ui.R",local = TRUE)$value,
                      ##############################################
                      # Search Data Source Conditional Sidebar Items
                      ##############################################
                      
                      ##############################################
                      # Submit Data Source Conditional Sidebar Items
                      ##############################################
                      source("./src/ui/submitDataSource_Sidebar_ui.R",local = TRUE)$value
                      ##############################################
                      # Submit Data Source Conditional Sidebar Items
                      ##############################################

                    ),
                    body = dashboardBody(
                      shinyjs::useShinyjs(),
                      tags$style(".popover{max-width: 50%;}"),
                      # styling for search page filter collapse panels
                      # makes them slightly faded until mouse over
                      tags$style("#searchFiltersAbsPanel {opacity:0.65;}"),
                      tags$style("#searchFiltersAbsPanel:hover{opacity:1;}"),
                      
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

                        source("./src/ui/searchDataSource_ui.R", local = TRUE)$value, # tabItem search
                        
                        source("./src/ui/submitDataSource_ui.R",local = TRUE)$value, # tabItem submit data
                        
                        source("./src/ui/metadataNodeReport_ui.R", local = TRUE)$value, # tabItem metadataNodeReport
                        
                        source("./src/ui/submitResearchActivity_ui.R",local = TRUE)$value, #tabItem submit research
                        
                        source("./src/ui/administratorArea_ui.R",local = TRUE)$value #tabItem Admin Area
                      ) # tabItems close
                    ) # dashboardBody close
                    #controlbar = dashboardControlbar(collapsed = FALSE, skinSelector()) # RIGHT Hand Sidebar
) # dashboardPage close

