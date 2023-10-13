server <- function(input, output, session) {
  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)
  ############################
  # Reactive Values START
  ############################
  # intialise reactive values
  # Set to NULL to catch empty values within if statements
  sessionUUID <- reactiveVal(NULL)
  sessionFile <- reactiveVal(NULL)
  sessionXML <- reactiveVal(NULL)
  sessionUserBookmarks <- reactiveVal(NULL)
  # Define reactive value for reactive filtering on search tabs
  metadataFilterReactive <- reactiveVal()
  hypothesisExploreReactive <- reactiveVal()
  
  #KNB Token
  token_info <- reactiveVal(NULL)
  # logged on user information
  user_info <- reactiveVal(NULL)
  user_info(tibble::tibble(result = FALSE,admin = FALSE))
  
  # drop-down menu lists 
  
  lsfDomains <- reactiveVal(NULL)
  lsfMetadata <- reactiveVal(NULL)
  lsfHypotheses <- reactiveVal(NULL)
  lsfVariableClasses <- reactiveVal(NULL)
  ############################
  # Reactive Values END
  ############################
  
  # Send user to registration page if URL contains register query ?register
  # Send user to search page if URL contains ?search (figure out how to auto-prompt logon too)
  # Send user to submit page if URL contains ?submit (figure out how to auto-prompt logon too)
  # Send user to research activity page is URL contains ?newproject
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['register']])) {
      updateTabItems(session, 'menu1', 'newMemberRegistration')
    }else if (!is.null(query[['search']])) {
      updateTabItems(session, 'menu1', 'searchlsf')
    }else if (!is.null(query[['submit']])) {
      updateTabItems(session, 'menu1', 'newsource')
    }else if (!is.null(query[['newproject']])) {
      updateTabItems(session, 'menu1', 'newproject')
    }
  })
  
  ############################
  # header items
  ############################
  
  source("./src/server/headerLogon_server.R", local = TRUE)$value

  ############################
  # header items END
  ############################
  
  ############################
  # Sidebar items
  ############################
  
  source("./src/server/sidebar_server.R", local = TRUE)$value
  updateTabItems(session, 'menu1', 'searchlsf') # this line ensures that the intro tab is selected on initial load, required after moving to renderUI sidebar items
  ############################
  # Sidebar items END
  ############################
  
  ############################
  # introduction tab START
  ############################
  
  source("./src/server/introductionTab_server.R", local = TRUE)$value
  
  ############################
  # introduction tab END
  ############################
  
  ############################
  # newUserRegistrationTab_server.R tab START
  ############################
  
  source("./src/server/newUserRegistrationTab_server.R", local = TRUE)$value
  
  ############################
  # newUserRegistrationTab_server.R tab END
  ############################
  
  ############################
  # submitDataSource_server.R START
  ############################
  
  source("./src/server/submitDataSource_server.R", local = TRUE)$value
  
  ############################
  # submitDataSource_server.R END
  ############################
  
  ############################
  # domainExplore_server.R START
  ############################
  
  # Load database information
  source("./src/server/dataLoad_server.R",local = TRUE)
  source("./src/server/exploreMetadata_server.R", local = TRUE)$value
  ############################
  # domainExplore_server.R END
  ############################
  
  ############################
  # submitResearchActivity_server.R START
  ############################
  
  source("./src/server/submitResearchActivity_server.R", local = TRUE)$value
  
  ############################
  # submitResearchActivity_server.R END
  ############################
  
  ############################
  # administrationArea_server.R START
  ############################
  
  source("./src/server/administrationArea_server.R", local = TRUE)$value
  source("./src/server/qcDataSource_server.R", local = TRUE)$value
  
  ############################
  # administrationArea_server.R END
  ############################
  
  #modalDialogs!
  
  source("./src/server/modalDialogs_server.R", local = TRUE)$value
  
  #popovers!
  
  source("./src/server/infoPopOvers_server.R", local = TRUE)$value
  

  
}