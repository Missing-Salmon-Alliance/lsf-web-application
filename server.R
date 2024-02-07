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
  query_doi <- reactiveVal(353) # TESTING
  # Define reactive value for reactive filtering on search tabs
  metadataFilterReactive <- reactiveVal()
  hypothesisExploreReactive <- reactiveVal()
  
  #KNB Token
  token_info <- reactiveVal(NULL)
  # logged on user information
  user_info <- reactiveValues(result = FALSE,admin = FALSE, user_info = NULL)
  # trying to modify user_info object during user session other than logon/logoff has undesirable effects
  # create reactiveVal objects to hold user information which may need to be updated during user session
  userRequestedReactive <- reactiveVal(NULL)
  userSubmittedReactive <- reactiveVal(NULL)
  sessionUserBookmarks <- reactiveVal(NULL)
  
  # drop-down menu lists
  lsfDomains <- reactiveVal(NULL)
  lsfMetadata <- reactiveVal(NULL)
  lsfHypotheses <- reactiveVal(NULL)
  lsfVariableClasses <- reactiveVal(NULL)
  
  # Initialize a cache
  lsf_cache <- cachem::cache_mem(max_age = 360)
  
  ############################
  # Reactive Values END
  ############################
  
  ############################
  # Routines to enable URL direction to specific places on the site or data sources
  ############################
  # Send user to registration page if URL contains register query ?register
  # Send user to search page if URL contains ?search (figure out how to auto-prompt logon too)
  # Send user to submit page if URL contains ?submit (figure out how to auto-prompt logon too)
  # Send user to research activity page is URL contains ?newproject
  # Send user to specific data resource if URL contains ?doi=##
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$register)) {
      updateTabItems(session, 'menu1', 'newMemberRegistration')
    }else if (!is.null(query$search)) {
      updateTabItems(session, 'menu1', 'searchlsf')
    }else if (!is.null(query$submit)) {
      updateTabItems(session, 'menu1', 'newsource')
    }else if (!is.null(query$newproject)) {
      updateTabItems(session, 'menu1', 'newproject')
    }
  })
  
  o <- observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$doi)){
      updateTabItems(session, 'menu1', 'searchlsf')
      # select relevant row in data table
      # get row index and select that row
      row <- which(domainExploreReactive()$metadataUUID == query$doi)
      # find row in pages and select that page, plus and minus 1 in this line deal with end of page cases
      page <- (which(input$metadataExploreTable_rows_all == row) - 1) %/% input$metadataExploreTable_state$length + 1
      DT::dataTableProxy('metadataExploreTable') %>% DT::selectRows(row) %>% DT::selectPage(page)
    }else{ # if query$doi is null, simply destroy this observer
      o$destroy()
    }
    # observe will continually run, triggered by any reactive update, so must be destroyed as soon as it has done its job
    if(!is.null(input$metadataExploreTable_rows_selected)){
      if(input$metadataExploreTable_rows_selected == row){
        o$destroy()
      }
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