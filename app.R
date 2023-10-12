### Global Section

require(shiny)
require(shinydashboard) # css styles
require(shinydashboardPlus) # advanced features
#require(neo4r)# interact with neo4j graph db
#require(visNetwork)# view and manipulate networks
#require(shinyWidgets)# create different input styles
#require(dplyr)
#require(DT)
#require(readr)
#require(RPostgres)
#require(DBI)#interact with database
#require(leaflet)# create maps
#require(rgdal)# map calcs
#require(sf)
#require(dataone)# interact with knb/dataone datasets
#require(stringr)# used to interact with dataone/knb API
require(shinyBS)# create modals
#require(aws.s3)# s3 storage R api
#require(uuid)
#require(crosstalk)
#require(shinyjs)
require(tidyverse)
#require(plotly)
#require(xml2)
#require(shinybusy)
#require(data.table)
#require(leaflet.extras)

source("./src/secrets.R",local = TRUE)
source("./src/custom_functions.R",local = TRUE)
## Create NEO4J Connection object

neo_con <- neo4r::neo4j_api$new(url = paste("http://",NEO4J_HOST,":",NEO4J_PORT,sep = ""),
  user = NEO4J_USER,
  password = NEO4J_PASSWD)

# Set up waiter that covers search map during db updates
initDataLoad_Metadata <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading the SalHub Metadata Database...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_ICES <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading EcoRegion GIS Data from ICES...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_NAFO <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading Division GIS Data from NAFO...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_Rivers <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading River GIS Data from ICES and NASCO...</h4></div>"),
  color = waiter::transparent(.7)
)
initDataLoad_GIS_LSF <- waiter::Waiter$new(
  html = HTML("<div class='container--box'><div class='boxxy'><div class='spinner spinner--3'></div></div></div><div><h4 style='color:black;'>One moment, we're just loading final GIS Data... Almost Done! :)</h4></div>"),
  color = waiter::transparent(.7)
)

### User Interface created using shinydashboard package
ui <- dashboardPage(skin = "blue",
  title = "Missing Salmon Alliance", # This is the title that appears in the browser tab name
  header = dashboardHeader(
    # Set MSA logo as title and clickable link to MSA website
    # https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
    # title = tags$a(href='https://missingsalmonalliance.org',
    #                tags$img(src='The_missing_salmon_type-394x100.png'), # note, logo stored in www folder
    #                target="_blank"), # opens the link in a new tab/window
    title = tags$img(class = 'logo-lg',src = './images/SalHub_full_394x100.png',width = 394),
    titleWidth = 394,
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
    # Conditional sidebar menu items controlled from server file sidebar_server.R
    uiOutput('menu')
  ),
  body = dashboardBody(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyjs::extendShinyjs(script = 'custom.js', functions = c('markerClick')),
    includeCSS('www/custom.css'),
    shinyBS::bsPopover("test","test"), # bit of a bodge, server defined popovers don't seem to work without this line
    
    #################################
    # New User Registration UI
    #################################
    tabItem(tabName = 'newMemberRegistration',
            h2("Data Sharing Group New Member Registration"),
            h4("By registering you are accepting the terms in the Data Sharing Memorandum of Agreement."),
            br(),
            box(
              width = 4,
              status = 'primary',
              uiOutput('registerFieldBlank'),
              textInput('newUserName',"Name:"),
              textInput('newUserEmail',"Email:"),
              textInput('newUserOrganisation',"Affiliation, Organisation or Institute:"),
              hr(),
              checkboxInput('dataSharingAgreementAccept',
                            "I have read and agree to abide by the terms and guiding principles set out in the Data Sharing Group Memorandum of Agreement",
                            value = FALSE),
              checkboxInput('dataSharingAgreementAdvertiseOrganisation',
                            "Organisations that are part of the Data Sharing Group can appear on the Salmon Ecosystem Data Hub as contributors. If you would like your organisation to be shown, please tick here.",
                            value = FALSE),
              hr(),
              actionButton('newUserRegisterSubmit',"Submit")
            ),
            box(
              width = 8,
              title = "Please review this agreement before proceeding.",
              status = 'warning',
              includeMarkdown("src/DataSharingMoA.Rmd")
            ))
    #################################
    # New User Registration UI
    #################################
  ), # dashboardBody close

  controlbar = dashboardControlbar(collapsed = TRUE,    # three breaks needed to bring buttons lower than enlarged header bar height
    br(),
    br(),
    br()) # RIGHT Hand Sidebar
) # dashboardPage close

server <- function(input, output, session) {
  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)
  
  # Load databases
  initDataLoad_Metadata$show()
  lsfDomains <- reactiveVal(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder))
  lsfMetadata <- reactiveVal(sf::st_as_sf(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  lsfHypotheses <- reactiveVal(neo4r::call_neo4j("MATCH (h:Hypothesis) RETURN h;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
  lsfVariableClasses <- reactiveVal(neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle))
  initDataLoad_Metadata$hide()

  initDataLoad_GIS_ICES$show()
  ICES_Ecoregions <<- loadFullWKBData("ices_ecoregions_simplified")
  initDataLoad_GIS_ICES$hide()
  
  initDataLoad_GIS_NAFO$show()
  nafoDivisionsSF <<- loadFullWKBData("nafo_divisions")
  initDataLoad_GIS_NAFO$hide()
  
  
  initDataLoad_GIS_Rivers$show()
  riversDBSF <<- loadFullWKBData('rivers_db')
  nascoRiversDBSF <<- riversDBSF[riversDBSF$nasco_rivers_db == TRUE,] # subset of rivers that were sourced from the original NASCO DB
  indexRiversSF <<- riversDBSF[riversDBSF$ices_index == TRUE,] # subset of rivers that are ICES index rivers
  initDataLoad_GIS_Rivers$hide()
  
  initDataLoad_GIS_LSF$show()
  salmosalarRange <<- loadFullWKBData('atlantic_salmon_range')
  salmosalarExtents <<- sf::st_bbox(salmosalarRange)
  initDataLoad_GIS_LSF$hide()
  
  ##############################################################
  # Logon and Logout observers + Header Item Rendered UI and Observers
  ##############################################################
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
  # reactive that triggers conditionalPanels throughout the site, hiding elements if user is not logged on
  output$logonTrue <- reactive({
    user_info()$result
  })
  # required options to ensure reactive is triggered even though it is not attached to a UI element
  outputOptions(output, 'logonTrue', suspendWhenHidden = FALSE)
  # drop-down menu lists 
  lsfDomains <- reactiveVal(neo4r::call_neo4j("MATCH (d:Domain) RETURN d;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(domainOrder))
  lsfMetadata <- reactiveVal(sf::st_as_sf(neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'), wkt = "metadataCoverageCentroid", crs = 4326, na.fail = FALSE))
  lsfHypotheses <- reactiveVal(neo4r::call_neo4j("MATCH (h:Hypothesis) RETURN h;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all'))
  lsfVariableClasses <- reactiveVal(neo4r::call_neo4j("MATCH (esv:EssentialSalmonVariable) RETURN esv;",neo_con,type='graph')$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::arrange(esvCategory,esvTitle))
  
  ############################
  # Reactive Values END
  ############################
  
  #########################################################
  #https://rstudio.github.io/shinydashboard/appearance.html
  # Create an indication who the current logged in user is
  output$userpanel <- renderUI({
    req(user_info()) # only action if user_info has been created
    if (user_info()$result) { # if user logon is true:
      actionLink('userInfoModal',label = str_to_title(user_info()$user_info$fullname),icon = icon('id-card'),style='padding:5px; font-size:120%; color:white;float:right;')
    }
  })
  
  # observe logon button click - action use modal to prompt for username and password or for new user registration
  observeEvent(input$loginModal | input$introSideLogonButton, {
    # req prevents trigger on load
    req(input$loginModal != 0 || input$introSideLogonButton != 0)
    showModal(
      modalDialog(title = "Login",
                  p("Please enter your email and password",
                    actionLink('registerModal',strong("or register here"))),
                  uiOutput('credentialsIncorrect'),
                  textInput('username',"Email:"),
                  passwordInput('password',"Password:"),
                  hr(),
                  actionButton('loginSubmit',"Login")
      )
    )
  })
  
  
  # observe new user registration click - action: send user to hidden tabItem containing registration details
  # TODO: change from saving as CSV to producing some kind of email to data admin inbox
  observeEvent(input$registerModal, {
    removeModal()
    updateTabItems(session, 'menu1', 'newMemberRegistration')
  })
  
  # observe login submit button - action: check credentials on DB
  # if success logon user
  # if fail show modal again with error message
  observeEvent(input$loginSubmit, {
    
    logonResult <- checkUserCredentials(input$username,input$password)
    user_info(logonResult)
    
    if(user_info()$result){
      # logon success
      shinyjs::hideElement('loginModal')
      shinyjs::showElement('logoutModal')
      shinyjs::disable('introSideLogonButton')
      # Autofill some fields in the checkout page
      updateTextInput(session, 'requestName', value = user_info()$user_info$fullname)
      updateTextInput(session, 'requestOrganisation', value = user_info()$user_info$affiliation)
      # Pull in users saved bookmarks and populate bookmarks list
      if(user_info()$user_info$bookmarks != ""){
        sessionUserBookmarks(stringr::str_split(user_info()$user_info$bookmarks,",",simplify = T)[1,])
      }else{
        sessionUserBookmarks(c())
      }
      removeModal()
      
      # Load database information upon successful log on
      
      source("./src/server/dataLoad_server.R",local = TRUE)
      
    }else{
      # logon fail, add red fail text to modal
      user_info(NULL)
      output$credentialsIncorrect <- renderUI(span(style="color:red", "username or password incorrect"))
    }
    # send user back to introduction page regardless of login success
    # To fix a bug where conditional sidebar items fail to appear if user logs in whilst a non-intro tab is active
    updateTabItems(session, 'menu1', 'introduction')
    
  })
  
  # observe logout button click - action: show thank you message, log user out and return to intro screen
  observeEvent(input$logoutModal, {
    # Capture users bookmarks so it can be retained for next time they log in
    neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
    # clear user information
    user_info(NULL)
    # clear bookmarks
    sessionUserBookmarks(c())
    # change logout button into login button
    shinyjs::hideElement('logoutModal')
    shinyjs::showElement('loginModal')
    shinyjs::enable('introSideLogonButton')
    # goodbye message
    showModal(
      modalDialog(title = "Logged Out",
                  p("Thank you for visiting the Salmon Ecosystem Data Hub"),
                  p("We hope you found this interface useful and will return soon"),
                  p("All feedback welcome via either registering an issue/bug or feature request at our",
                    a(href="https://github.com/Missing-Salmon-Alliance/lsf-web-application","github repository"),
                    "or by directly emailing ",a(href="mailto: data.admin@missingsalmonalliance.org","the team."))
                  
      )
    )
    updateTabItems(session, 'menu1', 'introduction')
  })
  
  observeEvent(input$userInfoModal, {
    showModal(
      modalDialog(title = "Account Information", size = 'l',
                  h4("Your details:"),
                  p(strong("Name: "),user_info()$user_info$fullname),
                  p(strong("Affiliation: "),user_info()$user_info$affiliation),
                  p(strong("Email: "),user_info()$user_info$email),
                  p("If any of the above details are incorrect please contact ",a(href="mailto: data.admin@missingsalmonalliance.org","the Data Administrator.")),
                  hr(),
                  tabsetPanel(
                    tabPanel(title = "Request history",
                             DT::DTOutput('requestHistory')),
                    tabPanel(title = "Submit history",
                             DT::DTOutput('submitHistory'))
                  )
                  
      )
    )
  })
  
  output$requestHistory <- DT::renderDT({
    req(user_info())
    user_info()$user_info$requested[,c('metadataTitle','metadataAbstract')]
    #TODO: include request status e.g. fulfilled and created/lastModified dates
    
  })
  
  output$submitHistory <- DT::renderDT({
    req(user_info())
    user_info()$user_info$submitted[,c('metadataTitle','metadataAbstract')]
    #TODO: include submission status e.g. QC pending and created/lastModified dates
    
  })
  
  # Creation of a DT which shows the contents of the Bookmarks
  output$bookmarkContentsTable <- DT::renderDT({
    lsfMetadata()[lsfMetadata()$id %in% sessionUserBookmarks(),c("id","metadataTitle","metadataCoverageCentroid")]
  },
  selection = 'single',
  rownames = FALSE,
  editable = FALSE,
  colnames = c("ID","Title","Centroid"),
  options = list(pageLength = 7,
                 searching = F,
                 lengthChange = F,
                 info = FALSE,
                 columnDefs = list(list(visible=FALSE, targets=c(2))) # hides Centroid column (column 2)
  )
  
  )
  
  # Remove a Single Bookmark Item using Tables_Rows_selected
  
  observeEvent(input$clearRows,{
    
    if (!is.null(input$bookmarkContentsTable_rows_selected)) {
      rowToBeRemoved <- input$bookmarkContentsTable_rows_selected
      idToBeRemoved <- lsfMetadata()[lsfMetadata()$id %in% sessionUserBookmarks(),]$id[rowToBeRemoved]
      sessionUserBookmarks(sessionUserBookmarks()[sessionUserBookmarks() != idToBeRemoved])
      # update database bookmarks
      neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
    }
  })
  
  # Clear the Bookmark using Action Button
  observeEvent(input$clearBookmarks, {
    sessionUserBookmarks(c())
    # update database bookmarks
    neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
  })
  
  # Render UI which includes both an action link, and a count of the Bookmark Length. 
  
  output$bookmarkUI <- renderUI({
    req(user_info())
    if (user_info()$result) {
      dynamicLabel <- paste("Bookmarks:",length(unique(sessionUserBookmarks())))
      actionLink(inputId = "bookmarks", label = dynamicLabel ,icon = icon("bookmark"),style='padding:5px; font-size:120%; color:white;float:right;')
    }
  })
  
  # bookmarks modal
  # TODO: Move all modals defined in UI files to observers in SERVER files
  # TODO: Alternatively all modals could reside in a single file such as searchDataSource_modals_server.R for easy access
  observeEvent(input$bookmarks,{
    showModal(
      modalDialog(title = "Your Bookmarks", size = "l",
                  column(width = 12,
                         DT::DTOutput('bookmarkContentsTable'),
                         column(width = 6,actionButton('clearBookmarks', "Clear All Bookmarked Sources")),
                         column(width = 5,actionButton('clearRows', "Delete Selected Row")),
                         hr()
                  ),
                  column(
                    width = 12,
                    h4("Request Bookmarked Data - Under Development"),
                    p("When you fill out and submit this form the data manager will attempt to arrange access to the requested data. Note that not all sources have guaranteed availability."),
                    br(),
                    textInput('requestName', "Name:", value = user_info()$user_info$fullname),
                    textInput('requestOrganisation', "Organisation:", value = user_info()$user_info$affiliation),
                    #selectInput("requestPosition", "Position/Occupation:", choices = c("Researcher","Database Manager","Government Official", "Conservationist", "Student", "Lecturer", "Other")),
                    #selectInput("requestDataUse", "What will the data be used for?", choices = c("Independent Research", "Conservation", "Guidance to Managers", "Other")),
                    #textInput("requestOther","Please describe what is meant, if you selected other"),
                    #selectInput("requestProvision", "Do you intend to provide data to the Salmon Ecosystem Data Hub?", choices = c("Yes", "No", "In the Future")),
                    textAreaInput('requestIntention', "Please describe the intended use for the data. Please include information on the project, time scale of usage and expected number of users.", width = "1000px", height = "50px"),
                    
                    actionButton('sendRequest', "Send Data Request")
                    
                  )
                  
      )
    )
  })
  
  #######################
  # Sidebar menu UI
  #######################
  output$menu <- renderUI(
    if(!is.null(user_info()) && user_info()$admin){
      sidebarMenu(id="menu1",
                  
                  # Admin Menu items
                  checkboxInput('debug',"Debug Info"),# DEBUG - Tick box to show input raw outputs
                  menuItem("Introduction", tabName = 'introduction', icon = icon('info',verify_fa = FALSE)),
                  shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus',verify_fa = FALSE))),
                  menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram',verify_fa = FALSE)),
                  menuItem("Search and Explore",tabName = 'searchlsf', icon = icon('search-location',verify_fa = FALSE)),
                  menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram',verify_fa = FALSE)),
                  menuItem("Administration", tabName = 'adminZone', icon = icon('cogs',verify_fa = FALSE))
                  
                  
      )
    }else{
      sidebarMenu(id="menu1",
                  
                  # Basic Menu items
                  menuItem("Introduction", tabName = 'introduction', icon = icon('info')),
                  shinyjs::hidden(menuItem("New Member Registration",tabName = 'newMemberRegistration',icon = icon('user-plus',verify_fa = FALSE))),
                  menuItem("Submit Data Source", tabName = 'newsource', icon = icon('project-diagram',verify_fa = FALSE)),
                  menuItem("Search and Explore",tabName = 'searchlsf', icon = icon('search-location',verify_fa = FALSE)),
                  menuItem("Submit Research Project", tabName = 'newproject', icon = icon('project-diagram',verify_fa = FALSE))
      )
    }
  )
  #######################
  # Sidebar menu UI ENDS
  #######################

}

shinyApp(ui, server)