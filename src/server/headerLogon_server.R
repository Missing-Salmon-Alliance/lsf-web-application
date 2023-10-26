##############################################################
# Logon and Logout observers + Header Item Rendered UI and Observers
##############################################################

# reactive that triggers conditionalPanels throughout the site, hiding elements if user is not logged on
output$logonTrue <- reactive({
  user_info()$result
})
# required options to ensure reactive is triggered even though it is not attached to a UI element
outputOptions(output, 'logonTrue', suspendWhenHidden = FALSE)


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
                  actionLink('registerModal1',strong("or register here"))),
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
observeEvent(input$registerModal1, {
  removeModal()
  updateTabItems(session, 'menu1', 'newMemberRegistration')
})
observeEvent(input$registerModal2, {
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
    # send user to search page
    updateTabItems(session, 'menu1', 'searchlsf')
    
  }else{
    # logon fail, add red fail text to modal
    user_info(NULL)
    output$credentialsIncorrect <- renderUI(span(style="color:red", "username or password incorrect"))
    # send user back to introduction page
    updateTabItems(session, 'menu1', 'introduction')
  }
  
  
  
})

# observe logout button click - action: show thank you message, log user out and return to intro screen
observeEvent(input$logoutModal, {
  # Capture users bookmarks so it can be retained for next time they log in
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.personBookmarks = '",formatNumericList(sessionUserBookmarks()),"';"),con = neo_con, type = 'row')
  # clear user information
  user_info(NULL)
  user_info(tibble::tibble(result = FALSE,admin = FALSE))
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