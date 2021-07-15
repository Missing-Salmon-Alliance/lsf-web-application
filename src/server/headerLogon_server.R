################################
# Logon and Logout observers
################################

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
  req(user_info())
  if (user_info()$result) { # note div in label string. This is used to change the order of text and icon
  actionLink('userInfoModal',label = str_to_title(user_info()$user_info$fullname),icon = icon('id-card'),style='padding:5px; font-size:120%; color:white;float:right;')
  }
})

# observe logon button click - action use modal to prompt for username and password or for new user registration
observeEvent(input$loginModal, {
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
    # Autofill some fields in the submit page
    updateTextInput(session, 'sourceCreator', value = user_info()$user_info$fullname)
    updateTextInput(session, 'sourceCreatorEmail', value = user_info()$user_info$email)
    updateTextInput(session, 'sourceOrganisation', value = user_info()$user_info$affiliation)
    # Autofill some fileds in the checkout page
    updateTextInput(session, 'basketName', value = user_info()$user_info$fullname)
    updateTextInput(session, 'basketOrganisation', value = user_info()$user_info$affiliation)
    removeModal()
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
  # Capture users basket so it can be retained for next time they log in
  neo4r::call_neo4j(query = paste0("MATCH (p:Person) WHERE id(p) = ",user_info()$user_info$id," SET p.liveBasket = '",formatNumericList(sessionUserBasket()),"';"),con = neo_con, type = 'row')
  # clear user information
  user_info(NULL)
  # clear basket
  sessionUserBasket(c())
  # change logout button into login button
  shinyjs::hideElement('logoutModal')
  shinyjs::showElement('loginModal')
  # goodbye message
  showModal(
    modalDialog(title = "Logged Out",
                p("Thank you for visiting the Likely Suspects Framework for Atlantic Salmon Central Data Resource"),
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
  user_info()$user_info$requested
  #TODO: include request status e.g. fulfilled and created/lastModified dates
  
})

output$submitHistory <- DT::renderDT({
  req(user_info())
  user_info()$user_info$submitted
  #TODO: include submission status e.g. QC pending and created/lastModified dates
  
})
