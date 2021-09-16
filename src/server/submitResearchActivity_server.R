fields = c("activityTitle", "activityWPTitle", "activitySummary", "activityURL", "activityStatus", "activityStartYear", "activityEndYear", "activityCreated")
# Field name convert from SQL names to Friendly Names
newNames = c("Main Project Title","Work Package Title","Summary","URL","Status","Start Year","End Year","Date Added")

activityDFReactive <- reactiveVal()
activityDFReactive(loadData())

output$activityDF <- DT::renderDataTable({activityDFReactive()[,fields]},
                                         rownames = FALSE,
                                         colnames = newNames,
                                         options = list(order = list(7, 'desc')) # order by 7th column (created), note 0 index
)

observeEvent(input$refreshProjectTable,
             {activityDFReactive(loadData())}
)


# Create a dataframe out of the form data, project detail only
responseDF <- reactive({
  if(input$startUnknown){startDate <- "unknown-unknown"}
  else{startDate <- paste(input$startYear,input$startMonth,sep="-")}
  if(input$endUnknown){endDate <- "unknown-unknown"}
  else{endDate <- paste(input$endYear,input$endMonth,sep="-")}
  if(input$iasrb){source <- 'iasrb'}else{source <- 'lsf'}
  data.frame(activityTitle = sanitiseFreeTextInputs(input$projectTitle),
             activityWPTitle = sanitiseFreeTextInputs(input$workPackageTitle),
             activitySummary = sanitiseFreeTextInputs(input$summary),
             activityURL = sanitiseFreeTextInputs(input$urls),
             activityStatus = input$status,
             activityStartMonth = stringr::str_split(startDate,"-",simplify = T)[2],
             activityStartYear = stringr::str_split(startDate,"-",simplify = T)[1],
             activityEndMonth = stringr::str_split(endDate,"-",simplify = T)[2],
             activityEndYear = stringr::str_split(endDate,"-",simplify = T)[1],
             activitySource = source,
             stringsAsFactors = FALSE
  )
})

responsePerson <- reactive({
  data.frame(personAffiliation = sanitiseFreeTextInputs(input$organisations),
             personName = sanitiseFreeTextInputs(input$coordinator),
             personEmail = sanitiseFreeTextInputs(input$email),
             stringsAsFactors = FALSE
  )
})

# Output transposed dataframe in the submission review pop up (modal)
output$modalDataFrame1 <- shiny::renderTable({t(responseDF())}, rownames = TRUE, colnames = FALSE)
output$modalDataFrame2 <- shiny::renderTable({t(responsePerson())}, rownames = TRUE, colnames = FALSE)

# function for creating the pop up (modal) window as a confirmation screen for users submitting a project
# Displays the data they have entered in a table and gives them a choice to submit or to cancel and edit
dataModal <- function() {
  modalDialog(size = "l",title = "Thank you for your submission!",
              h4("Please review the details and confirm to save, or cancel to go back and edit."),
              shiny::tableOutput("modalDataFrame1"),
              shiny::tableOutput("modalDataFrame2"),
              formatLSFCategories(input$theme),
              br(),
              formatLSFCategories(input$mortalityFocus),
              br(),
              h4("debugging"),
              paste0("CREATE ",
                     neo4r::vec_to_cypher_with_var(as.list(responseDF()),'Activity','a'),
                     " SET a.activityLabel = 'temporaryLabel', a.activityCreated = '",
                     format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                     "';"),
              
              footer = tagList(
                modalButton("Cancel"),
                actionButton("confirmSubmit", "Confirm", class = "btn-success")
              )
  )
}

# Show modal when "Submit Project" button is pressed.
observeEvent(input$submitProject, {
  showModal(dataModal())
})

# When user confirms submission, run save data routine and reset form fields ready for next submission
observeEvent(input$confirmSubmit,
             {
               # saves data using function from global.R
               saveData(responseDF(),responsePerson(),input$theme,input$mortalityFocus)
               # closes pop up (modal)
               removeModal()
               # resets form
               updateCheckboxInput(session,"iasrb",value = FALSE)
               updateTextInput(session,"projectTitle",value = "")
               updateTextInput(session, "workPackageTitle",value = "")
               updateTextInput(session,"coordinator",value = "")
               updateTextInput(session,"organisations",value = "")
               updateTextInput(session,"summary",value = "")
               updateTextInput(session,"email",value = "")
               updateTextInput(session,"urls",value = "")
               updateRadioButtons(session,"status",selected = "Proposed")
               updateSelectInput(session,"startYear",selected = format(Sys.time(),"%Y")) # resets year input to this year
               updateSelectInput(session,"startMonth",selected = format(Sys.time(),"%b")) # resets year input to this year
               updateSelectInput(session,"endYear",selected = format(Sys.time(),"%Y")) # resets month input to this month
               updateSelectInput(session,"endMonth",selected = format(Sys.time(),"%b")) # resets month input to this month
               updateCheckboxInput(session,"startUnknown",value = TRUE)
               updateCheckboxInput(session,"endUnknown",value = TRUE)
               updateCheckboxGroupInput(session,"theme",selected = FALSE) # clears all checked boxes
               updateCheckboxGroupInput(session,"mortalityFocus",selected = FALSE) # clears all checked boxes
             })


observeEvent(input$startUnknown,
             {
               if(input$startUnknown){
                 shinyjs::disable("startMonth")
                 shinyjs::disable("startYear")
               }
               else
               {
                 shinyjs::enable("startMonth")
                 shinyjs::enable("startYear")
               }
               
             })

observeEvent(input$endUnknown,
             {
               if(input$endUnknown){
                 shinyjs::disable("endMonth")
                 shinyjs::disable("endYear")
               }
               else
               {
                 shinyjs::enable("endMonth")
                 shinyjs::enable("endYear")
               }
               
             })
# Input box max lengths
shinyjs::runjs("$('#projectTitle').attr('maxlength', 150)")
shinyjs::runjs("$('#workPackageTitle').attr('maxlength', 150)")
shinyjs::runjs("$('#summary').attr('maxlength', 500)")
shinyjs::runjs("$('#organisations').attr('maxlength', 200)")
shinyjs::runjs("$('#urls').attr('maxlength', 200)")
shinyjs::runjs("$('#coordinator').attr('maxlength', 50)")
shinyjs::runjs("$('#email').attr('maxlength', 50)")
