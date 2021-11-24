################################
# Hypothesis/Variable Class filters
################################


output$hypothesisExploreTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    fluidPage(
      h4("Explore available data resources based on addressing a specific hypothesis."),
      p("To advance international efforts to uncover the reasons for downward trends in
      salmon abundance, the MSA Likely Suspects Framework team has been trying to focus
      scientific efforts onto identifying a group of key hypotheses relating to possible
      drivers of salmon mortality. Read more about this activity on the",a(href="https://missingsalmonalliance.org","MSA website.",target="_blank"),
        "The set of 11 resultant priority candidate hypotheses statements are presented here
      as a way to filter available data resources based on how applicable variable classes
      are to the selected hypothesis."),
      box(
        status = 'primary',
        title = "Step 1 - Hypotheses",
        width = 5,
        column(
          width = 12,
          selectizeInput('hypothesisFilter',
                         "Select a Mortality Hypothesis...",
                         choiceNames = c("",lsfHypotheses()$hypothesisTitle),
                         choiceVa
                         selected = "",
                         multiple = FALSE,
                         width = '100%'),
          selectizeInput('subHypothesisFilter',
                         "... and a Sub-Hypothesis",
                         choices = c("Please select a Mortality Hypothesis"),
                         selected = "Please select a Mortality Hypothesis",
                         multiple = FALSE,
                         width = '100%')
        )
      ),
      box(
        status = 'primary',
        title = "Step 2 - Variable Classes Relevant to Selected Hypothesis",
        width = 7,
        shinyWidgets::checkboxGroupButtons('esvFilterBioHyp',"Biological Processes",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                           justified = F,
                                           individual = T,
                                           status = "default",
                                           size = 'xs',
                                           checkIcon = checkboxGroupButtonsIcons),
        shinyWidgets::checkboxGroupButtons('esvFilterPhysHyp',"Physical Environment",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                           justified = F,
                                           individual = T,
                                           status = "default",
                                           size = 'xs',
                                           checkIcon = checkboxGroupButtonsIcons),
        shinyWidgets::checkboxGroupButtons('esvFilterTraitHyp',"Salmon Traits",choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                           justified = F,
                                           individual = T,
                                           status = "default",
                                           size = 'xs',
                                           checkIcon = checkboxGroupButtonsIcons)
      ),
      box(
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        title = "Step 3 - Results",
        DT::DTOutput('hypothesisExploreTable')
      )
    )
  }else{
    fluidPage(
      h1("Hypothesis Explore Area"),
      h3("Please authenticate to access this area")
    )
  }
})

# observe Mortality Hypothesis Filter - Action: populate Sub-Hypothesis filter
observeEvent(input$hypothesisFilter,{
  
  # capture user remove all values from filter, reset dependent filters
  if(is.null(input$hypothesisFilter) || input$hypothesisFilter == ""){
    updateSelectizeInput(session, 'subHypothesisFilter', choices = c("Please select a Mortality Hypothesis"),selected = "Please select a Mortality Hypothesis")
    hypothesisExploreReactive(NULL)
  }else{
    # capture filtered domain ID's as vector
    filteredHypothesisIds <- LSFHypothesisTibble[LSFHypothesisTibble$hypothesisTitle == input$hypothesisFilter,]$id
    # capture ESV ids (startNodes) from filtered Relationships as vector
    filteredSubHypothesisIds <- hypothesisSubHypothesisESVRelationships[hypothesisSubHypothesisESVRelationships$startNode %in% filteredHypothesisIds,]$endNode
    # create filtered tibble for choices
    filteredLSFSubHypothesisTibble <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$id %in% filteredSubHypothesisIds,]
    # update select input using filtered class Titles
    updateSelectizeInput(session, 'subHypothesisFilter', choices = c("",filteredLSFSubHypothesisTibble$subHypothesisTitle),selected = "")
  }
},ignoreNULL = FALSE)

# observe Sub Hypothesis Filter - Action: Update Variable Class 'Hyp' Filters (available class choices filtered by Sub Hypothesis)
observeEvent(input$subHypothesisFilter,{
  # Note OR operator
  if(is.null(input$subHypothesisFilter) || input$subHypothesisFilter == "" || input$subHypothesisFilter == "Please select a Mortality Hypothesis"){
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    hypothesisExploreReactive(NULL)
  }else{
    # capture filtered subhypothesis ID's as vector
    filteredSubHypothesisIds <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$id
    # capture class ids (endNodes) from filtered Relationships as vector
    filteredESVIdsHyp <- hypothesisSubHypothesisESVRelationships[hypothesisSubHypothesisESVRelationships$startNode %in% filteredSubHypothesisIds,]$endNode
    # update select input using filtered class Titles
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioHyp', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Biological" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysHyp', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Physical" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitHyp', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Salmon Trait" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
  }
},ignoreNULL = FALSE)

# Observe class Filter - Action: Update metadata displayed in hypothesisExplore table (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioHyp,{
  req(input$esvFilterBioHyp)
  
  if("Please select a Sub-Hypothesis" %in% input$esvFilterBioHyp){
    # leaflet update here - show all?
    hypothesisExploreReactive(NULL)
  }else{
    hypothesisExploreReactive(NULL)
    #Clear out other two class drop-downs TODO only one at a time at the moment - To Be Improved!
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterPhysHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterTraitHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    
    #filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterBioHyp,]$id

    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment

    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle

    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode

    hypothesisExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
},ignoreNULL = FALSE)

# PHYSICAL classes
observeEvent(input$esvFilterPhysHyp,{
  req(input$esvFilterPhysHyp)
  
  if("Please select a Sub-Hypothesis" %in% input$esvFilterPhysHyp){
    # leaflet update here - show all?
    hypothesisExploreReactive(NULL)
  }else{
    hypothesisExploreReactive(NULL)
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterTraitHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterBioHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    # filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterPhysHyp,]$id

    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment

    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle

    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode

    hypothesisExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
},ignoreNULL = FALSE)

# SALMON TRAIT classes
observeEvent(input$esvFilterTraitHyp,{
  req(input$esvFilterTraitHyp)
  
  if("Please select a Sub-Hypothesis" %in% input$esvFilterTraitHyp){
    # leaflet update here - show all?
    hypothesisExploreReactive(NULL)
  }else{
    hypothesisExploreReactive(NULL)
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterPhysHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterBioHyp',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    # filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterTraitHyp,]$id

    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment

    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle

    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode

    hypothesisExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
    
  }
},ignoreNULL = FALSE)

output$hypothesisExploreTable <- DT::renderDT({
    if(!is.null(hypothesisExploreReactive())){
      sf::st_set_geometry(hypothesisExploreReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
    }
  },
  selection = 'single',
  rownames = FALSE,
  editable = FALSE,
  colnames = c('Title','Abstract','Keywords'),
  options = list(pageLength = 20,
                 columnDefs = list(list(visible=FALSE, targets=c(2)))
  )
  
)
