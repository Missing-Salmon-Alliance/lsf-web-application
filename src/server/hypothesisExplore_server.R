################################
# Hypothesis/Variable Class filters
################################

hypothesisExploreReactive <- reactiveVal()
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
