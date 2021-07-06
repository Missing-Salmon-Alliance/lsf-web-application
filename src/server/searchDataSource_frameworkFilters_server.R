######################################################
# Framework filters - Hypothesis, Domain, and Variable Class

################################
# Hypothesis/Variable Class filters
################################
# observe Mortality Hypothesis Filter - Action: populate Sub-Hypothesis filter
observeEvent(input$hypothesisFilter,{
  #prevent run when filter reset to character(0) by using req()
  req(input$hypothesisFilter)
  # Domain Filter and Hypothesis filter are currently mutually exclusive
  # So first step, reset domainFilter filter to blank
  updateSelectizeInput(session, 'domainFilter',selected = character(0))
  # capture user remove all values from filter, reset dependent filters
  if(is.null(input$hypothesisFilter) || input$hypothesisFilter == ""){
    updateSelectizeInput(session, 'subHypothesisFilter', choices = c("Please select a Mortality Hypothesis"),selected = "Please select a Mortality Hypothesis")
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
    updateSelectizeInput(session, 'esvFilterBioHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis")
    updateSelectizeInput(session, 'esvFilterPhysHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis")
    updateSelectizeInput(session, 'esvFilterTraitHyp', choices = c("Please select a Sub-Hypothesis"),selected = "Please select a Sub-Hypothesis")
  }else{
    # capture filtered subhypothesis ID's as vector
    filteredSubHypothesisIds <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$id
    # capture class ids (endNodes) from filtered Relationships as vector
    filteredESVIdsHyp <- hypothesisSubHypothesisESVRelationships[hypothesisSubHypothesisESVRelationships$startNode %in% filteredSubHypothesisIds,]$endNode
    # update select input using filtered class Titles
    updateSelectizeInput(session, 'esvFilterBioHyp', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Biological" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = "")
    updateSelectizeInput(session, 'esvFilterPhysHyp', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Physical" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = "")
    updateSelectizeInput(session, 'esvFilterTraitHyp', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Salmon Trait" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIdsHyp,]$esvTitle),selected = "")
  }
},ignoreNULL = FALSE)

# Observe class Filter - Action: Update metadata nodes displayed on map (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioHyp,{
  req(input$esvFilterBioHyp)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator
  
  if(input$esvFilterBioHyp == "Please select a Sub-Hypothesis" || input$esvFilterBioHyp == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    #Clear out other two class drop-downs TODO only one at a time at the moment - To Be Improved!
    updateSelectizeInput(session,'esvFilterPhysHyp',selected = character(0))
    updateSelectizeInput(session,'esvFilterTraitHyp',selected = character(0))
    # filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterBioHyp,]$id
    
    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment
    
    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
},ignoreNULL = FALSE)

# PHYSICAL classes
observeEvent(input$esvFilterPhysHyp,{
  req(input$esvFilterPhysHyp)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator
  if(input$esvFilterPhysHyp == "Please select a Sub-Hypothesis" || input$esvFilterPhysHyp == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    updateSelectizeInput(session,'esvFilterTraitHyp',selected = character(0))
    updateSelectizeInput(session,'esvFilterBioHyp',selected = character(0))
    # filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterPhysHyp,]$id
    
    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment
    
    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
},ignoreNULL = FALSE)

# SALMON TRAIT classes
observeEvent(input$esvFilterTraitHyp,{
  req(input$esvFilterTraitHyp)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator
  if(input$esvFilterTraitHyp == "Please select a Sub-Hypothesis" || input$esvFilterTraitHyp == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    updateSelectizeInput(session,'esvFilterPhysHyp',selected = character(0))
    updateSelectizeInput(session,'esvFilterBioHyp',selected = character(0))
    # filter classes including domain subset too
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterTraitHyp,]$id
    
    filteredSubHypEnv <- LSFSubHypothesisTibble[LSFSubHypothesisTibble$subHypothesisTitle == input$subHypothesisFilter,]$subHypothesisEnvironment
    
    filteredDomainTitles <- LSFDomainTibble[LSFDomainTibble$domainEnvironment == filteredSubHypEnv,]$domainTitle
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% filteredDomainTitles),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
  
},ignoreNULL = FALSE)

################################
# Domain/Var Class filters
################################

# Observe Domain Filter - Action: Update Variable Class Filters (available class choices filtered by Domain)
observeEvent(input$domainFilter,{
  req(input$domainFilter)
  # Domain Filter and Hypothesis filter are currently mutually exclusive
  # So first step, reset hypothesisFilter filter to blank
  updateSelectizeInput(session, 'hypothesisFilter',selected = character(0))
  updateSelectizeInput(session, 'subHypothesisFilter', choices = c("Please select a Mortality Hypothesis"),selected = "Please select a Mortality Hypothesis")
  
  if(is.null(input$domainFilter)){
    updateSelectizeInput(session, 'esvFilterBioDom', choices = c("Please select a Domain"),selected = "Please select a Domain")
    updateSelectizeInput(session, 'esvFilterPhysDom', choices = c("Please select a Domain"),selected = "Please select a Domain")
    updateSelectizeInput(session, 'esvFilterTraitDom', choices = c("Please select a Domain"),selected = "Please select a Domain")
  }else{
    # capture filtered domain ID's as vector
    filteredDomainIds <- LSFDomainTibble[LSFDomainTibble$domainTitle %in% input$domainFilter,]$id
    # capture class ids (startNodes) from filtered Relationships as vector
    filteredESVIds <- metadataESVDomainRelationships[metadataESVDomainRelationships$endNode %in% filteredDomainIds,]$startNode
    # update select input using filtered class Titles
    updateSelectizeInput(session, 'esvFilterBioDom', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Biological" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = "")
    updateSelectizeInput(session, 'esvFilterPhysDom', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Physical" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = "")
    updateSelectizeInput(session, 'esvFilterTraitDom', choices = c("",LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Salmon Trait" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = "")
  }
},ignoreNULL = FALSE)

# Observe Var Class Filter - Action: Update metadata nodes displayed on map (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioDom,{
  req(input$esvFilterBioDom)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator

  if(input$esvFilterBioDom == "Please select a Domain" || input$esvFilterBioDom == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    updateSelectizeInput(session,'esvFilterPhysDom',selected = character(0))
    updateSelectizeInput(session,'esvFilterTraitDom',selected = character(0))
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterBioDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
},ignoreNULL = FALSE)

# PHYSICAL Classes
observeEvent(input$esvFilterPhysDom,{
  req(input$esvFilterPhysDom)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator
  if(input$esvFilterPhysDom == "Please select a Domain" || input$esvFilterPhysDom == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    updateSelectizeInput(session,'esvFilterTraitDom',selected = character(0))
    updateSelectizeInput(session,'esvFilterBioDom',selected = character(0))
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterPhysDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
},ignoreNULL = FALSE)

# SALMON TRAIT Classes
observeEvent(input$esvFilterTraitDom,{
  req(input$esvFilterTraitDom)
  # clear existing markers to make way for filtered markers
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  # Note OR operator
  if(input$esvFilterTraitDom == "Please select a Domain" || input$esvFilterTraitDom == ""){
    # leaflet update here - show all?
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    updateSelectizeInput(session,'esvFilterPhysDom',selected = character(0))
    updateSelectizeInput(session,'esvFilterBioDom',selected = character(0))
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterTraitDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
  
},ignoreNULL = FALSE)