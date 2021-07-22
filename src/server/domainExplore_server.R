################################
# Domain/Var Class filters
################################

domainExploreReactive <- reactiveVal()
# Observe Domain Filter - Action: Update Variable Class Filters (available class choices filtered by Domain)
observeEvent(input$domainFilter,{
  #req(input$domainFilter)
  
  if(is.null(input$domainFilter)){
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioDom', choices = c("Please select a Domain"),selected = "Please select a Domain",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysDom', choices = c("Please select a Domain"),selected = "Please select a Domain",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitDom', choices = c("Please select a Domain"),selected = "Please select a Domain",
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
  }else{
    # capture filtered domain ID's as vector
    filteredDomainIds <- LSFDomainTibble[LSFDomainTibble$domainTitle %in% input$domainFilter,]$id
    # capture class ids (startNodes) from filtered Relationships as vector
    filteredESVIds <- metadataESVDomainRelationships[metadataESVDomainRelationships$endNode %in% filteredDomainIds,]$startNode
    # update select input using filtered class Titles
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioDom', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Biological" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysDom', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Physical" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitDom', choices = c(LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvCategory == "Salmon Trait" & LSFEssentialSalmonVariableTibble$id %in% filteredESVIds,]$esvTitle),selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
  }
},ignoreNULL = FALSE)

# Observe Var Class Filter - Action: Update metadata nodes displayed on map (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioDom,{
  req(input$esvFilterBioDom)
  
  if("Please select a Domain" %in% input$esvFilterBioDom){
    # Update table - clear results and do nothing else
    domainExploreReactive(NULL)
  }else{
    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterPhysDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterTraitDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterBioDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    domainExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
},ignoreNULL = FALSE)

# PHYSICAL Classes
observeEvent(input$esvFilterPhysDom,{
  req(input$esvFilterPhysDom)
  
  if("Please select a Domain" %in% input$esvFilterPhysDom){
    # Update table - clear results and do nothing else
    domainExploreReactive(NULL)
  }else{
    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterTraitDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterBioDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterPhysDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    domainExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
},ignoreNULL = FALSE)

# SALMON TRAIT Classes
observeEvent(input$esvFilterTraitDom,{
  req(input$esvFilterTraitDom)
  
  if("Please select a Domain" %in% input$esvFilterTraitDom){
    # Update table - clear results and do nothing else
    domainExploreReactive(NULL)
  }else{
    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterPhysDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session,'esvFilterBioDom',selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    # leaflet update here - show filter
    filteredESVId <- LSFEssentialSalmonVariableTibble[LSFEssentialSalmonVariableTibble$esvTitle %in% input$esvFilterTraitDom,]$id
    
    filteredMetadataIds <- metadataESVDomainRelationships[(metadataESVDomainRelationships$endNode %in% filteredESVId & metadataESVDomainRelationships$domain %in% input$domainFilter),]$startNode
    
    domainExploreReactive(LSFMetadataTibble[LSFMetadataTibble$id %in% filteredMetadataIds,])
  }
  
},ignoreNULL = FALSE)

output$domainExploreTable <- DT::renderDT({
    if(!is.null(domainExploreReactive())){
      sf::st_set_geometry(domainExploreReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')],NULL)
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