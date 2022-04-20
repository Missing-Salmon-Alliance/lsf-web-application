output$domainExploreTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    fluidPage(
      h4("Explore available data resources based on salmon life-stage domains."),
      p("The life-stage domains represent a combination of salmon lifecycle and the environments within which they reside and transit.
        Although marine phases are defined here at a low resolution, geographic information available within the resource can improve context."),
      box(
        width = 5,
        status = 'primary',
        title = "Step 1 - Life-Stage Domain (multi-select available)",
        shinyWidgets::checkboxGroupButtons('domainFilter',"Select a Salmon Life-Stage Domain",choiceNames = lsfDomains()$domainTitle, choiceValues = lsfDomains()$id,
                                           checkIcon = checkboxGroupButtonsIcons)
      ),
      box(
        status = 'primary',
        title = "Step 2 - Variable Classes Relevant to Selected Life-Stage Domains",
        width = 7,
        shinyWidgets::checkboxGroupButtons('esvFilterBioDom',"Biological Processes",choices = c("Please select a Domain"),selected = "Please select a Domain"),
        shinyWidgets::checkboxGroupButtons('esvFilterPhysDom',"Physical Environment",choices = c("Please select a Domain"),selected = "Please select a Domain"),
        shinyWidgets::checkboxGroupButtons('esvFilterTraitDom',"Salmon Traits",choices = c("Please select a Domain"),selected = "Please select a Domain")
      ),
      box(
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        title = "Step 3 - Results",
        DT::DTOutput('domainExploreTable')
      )
    )
  }else{
    fluidPage(
      h1("Life-Stage Domain Explore Area"),
      h3("Please authenticate to access this area")
    )
  }
})

################################
# Domain/Var Class filters
################################

domainExploreReactive <- reactiveVal()
# Observe Domain Filter - Action: Update Variable Class Filters (available class choices filtered by Domain)
observeEvent(input$domainFilter,{
  
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
    filteredDomainIds <- input$domainFilter
    # load variable classes from graph that have a relationship with selected domains
    filteredVariableClasses <- neo4r::call_neo4j(paste0("MATCH (d)<-[:HAS_DOMAIN]-(esv:EssentialSalmonVariable) WHERE id(d) IN [",formatNumericList(input$domainFilter),"] RETURN esv;"),neo_con,type='graph')
    filteredVariableClasses <- filteredVariableClasses$nodes %>% neo4r::unnest_nodes('all')
    
    # update select input using filtered class Titles
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioDom',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$id,
                                             selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysDom',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$id,
                                             selected = character(0),checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitDom',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$id,
                                             selected = character(0),checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
  }
},ignoreNULL = FALSE)

# Observe Var Class Filter - Action: Update metadata nodes displayed on map (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioDom,{
  if("Please select a Domain" %in% input$esvFilterBioDom || is.null(input$esvFilterBioDom)){
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
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",
                                                 formatNumericList(input$esvFilterBioDom),
                                                 "AND r.domainID IN [",
                                                 formatNumericList(input$domainFilter),
                                                 "] RETURN m;"),
                                          neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
  }
},ignoreNULL = FALSE)

# PHYSICAL Classes
observeEvent(input$esvFilterPhysDom,{
  if("Please select a Domain" %in% input$esvFilterPhysDom || is.null(input$esvFilterPhysDom)){
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
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",formatNumericList(input$esvFilterPhysDom),
                                                 "AND r.domainID IN [",
                                                 formatNumericList(input$domainFilter),
                                                 "] RETURN m;"),neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
  }
},ignoreNULL = FALSE)

# SALMON TRAIT Classes
observeEvent(input$esvFilterTraitDom,{
  if("Please select a Domain" %in% input$esvFilterTraitDom || is.null(input$esvFilterTraitDom)){
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
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",formatNumericList(input$esvFilterTraitDom),
                                                 "AND r.domainID IN [",
                                                 formatNumericList(input$domainFilter),
                                                 "] RETURN m;"),neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
  }
  
},ignoreNULL = FALSE)

output$domainExploreTable <- DT::renderDT({
    if(!is.null(domainExploreReactive())){
      domainExploreReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')]
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