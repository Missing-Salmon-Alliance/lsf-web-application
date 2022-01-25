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
                         # set the choices as a name/value list with name being the hypothesis title and value being the node id
                         choices = c("",setNames(as.character(lsfHypotheses()$id), lsfHypotheses()$hypothesisTitle)),
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
    filteredSubHypotheses <- neo4r::call_neo4j(paste0("MATCH (h:Hypothesis)-[:HAS_SUBHYPOTHESIS]->(sh:SubHypothesis) WHERE id(h) = ",input$hypothesisFilter," RETURN sh;"),neo_con,type='graph')
    filteredSubHypotheses <- filteredSubHypotheses$nodes %>% neo4r::unnest_nodes('all')
    # update select input using filtered class Titles
    updateSelectizeInput(session, 'subHypothesisFilter', choices = c("",setNames(as.character(filteredSubHypotheses$id),filteredSubHypotheses$subHypothesisTitle)),selected = "")
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
    filteredVariableClasses <- neo4r::call_neo4j(paste0("MATCH (sh:SubHypothesis)-[:REQUIRES_ESV]->(esv:EssentialSalmonVariable) WHERE id(sh) = ",input$subHypothesisFilter," RETURN esv;"),neo_con,type='graph')
    filteredVariableClasses <- filteredVariableClasses$nodes %>% neo4r::unnest_nodes('all')
    
    # update select input using filtered class Titles
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterBioHyp',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$id,
                                             selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterPhysHyp',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$id,
                                             selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
    shinyWidgets::updateCheckboxGroupButtons(session, 'esvFilterTraitHyp',
                                             choiceNames = filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$esvTitle,
                                             choiceValues = filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$id,
                                             selected = character(0),
                                             checkIcon = checkboxGroupButtonsIcons,
                                             size = 'xs')
  }
},ignoreNULL = FALSE)

# Observe class Filter - Action: Update metadata displayed in hypothesisExplore table (filter by relationship to class selected)
# BIOLOGICAL classes
observeEvent(input$esvFilterBioHyp,{
  #req(input$esvFilterBioHyp)
  # if SubHypotheses selection is cleared out, revert to null
  if("Please select a Sub-Hypothesis" %in% input$esvFilterBioHyp || is.null(input$esvFilterBioHyp)){
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
    # capture target environment from selected subHyp node
    filteredSubHypEnv <- neo4r::call_neo4j(paste0("MATCH (n) WHERE id(n) = ",input$subHypothesisFilter," RETURN n.subHypothesisEnvironment as env;"),neo_con,type='row')$env$value
    # capture subset of domains that apply to this environment
    filteredDomainTitles <- lsfDomains()[lsfDomains()$domainEnvironment == filteredSubHypEnv,]$domainTitle
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[a:HAS_ESV]-(esv) WHERE id(esv) IN [",formatNumericList(input$esvFilterBioHyp),"] AND a.domain IN [",formatCheckboxGroupCategories(filteredDomainTitles),"] RETURN m;"),neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      hypothesisExploreReactive(filteredMetadata)
    }else{
      hypothesisExploreReactive(NULL)
    }

  }
},ignoreNULL = FALSE)

# PHYSICAL classes
observeEvent(input$esvFilterPhysHyp,{
  if("Please select a Sub-Hypothesis" %in% input$esvFilterPhysHyp || is.null(input$esvFilterPhysHyp)){
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
    # capture target environment from selected subHyp node
    filteredSubHypEnv <- neo4r::call_neo4j(paste0("MATCH (n) WHERE id(n) = ",input$subHypothesisFilter," RETURN n.subHypothesisEnvironment as env;"),neo_con,type='row')$env$value
    # capture subset of domains that apply to this environment
    filteredDomainTitles <- lsfDomains()[lsfDomains()$domainEnvironment == filteredSubHypEnv,]$domainTitle
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[a:HAS_ESV]-(esv) WHERE id(esv) IN [",formatNumericList(input$esvFilterPhysHyp),"] AND a.domain IN [",formatCheckboxGroupCategories(filteredDomainTitles),"] RETURN m;"),neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      hypothesisExploreReactive(filteredMetadata)
    }else{
      hypothesisExploreReactive(NULL)
    }
  }
},ignoreNULL = FALSE)

# SALMON TRAIT classes
observeEvent(input$esvFilterTraitHyp,{
  req(input$esvFilterTraitHyp)
  
  if("Please select a Sub-Hypothesis" %in% input$esvFilterTraitHyp || is.null(input$esvFilterTraitHyp)){
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
    # IN DEVELOPMENT
    # Hypotheses are directly related to Domains in the graph
    # Use this relationship to filter the metadata based on metadata-ESV relationship domain
    #filteredDomainTitles <- neo4r::call_neo4j(paste0("MATCH (h:Hypothesis)-[:HAS_DOMAIN]-(d) WHERE id(h) = ",input$hypothesisFilter," RETURN d.domainTitle as dom;"),neo_con,type = 'row')$dom$value
    
    
    # capture target environment from selected subHyp node
    filteredSubHypEnv <- neo4r::call_neo4j(paste0("MATCH (n) WHERE id(n) = ",input$subHypothesisFilter," RETURN n.subHypothesisEnvironment as env;"),neo_con,type='row')$env$value
    # capture subset of domains that apply to this environment
    filteredDomainTitles <- lsfDomains()[lsfDomains()$domainEnvironment == filteredSubHypEnv,]$domainTitle
    
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[a:HAS_ESV]-(esv) WHERE id(esv) IN [",formatNumericList(input$esvFilterTraitHyp),"] AND a.domain IN [",formatCheckboxGroupCategories(filteredDomainTitles),"] RETURN m;"),neo_con,type = 'graph')
    
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      hypothesisExploreReactive(filteredMetadata)
    }else{
      hypothesisExploreReactive(NULL)
    }
  }
},ignoreNULL = FALSE)

output$hypothesisExploreTable <- DT::renderDT({
    if(!is.null(hypothesisExploreReactive())){
      hypothesisExploreReactive()[,c('metadataTitle','metadataAbstract','metadataKeywords')]
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
