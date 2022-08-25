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
        title = "Step 1 - Life-Stage Domain",
        shinyWidgets::pickerInput(
          inputId = "domainFilter",
          label = "Select Salmon Life-Stage Domains",
          choices = stats::setNames(as.list(lsfDomains()$id),lsfDomains()$domainTitle),
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            selectedTextFormat = 'count',
            liveSearch = TRUE)
        )
      ),
      box(
        status = 'primary',
        title = "Step 2 - Variable Classes Relevant to Selected Life-Stage Domains",
        width = 7,
        
        shinyWidgets::pickerInput('esvFilter',"Select Variable Classes",choices = c("Please select a Domain"),selected = "Please select a Domain",
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            selectedTextFormat = 'count',
            liveSearch = TRUE)
        )
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
    shinyWidgets::updatePickerInput(session, 'esvFilter', choices = c("Please select a Domain"),selected = "Please select a Domain")

    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) RETURN m;"),neo_con,type = 'graph')
    
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
    
  }else{
    # capture filtered domain ID's as vector
    filteredDomainIds <- input$domainFilter
    # load variable classes from graph that have a relationship with selected domains
    filteredVariableClasses <- neo4r::call_neo4j(paste0("MATCH (d)<-[:HAS_DOMAIN]-(esv:EssentialSalmonVariable) WHERE id(d) IN [",formatNumericList(input$domainFilter),"] RETURN esv;"),neo_con,type='graph')
    filteredVariableClasses <- filteredVariableClasses$nodes %>% neo4r::unnest_nodes('all') %>% dplyr::group_by('esvCategory')
    
    # update select input using filtered class Titles
    shinyWidgets::updatePickerInput(session, 'esvFilter',
      choices = list('Biological Processes' = 
      stats::setNames(as.list(filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$id),
        filteredVariableClasses[filteredVariableClasses$esvCategory == "Biological",]$esvTitle),
        'Physcial Environment' = 
      stats::setNames(as.list(filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$id),
        filteredVariableClasses[filteredVariableClasses$esvCategory == "Physical",]$esvTitle),
        'Salmon Trait' = 
      stats::setNames(as.list(filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$id),
        filteredVariableClasses[filteredVariableClasses$esvCategory == "Salmon Trait",]$esvTitle)
      ),
      selected = character(0))

  }
},ignoreNULL = FALSE)

# Observe Var Class Filter - Action: Update metadata nodes displayed on map (filter by relationship to class selected)
observeEvent(input$esvFilter,{
  if("Please select a Domain" %in% input$esvFilter || is.null(input$esvFilter)){
    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) WHERE r.domainID IN [",
      formatNumericList(input$domainFilter),
      "] RETURN m;"),
      neo_con,type = 'graph')
    if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
      filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all')
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
  }else{
    # Update table - clear results and refill with new selection
    domainExploreReactive(NULL)
    # filter
    filteredMetadata <- neo4r::call_neo4j(paste0("MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",
                                                 formatNumericList(input$esvFilter),
                                                 "] AND r.domainID IN [",
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