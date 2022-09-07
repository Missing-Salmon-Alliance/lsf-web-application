output$domainExploreTabUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    fluidPage(
      h4("Explore available data resources based on salmon life-stage domains."),
      p("The life-stage domains represent a combination of salmon lifecycle and the environments within which they reside and transit.
        Although marine phases are defined here at a low resolution, geographic information available within the resource can improve context."),
      # box(
      #   width = 5,
      #   status = 'primary',
      #   title = "Life-Stage Domain",
      #   shinyWidgets::pickerInput(
      #     inputId = "domainFilter",
      #     label = "Select Salmon Life-Stage Domains",
      #     choices = stats::setNames(as.list(lsfDomains()$id),lsfDomains()$domainTitle),
      #     multiple = TRUE,
      #     options = shinyWidgets::pickerOptions(
      #       selectedTextFormat = 'count',
      #       liveSearch = TRUE)
      #   )
      # ),
      # box(
      #   status = 'primary',
      #   title = "Variable Class",
      #   width = 7,
      #   
      #   shinyWidgets::pickerInput('esvFilter',"Select Variable Classes",
      #     choices = list(
      #       'Biological Processes' = 
      #         stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$id),
      #                         lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$esvTitle),
      #       'Physcial Environment' = 
      #         stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$id),
      #                         lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$esvTitle),
      #       'Salmon Trait' = 
      #         stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$id),
      #                         lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$esvTitle)
      #   ),
      #     multiple = TRUE,
      #     options = shinyWidgets::pickerOptions(
      #       selectedTextFormat = 'count',
      #       liveSearch = TRUE)
      #   )
      # ),
      # box(
      #   status = 'success',
      #   width = 12,
      #   solidHeader = TRUE,
      #   title = "Results",
      verbatimTextOutput('domainFilterActive'),
      verbatimTextOutput('vclassFilterActive'),
      verbatimTextOutput('stockunitFilterActive'),
        downloadButton('downloadSearchResults',"Download Search Results", class = 'btn-primary btn-xs'),
        DT::DTOutput('domainExploreTable')
      # )
    )
  }else{
    fluidPage(
      h1("Life-Stage Domain Explore Area"),
      h3("Please authenticate to access this area")
    )
  }
})

################################
# Domain/Var Class filters (sidebar UI)
################################

output$domainExploreFiltersUI <- renderUI({
  req(user_info()) # only action if user_info has been created
  if (user_info()$result) { # if user logon is true:
    tagList(
      shinyWidgets::pickerInput(
        inputId = 'domainFilter',
        label = "Salmon Life-Stage Domain",
        choices = stats::setNames(as.list(lsfDomains()$id),lsfDomains()$domainTitle),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          selectedTextFormat = 'count',
          liveSearch = TRUE)),
      shinyWidgets::pickerInput(
        inputId = 'esvFilter',
        label = "Variable Class",
        choices = list(
          'Biological Processes' =
            stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$id),
                            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Biological",]$esvTitle),
          'Physcial Environment' =
            stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$id),
                            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Physical",]$esvTitle),
          'Salmon Trait' =
            stats::setNames(as.list(lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$id),
                            lsfVariableClasses()[lsfVariableClasses()$esvCategory == "Salmon Trait",]$esvTitle)
        ),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          selectedTextFormat = 'count',
          liveSearch = TRUE)),
      shinyWidgets::pickerInput(
        inputId = 'stockunitFilter',
        label = "Stock Unit",
        choices = c("LB","NFLD","QB","GF","SF","US","IC.SW","SC.W","SC.E",
                    "IR.N","IR","EW","FR","GY","SP","RU","FI","NO","SWD","IC.NE","DK"),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          selectedTextFormat = 'count',
          liveSearch = TRUE)),
    )
  }
})

# UI showing active filters

output$domainFilterActive <- renderText({
  lsfDomains()[lsfDomains()$id %in% domainSearchSpace(),]$domainTitle
  })
output$vclassFilterActive <- renderText({
  lsfVariableClasses()[lsfVariableClasses()$id %in% esvSearchSpace(),]$esvTitle
  })
output$stockunitFilterActive <- renderText({
  stockunitSearchSpace()
})

domainExploreReactive <- reactiveVal()
domainSearchSpace <- reactiveVal()
esvSearchSpace <- reactiveVal()
stockunitSearchSpace <- reactiveVal()

# Observe Filters - Action: Update search space and query database
observeEvent(c(input$domainFilter,input$esvFilter,input$stockunitFilter),{

  # create domainFilter search space
  if(is.null(input$domainFilter)){
    domainSearchSpace(lsfDomains()$id) # selecting zero domains has the effect of adding all domains to the search space (i.e., no domain filter applied)
  }else{
    domainSearchSpace(input$domainFilter)
  }
  # create esvFilter search space
  if(is.null(input$esvFilter)){
    esvSearchSpace(lsfVariableClasses()$id) # selecting zero variable classes has the effect of adding all variable classes to the search space (i.e., no variable class filter applied)
  }else{
    esvSearchSpace(input$esvFilter)
  }
  
  # create stockunitFilter search space
  if(is.null(input$stockunitFilter)){
    stockunitSearchSpace(c("LB","NFLD","QB","GF","SF","US","IC.SW","SC.W","SC.E",
                           "IR.N","IR","EW","FR","GY","SP","RU","FI","NO","SWD","IC.NE","DK")) # selecting stock units has the effect of adding all stock units to the search space (i.e., no stock unit filter applied)
  }else{
    stockunitSearchSpace(input$stockunitFilter)
  }
  
  
  # # load metadata with filters applied
  filteredMetadata <- neo4r::call_neo4j(
    paste0(
      "MATCH (m)-[r:HAS_ESV]-(esv) WHERE id(esv) IN [",
      formatNumericList(esvSearchSpace()),
      "] AND r.domainID IN [",
      formatNumericList(domainSearchSpace()),
      "] RETURN m;"),
    neo_con,type = 'graph')
  
  # deal with empty results
  if(paste0(class(filteredMetadata),collapse = ",") == 'neo,list'){ # test that returned item is a valid graph object, otherwise ignore empty result
    filteredMetadata <- filteredMetadata$nodes %>% neo4r::unnest_nodes('all') # if valid graph, unnest nodes
    # apply stockunit search space
    filteredMetadata$x <- list(stockunitSearchSpace)
    filteredMetadata$y <- stringr::str_split(filteredMetadata$metadataStockUnit,",")
    filteredMetadata <- filteredMetadata %>% rowwise() %>% mutate(z = paste0(intersect(x,y),collapse = ","))
    
    filteredMetadata <- filteredMetadata[filteredMetadata$z != "",]
    
    if(nrow(filteredMetadata) > 0){
      domainExploreReactive(filteredMetadata)
    }else{
      domainExploreReactive(NULL)
    }
  }else{
    domainExploreReactive(NULL)
  }
  
},ignoreNULL = FALSE)

# load search results into table
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

# download search results - Action: prompt user to save dropped rows
output$downloadSearchResults <- downloadHandler(
  filename = function() {
    paste('SalHub_Search_Results_', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    results <- domainExploreReactive()[,c('metadataUUID','metadataTitle','metadataAbstract','metadataOrganisation','metadataAltURI','metadataAccessProtocol','metadataGeographicDescription','metadataCoverageNorth','metadataCoverageEast','metadataCoverageSouth','metadataCoverageWest')]
    names(results) <- c('UUID','Title','Abstract','Organisation','URL','AccessProtocol','GeographicDescription','lat1','lon1','lat2','lon2')
    readr::write_csv(results,file)
  }
)