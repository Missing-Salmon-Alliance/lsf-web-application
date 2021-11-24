observeEvent(input$temporalSlider,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
    # double slider control - filter resources based on start and end year
    # filters resources with start date LESS THAN OR EQUAL TO first selection AND end date GREATER THAN OR EQUAL TO second selection
    metadataFilterReactive(lsfMetadata()[lsfMetadata()$metadataCoverageStartYear <= input$temporalSlider[1] & lsfMetadata()$metadataCoverageEndYear >= input$temporalSlider[2],])
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

observeEvent(input$monthsSelect,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(is.null(input$monthsSelect)){
    metadataFilterReactive(lsfMetadata())
  }else{
    targets <- paste0("[(",paste0(input$monthsSelect,collapse = ")|("),")]")
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageMonthsOfYear,targets),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
},ignoreNULL = FALSE)
