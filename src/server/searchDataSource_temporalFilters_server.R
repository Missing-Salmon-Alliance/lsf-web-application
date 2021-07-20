observeEvent(input$temporalSlider,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
    # double slider control - filter resources based on start and end year
    # filters resources with start date LESS THAN OR EQUAL TO first selection AND end date GREATER THAN OR EQUAL TO second selection
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$metadataCoverageStartYear <= input$temporalSlider[1] & LSFMetadataTibble$metadataCoverageEndYear >= input$temporalSlider[2],])
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

observeEvent(input$monthsSelect,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$monthsSelect == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageMonthsOfYear,input$monthsSelect),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})
