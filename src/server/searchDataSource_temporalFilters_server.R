observeEvent(input$temporalSlider,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
    metadataFilterReactive(LSFMetadataTibble[LSFMetadataTibble$metadataCoverageStartYear >= input$temporalSlider[1] & LSFMetadataTibble$metadataCoverageEndYear <= input$temporalSlider[2],])
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

observeEvent(input$monthsSelect,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'metadataMarkers')
  if(input$monthsSelect == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageMonthsOfYear,input$monthsSelect),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})
