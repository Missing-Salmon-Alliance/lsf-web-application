# One observer for map_shape_click, search based on layer id
observeEvent(input$map_shape_click,{
  # first detect shape layer name and ignore if not one of the searchable layers
  if(input$map_shape_click[3] %in% c("Ecoregions","NAFO Divisions")){
    # if TRUE, next step clear all Data Source markers
    leaflet::leafletProxy("map", session) %>%
      leaflet::clearGroup(group = 'Data Source')
    # get name of area clicked using id and pass to filter metadata
    if(input$map_shape_click[3] == "Ecoregions"){
      layerIDname <- ICES_Ecoregions[ICES_Ecoregions$id == input$map_shape_click[1],]$name
      metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion,layerIDname),])
    }else{
      layerIDname <- nafoDivisionsSF[nafoDivisionsSF$id == input$map_shape_click[1],]$name
      metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectNAFODivision,layerIDname),])
    }
    # redraw new filtered data
    redrawFilteredMarkers(metadataFilterReactive(),session)
  }
  
})

# Eco regions based on pre-calculated intersects
observeEvent(input$ecoregionFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$ecoregionFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectICESEcoRegion,input$ecoregionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})


# NAFO divisions based on pre-calculated intersects
observeEvent(input$nafodivisionFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$nafodivisionFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectNAFODivision,input$nafodivisionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

# migration routes based on pre-calculated intersects
observeEvent(input$migrationRouteFilter,{
  leaflet::leafletProxy("map", session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$migrationRouteFilter == "All"){
    metadataFilterReactive(LSFMetadataTibble)
  }else{
    # apply regex escape
    filterValue <- str_replace_all(str_replace_all(input$migrationRouteFilter,"\\(","\\\\("),"\\)","\\\\)")
    metadataFilterReactive(LSFMetadataTibble[str_detect(LSFMetadataTibble$metadataCoverageIntersectMigrationRoutes,filterValue),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})