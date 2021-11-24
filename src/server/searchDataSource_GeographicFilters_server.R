activeGeographicFilterReactive <- reactiveVal("No Filter Selected")

# One observer for searchTabMap_shape_click, search based on layer id
observeEvent(input$searchTabMap_shape_click,{
  # first detect shape layer name and ignore if not one of the searchable layers
  if(input$searchTabMap_shape_click[3] %in% c("ICES Ecoregions","NAFO Divisions")){
    # if TRUE, next step clear all Data Source markers
    leaflet::leafletProxy('searchTabMap', session) %>%
      leaflet::clearGroup(group = 'Data Source')
    # get name of area clicked using id and pass to filter metadata
    if(input$searchTabMap_shape_click[3] == "ICES Ecoregions"){
      layerID <- stringr::str_split(input$searchTabMap_shape_click[1],"_",simplify = T)[,2]
      layerIDname <- ICES_Ecoregions[ICES_Ecoregions$objectid == layerID,]$ecoregion
      metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectICESEcoRegion,layerIDname),])
      activeGeographicFilterReactive(paste0("ICES Ecoregions - ",layerIDname))
    }else{
      layerID <- stringr::str_split(input$searchTabMap_shape_click[1],"_",simplify = T)[,2]
      layerIDname <- nafoDivisionsSF[nafoDivisionsSF$ogc_fid == layerID,]$zone
      metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectNAFODivision,layerIDname),])
      activeGeographicFilterReactive(paste0("NAFO Divisions - ",layerIDname))
    }
    # redraw new filtered data
    redrawFilteredMarkers(metadataFilterReactive(),session)
  }
  
})


output$activeGeographicFilter <- renderText(activeGeographicFilterReactive())


# Eco regions based on pre-calculated intersects
observeEvent(input$ecoregionFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$ecoregionFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectICESEcoRegion,input$ecoregionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})


# NAFO divisions based on pre-calculated intersects
observeEvent(input$nafodivisionFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$nafodivisionFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectNAFODivision,input$nafodivisionFilter),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})

# migration routes based on pre-calculated intersects
observeEvent(input$migrationRouteFilter,{
  leaflet::leafletProxy('searchTabMap', session) %>%
    leaflet::clearGroup(group = 'Data Source')
  if(input$migrationRouteFilter == "All"){
    metadataFilterReactive(lsfMetadata())
  }else{
    # apply regex escape
    filterValue <- str_replace_all(str_replace_all(input$migrationRouteFilter,"\\(","\\\\("),"\\)","\\\\)")
    metadataFilterReactive(lsfMetadata()[str_detect(lsfMetadata()$metadataCoverageIntersectMigrationRoutes,filterValue),])
  }
  redrawFilteredMarkers(metadataFilterReactive(),session)
})