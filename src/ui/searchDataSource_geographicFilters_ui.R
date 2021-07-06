shinyBS::bsCollapsePanel(
  title = "Geographic Filters",
  style = 'primary',
  column(
    width = 6,
    selectInput('ecoregionFilter',"Choose an ICES Eco Region",choices = c("All",uniqueICESEcoRegions),multiple = FALSE, selected = "All", width = '100%'),
    selectInput('nafodivisionFilter',"Choose a NAFO Division",choices = c("All",uniqueNAFODivisions),multiple = FALSE, selected = "All", width = '100%'),
    selectInput('migrationRouteFilter',"Choose a Migration Route (Month)",choices = c("All",uniqueMigrationRoutes),multiple = FALSE, selected = "All", width = '100%')
  ),
  column(
    width = 6,
    selectInput('icesStockUnitFilter',"Choose an ICES Stock Unit",choices = c("All",unique(riversSF[["ICES Stock Unit"]])),multiple = FALSE, selected = "All", width = '100%'),
    selectInput('icesIndexRiverFilter',"Choose an ICES Index River",choices = c("All",riversSF$RiverName),multiple = FALSE, selected = "All", width = '100%')
    
  )
  
)