############################################
# Pull data from SQL database using function

###########################
# OLD METHOD - Bit too complicated

# icesEcoregions <- loadgeoJSONData("ices_ecoregions","ecoregion")
# icesEcoregionsGeoJSON <- convertToGeojsonFeatureCollection(icesEcoregions,"ecoregion")
# ICES_Ecoregions <- sf::read_sf(icesEcoregionsGeoJSON)
# rm(icesEcoregions,icesEcoregionsGeoJSON)

# neContinents <- loadgeoJSONData("ne_continents","continent")
# neContinentsGeoJSON <- convertToGeojsonFeatureCollection(neContinents,"continent")
# neContinentsSF <- sf::read_sf(neContinentsGeoJSON)
# rm(neContinents,neContinentsGeoJSON)

# nascoRivers <- loadgeoJSONData("nasco_rivers_db","rivername")
# nascoRiversGeoJSON <- convertToGeojsonFeatureCollection(nascoRivers,"rivername")
# nascoRiversSF <- sf::read_sf(nascoRiversGeoJSON)
# rm(nascoRivers,nascoRiversGeoJSON)

# nafoDivisions <-loadgeoJSONData("nafo_divisions","zone")
# nafoDivisionsGeoJSON <- convertToGeojsonFeatureCollection(nafoDivisions,"zone")
# nafoDivisionsSF <- sf::read_sf(nafoDivisionsGeoJSON)
# rm(nafoDivisions,nafoDivisionsGeoJSON)

# icesStatEco <- loadgeoJSONData("ices_stat_rect_eco","icesname")
# icesStatEcoGeoJSON <- convertToGeojsonFeatureCollection(icesStatEco,"icesname")
# icesStatEcoSF <- sf::read_sf(icesStatEcoGeoJSON)
# rm(icesStatEco,icesStatEcoGeoJSON)

# feeding <- loadgeoJSONData("feeding_zones","name")
# feedingGeoJSON <- convertToGeojsonFeatureCollection(feeding,"name")
# feedingSF <- sf::read_sf(feedingGeoJSON)
# rm(feeding,feedingGeoJSON)

###########################################
# NEW METHOD - Possible after simplifying SQL tables

ICES_Ecoregions <- loadFullWKBData("ices_ecoregions")
nafoDivisionsSF <- loadFullWKBData("nafo_divisions")
migrationSF <- loadFullWKBData("proposed_migration_routes")
#icesStatEcoSF <- loadFullWKBData("ices_stat_rect_eco")
#feedingSF <- loadFullWKBData("feeding_zones")

# CSV Files - To be moved to SQL

rivers <- read_csv("./src/Index_Rivers_DB.csv")
rivers <- sf::st_as_sf(rivers, coords = c("Longitude_Decimal","Latitude_Decimal"), crs = 4326)
riversSF <- rivers


NASCO_rivers <- read_csv("./src/NASCO_RiversDB.csv",locale = locale(encoding = 'latin1'))
NASCO_rivers <- NASCO_rivers %>% filter(Longitude_Decimal != "") %>%
  filter(Latitude_Decimal != "") %>% filter(RiverName != "")
NASCO_rivers <- sf::st_as_sf(NASCO_rivers, coords = c("Longitude_Decimal","Latitude_Decimal"), crs = 4326)
