#' Map NWIS Gaeges with metadata popup
#'
#' @param stationID a vector of stationID(s)
#' @param filter use filtered data?
#' @return a leaflet map
#' @export
#' @author Mike Johnson

map = function(stationID, filter = TRUE){

if(filter){ meta = lowflows::usgs_filter } else { meta = lowflows::usgs_meta }
points = sp::SpatialPointsDataFrame(coords = cbind(meta$lon, meta$lat), data = meta, proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

p = points[points$siteID %in% stationID, ]

pop <- paste(
  paste("<strong>Site ID:</strong>", p$siteID),
  paste("<strong>COMID:</strong>", p$COMID),
  paste("<strong>Site Name:</strong>", p$name),
  paste("<strong>HUC8:</strong>", p$huc8),
  paste("<strong>Number of Records:</strong>", p$n.days),
  paste("<strong>Start:</strong>", p$startDate),
  paste("<strong>End:</strong>", p$endDate),
  paste("<strong>State:</strong>", p$state),
  paste("<strong>County:</strong>", p$county),
  sep = "<br/>"
)


leaflet() %>%
  addProviderTiles(leaflet::providers$CartoDB.Positron,     group = "Base") %>%
  addProviderTiles(leaflet::providers$Esri.WorldImagery,    group = "Imagery") %>%
  addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap , group = "Terrain") %>%
  
  addScaleBar("bottomleft") %>%
  addMiniMap(tiles = leaflet::providers$OpenStreetMap.BlackAndWhite,
             toggleDisplay = TRUE,
             minimized = TRUE) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "feet",
    primaryAreaUnit = "sqmiles",
    activeColor = "red",
    completedColor = "green"
  ) %>% 
  leaflet::addMarkers(data = p, popup = pop) %>% 
  addLayersControl(
    baseGroups = c("Base", "Imagery", "Terrain"),
    options = layersControlOptions(collapsed = T)
  )


}
