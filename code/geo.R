####### Recover correct Governorate & district using the coordinates
require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)

IntersectPtWithPoly <- function(x, y) {
  # Extracts values from a SpatialPolygonDataFrame with
  # SpatialPointsDataFrame, and appends table (similar to
  # ArcGIS intersect)
  # Args:
  #   x: SpatialPoints*Frame
  #   y: SpatialPolygonsDataFrame
  # Returns:
  # SpatialPointsDataFrame with appended table of polygon attributes
  
  # Set up overlay with new column of join IDs in x
  z <- overlay(y, x)
  
  # Bind captured data to points dataframe
  x2 <- cbind(x, z)
  
  # Make it back into a SpatialPointsDataFrame
  # Account for different coordinate variable names
  if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) {
    coordinates(x2) <- ~coords.x1 + coords.x2
  } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) {
    coordinates(x2) <- ~x + y
  }
  
  # Reassign its projection if it has one
  if(is.na(CRSargs(x@proj4string)) == "FALSE") {
    x2@proj4string <- x@proj4string
  }
  return(x2)
}