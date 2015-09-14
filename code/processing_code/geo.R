####### Recover correct Governorate & district using the coordinates
require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)

library(rgeos)
library(sp) 
library(maptools)
library(rgdal)
library(sp)

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
  z <- over(y, x)
  
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

### Spatial join on district 
## getting correct district and gov from coordinates
#districtgeo <- readOGR("geo/district.geojson", "OGRGeoJSON")
districtgeo1 <- readOGR("geo/subdistrict.geojson", "OGRGeoJSON")
districtgeo <- spTransform(districtgeo1, CRS("+proj=longlat +ellps=WGS84"))
#plot(districtgeo)
#names(districtgeo)

## Create a spatial data frame with vaf records
rm(datasp)
datasp <-homevisit[ , c("long", "lat")]
#3ploting for a quick check
##plot(datasp)
coords <- cbind(datasp$long, datasp$lat)
#datasp0 <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))
datasp0 <- SpatialPoints(coords, proj4string=CRS("+proj=longlat"))

#plot(datasp0)

## Intersection of points with district 
datasp1 <- IntersectPtWithPoly(datasp0, districtgeo)
datasp1@data$id <- rownames(datasp1@data)
rm(correct)
#View(districtgeo@data)

#correct <- datasp1@data[ ,c("id","district_c","district","Gov_NAME","Gov_code" )]

correct <- datasp1@data[ ,c( "gid" , "adm1_name" , "adm1_nam",    "adm1_code",  "adm1_pro", 
                             "adm2_name", "adm2_nam", "adm2_code", "adm2_pro", 
                             "adm3_name", "adm3_nam", "adm3_code", "adm3_pro",
                             "Pov_2008",   "Pov_2006" ,  "Pov_2002" ,  "Pov_2010"  )]

homevisit$id <- rownames(homevisit)
correct$id <- rownames(correct)

homevisit.geo <- homevisit
summary(homevisit.geo$dataset)

#homevisit <- merge(x=homevisit, y=correct, by="id")
homevisit <- merge(x=homevisit.geo, y=correct, by="id")
#rm(correct)
summary(homevisit$dataset)
## Let's create distinc variables for each records on gov
