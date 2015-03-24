

test2 <- read.delim("data/test2.tsv")

str(test2)

names(test2)


# "LAT"                                                                                                               
# "LONG"

summary(test2$LAT)                                                                                                               
summary(test2$LONG)

str(test2$LAT)                                                                                                               
str(test2$LONG)

test2$LAT <- as.numeric(test2$LAT)                                                                                                               
test2$LONG <- as.numeric(test2$LONG)

datasp <-test2[ , c("LONG", "LAT")]

#plot(datasp)

View(test2$LONG)

### Eliminate record without coordinates
test2 <-test2[!rowSums(is.na(test2["LAT"])), ]
test2 <-test2[!rowSums(is.na(test2["LONG"])), ]


require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)


coords <- cbind(test2$LONG, test2$LAT)
datasp0 <- SpatialPointsDataFrame(coords, data= test2, proj4string=CRS("+proj=longlat"))



### Map Overlay in gplot2

require("ggmap")


googleterrain <- get_map(location = c(lon =37.22, lat = 31.32),
                         color = "color", source = "google",maptype = "terrain", zoom = 8)
googleeterrain <- ggmap(googleterrain)

rm(mapcamp)
mapcamp <- googleeterrain
mapcamp <- mapcamp + 
  geom_point(aes(x = LONG, y = LAT), data=test2, color="orangered3")+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Home Visit")+
  theme_bw()
ggsave("map.png", mapcamp, width=8, height=6,units="in", dpi=300)

