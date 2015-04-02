
##########################################################################
#### Import data & Rename variable
source("code/2_Welfare-model.R")



require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)

require("hexbin")


###############################################################
## Create a spatial data frame with hv records
rm(datasp)
datasp1 <-homevisit3[ , c("long", "lat")]
# summary(datasp)
coords <- cbind(datasp1$long, datasp1$lat)
datasp <- SpatialPointsDataFrame(coords, data= datasp1, proj4string=CRS("+proj=longlat"))
plot(datasp)

###############################################################
### Getting a google map background
##############################################################

require("ggmap")
googleterrain <- get_map(location = c(lon =37.22, lat = 31.32),
                         color = "color", source = "google",maptype = "terrain", zoom = 7)
googleeterrain <- ggmap(googleterrain)




###############################################################
### Simple Map Overlay in gplot2
##############################################################


rm(map1)
map1 <- googleeterrain
map1 <- map1 + 
  geom_point(aes(x = long, y = lat), data=datasp1, color="orangered3")+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Home Visit")+
  theme_bw()
ggsave("out/map1.png", map1, width=8, height=6,units="in", dpi=300)
rm(map1)

###############################################################
### Hebin Map Overlay in gplot2 -- Plotting Family Size
##############################################################


rm(map.familysize.all)
map.familysize.all <- googleeterrain
map.familysize.all <- map.familysize.all +
  stat_summary_hex(aes(x= hve$long,  y= hve$lat, z = hve$Household.information.Family.Size ), alpha = 7/10)+
  #coord_equal() +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size") +
  ggtitle("Home Visit Analysis ")

# Save this!
ggsave("out/map-familysize-all.png", map.familysize.all, width=8, height=6,units="in", dpi=300)

rm(map.familysize.all)



###############################################################
### Hebin Map Overlay in gplot2 -- Plotting Severe Welfare score from v3
##############################################################

rm(hve.severe.v3)
hve.severe.v3 <- hve[ which(hve$predictedwellfare.vw5.v3.class == "Severe"), ]
View(hve.severe.v3)
rm(map.severe.v3)
map.severe.v3 <- googleeterrain
map.severe.v3 <- map.severe.v3 +
  stat_summary_hex(aes(x= hve.severe.v3$long,  y= hve.severe.v3$lat, z = hve.severe.v3$Household.information.Family.Size ),
                   alpha = 7/10)+
  #coord_equal() +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size for Severe welfare Cat") +
  ggtitle("Home Visit Analysis ")

# Save this!
ggsave("out/map-severev3.png", map.severe.v3, width=8, height=6,units="in", dpi=300)

rm(map.severe.v3)
rm(hve.severe.v3)

###############################################################
### Hebin Map Overlay in gplot2 -- Plotting Severe Welfare score from v4
##############################################################
hve.severe.v4 <-hve[ with(hve$predictedwellfare.vw5.v4.class == "Severe"), ]

rm(map.severe.v4)
map.severe.v4 <- googleeterrain
map.severe.v4 <- map.severe.v4 +
  stat_summary_hex(aes(x= hve.severe.v4$long,  y= hve.severe.v4$lat, z = hve.severe.v4$Household.information.Family.Size ),
                   alpha = 7/10)+
  #coord_equal() +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size for Severe welfare Cat") +
  ggtitle("Home Visit Analysis ")

# Save this!
ggsave("out/map-severev4.png", map.severe.v4, width=8, height=6,units="in", dpi=300)

rm(map.severe.v4)
rm(hve.severe.v4)

###############################################################
### Graph example with gplot2
##############################################################

# Reorgnise the matrix in order to compute pivot tables
# Tuto: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/

require("reshape2")
####### Let's graph Assets

# extract on asset per family size
homevisit3.assets <- melt(homevisit3, id=c("Household.information.Family.Size"),
                          measure=c("Type.of.Housing.Assets...Floor.mattress",
                                    "Type.of.Housing.Assets...Sofa.set",
                                    "Type.of.Housing.Assets...Kitchen.utilities",
                                    "Type.of.Housing.Assets...Computer",
                                    "Type.of.Housing.Assets...Blankets",
                                    "Type.of.Housing.Assets...Stove",
                                    "Type.of.Housing.Assets...Washing.machine",
                                    "Type.of.Housing.Assets...Table-chairs",
                                    "Type.of.Housing.Assets...Cabinets",
                                    "Type.of.Housing.Assets...Fridge",
                                    "Type.of.Housing.Assets...Television",
                                    "Type.of.Housing.Assets...Water.heater",
                                    "Type.of.Housing.Assets...Freezer",
                                    "Type.of.Housing.Assets...Other..specify"))


## Reorder factor level according to a value


homevisit3.assets <- homevisit3.assets[order(-homevisit3.assets$value), ]

#homevisit3.assets$variable <- as.character(homevisit3.assets$variable)

#str(homevisit3.assets)

#homevisit3.assets$variable <- factor(homevisit3.assets$variable , levels = homevisit3.assets[order(homevisit3.assets$value), 1])

levels(homevisit3.assets$variable)

#
homevisit3.assets2 <- dcast(homevisit3.assets, Household.information.Family.Size ~ variable)



# Bar graph based on reordered variable per value
asset.plot <- ggplot(data=homevisit3.assets, aes(x=reorder(variable, value) , y=value, fill=variable)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  coord_flip()+
  xlab("Asset") + 
  ylab("Household") +
  ggtitle("Household assets")

# Save this!
ggsave("out/assetplot.png", asset.plot, width=8, height=6,units="in", dpi=300)

assetplotfamily <- ggplot(data=homevisit3.assets, aes(x=Household.information.Family.Size , y=value, fill=variable))+
  geom_bar(stat="identity")+
  labs(x = "", y = "")

# Save this!
ggsave("out/assetplotfamily.png", assetplotfamily, width=8, height=2,units="in", dpi=300, bg = "transparent")
