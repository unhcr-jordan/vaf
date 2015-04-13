
##########################################################################
#### Import data & Rename variable
#source("code/2_Welfare-model.R")


### load required packages
require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)
require(hexbin)
require(sp)
require(ggplot2)

### Plotting map with mapbox background https://github.com/milafrerichs/plotmapbox
#library(devtools)
#install_github( "milafrerichs/plotmapbox")
#require(plotmapbox)


###############################################################
### Getting a map background
##############################################################
 ## Jordan bounding box NE 33.374828, 39.301128 ; SW 29.184090, 34.960232
#bounding <- bbox(datsp)
jordanbox <- c(34.8, 29.1, 39.5, 33.5) 


### Bounding can also be done in the ggplot2 object through the following

#  scale_x_continuous(limits = c(34.8, 39.5)) +
#  scale_y_continuous(limits = c(29.1, 33.5)) +


## Center on north 32.2607107,36.5205146,9


require("ggmap")
googleterrain <- get_map(location = c(lon =37.22, lat = 31.32),
                         color = "color", source = "google",maptype = "terrain", zoom = 7)
googleeterrain <- ggmap(googleterrain)

googleroad <- get_map(location = jordanbox,
                         color = "bw", source = "google",maptype = "road", zoom = 7)
googleeroad <- ggmap(googleroad)

northroad <- get_map(location = c(lon =36.12, lat = 32.26),
                         color = "bw", source = "google",maptype = "road", zoom = 9)
northeroad <- ggmap(northroad)

## Black & White cropped background map
stamen <- get_map(location = jordanbox,
                  crop = T, zoom = 8, color = "color", 
                  source = "stamen", maptype = "toner")
stamenet <- ggmap(stamen)


## Does not work
#osm <- get_openstreetmap(location = jordanbox,  source = "osm",  format = "png", filename = "ggmapTemp.png")
#osmback <- ggmap(osm)

#mapbox <- getmapbox_map(center = c(lng = 37.22, lat = 31.32),  mapbox = "unhcr.map-ohec27wu", zoom = 8, size = 640, filename = "map.png")
#mapboxback <- map_png(mapbox)



###############################################################
### Simple Map Overlay in gplot2
##############################################################
rm(map_obs)
map_obs <- googleeroad
map_obs <- map_obs + 
  geom_point(aes(x = long, y = lat), data=hve, color="#006ec7", # UN Blue...
             size=1, alpha = 1/10)+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Home Visit") +
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  theme_bw()
ggsave("out/map_obs.png", map_obs, width=8, height=6,units="in", dpi=300)
rm(map_obs)

###############################################################
### Hebin Map Overlay in gplot2 -- Plotting Family Size
##############################################################
rm(map.familysize.all)
map.familysize.all <- googleeroad
map.familysize.all <- map.familysize.all +
  stat_summary_hex(aes(x= long,  y= lat, z = case.size.vaf), 
                   data=hve ,
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size") +
  ggtitle("Home Visit Analysis ")
ggsave("out/map-familysize-all.png", map.familysize.all, width=8, height=6,units="in", dpi=300)
rm(map.familysize.all)



###############################################################
### Hebin Map Overlay in gplot2 -- Plotting Severe expenditure per capita
##############################################################

rm(hve.exp.severe)
hve.exp.severe <- hve[(hve$Expenditure.Per.Capita.class == "Severe"), ]
rm(map.exp.severe)
map.exp.severe <- googleeroad
map.exp.severe <- map.exp.severe +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.exp.severe, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Severe - # of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure.Per.Capita. Severe ")
ggsave("out/map-exp-1severe.png", map.exp.severe, width=8, height=6,units="in", dpi=300)
rm(map.exp.severe)
rm(hve.exp.severe)

rm(hve.exp.high)
hve.exp.high <- hve[(hve$Expenditure.Per.Capita.class == "High"), ]
rm(map.exp.high)
map.exp.high <- googleeroad
map.exp.high <- map.exp.high +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.exp.high, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "High - # of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure.Per.Capita. High ")
ggsave("out/map-exp-2high.png", map.exp.high, width=8, height=6,units="in", dpi=300)
rm(map.exp.high)
rm(hve.exp.high)

rm(hve.exp.moderate)
hve.exp.moderate <- hve[(hve$Expenditure.Per.Capita.class == "Moderate"), ]
rm(map.exp.moderate)
map.exp.moderate <- googleeroad
map.exp.moderate <- map.exp.moderate +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.exp.moderate, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Moderate - # of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure.Per.Capita Moderate ")
ggsave("out/map-exp-3moderate.png", map.exp.moderate, width=8, height=6,units="in", dpi=300)
rm(map.exp.moderate)
rm(hve.exp.moderate)

rm(hve.exp.low)
hve.exp.low <- hve[(hve$Expenditure.Per.Capita.class == "Low"), ]
rm(map.exp.low)
map.exp.low <- googleeroad
map.exp.low <- map.exp.low +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.exp.low, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Low - # of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure.Per.Capita Low ")
ggsave("out/map-exp-4low.png", map.exp.low, width=8, height=6,units="in", dpi=300)
rm(map.exp.low)
rm(hve.exp.low)



#################
## Zoom on the north 

rm(hve.exp.severe)
hve.exp.severe <- hve[(hve$Expenditure.Per.Capita.class == "Severe"), ]
rm(map.exp.severe.north)
map.exp.severe.north <- northeroad
map.exp.severe.north <- map.exp.severe.north +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.exp.severe, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure.Per.Capita - Severe ")
ggsave("out/map-permanent-shelter-north.png", map.exp.severe.north, width=8, height=6,units="in", dpi=300) 
rm(map.exp.severe.north)
rm(hve.exp.severe)


## Using faceting for expenditure level

rm(map.expenditure.n)
map.expenditure.n <- northeroad
map.expenditure.n <- map.expenditure.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  facet_grid( . ~ Expenditure.Per.Capita.class ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Expenditure level ") #+
#  geom_text(aes(15, .6, label="Severe<28JOD; High=28-68JOD; Moderate=68-100JOD; Low>100jod", color="MVH")) 
  
ggsave("out/map-expenditure-north.png", map.expenditure.n, width=8, height=6,units="in", dpi=300)
rm(map.expenditure.n)

###################################################
### Mapping latrine

rm(map.latrine.n)
map.latrine.n <- northeroad
map.latrine.n <- map.latrine.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  facet_grid( latrine ~ shelter ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Latrine type  ")
ggsave("out/map-latrine-north.png", map.latrine.n, width=8, height=6,units="in", dpi=300)
rm(map.latrine.n)


###################################################
### Mapping type of housing
#Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Permanent.Shelter..structurally.durable.sound.building.with.permanent.materials..cement..
#Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Transitional.Shelter..caravan..mud.hut..tin.or.wood.structure..scrap.material..
#Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Temporary..emergency.shelter..tent..

#View(hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Permanent.Shelter..structurally.durable.sound.building.with.permanent.materials..cement..)

## Using faceting

rm(map.shelter.n)
map.shelter.n <- northeroad
map.shelter.n <- map.shelter.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  facet_grid( . ~ shelter ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Shelter type ")
ggsave("out/map-shelter-north.png", map.shelter.n, width=8, height=6,units="in", dpi=300)
rm(map.shelter.n)



### Mapping shelter according to expenditure level
rm(map.shelter.exp.n)
map.shelter.exp.n <- northeroad
map.shelter.exp.n <- map.shelter.exp.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  facet_grid( Expenditure.Per.Capita.class ~ shelter ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Shelter type per expenditure level ")
ggsave("out/map-shelter-exp-north.png", map.shelter.exp.n, width=8, height=6,units="in", dpi=300)
rm(map.shelter.exp.n)



########### Permanent

rm(hve.permanent.shelter)
hve.permanent.shelter <- hve[(hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Permanent.Shelter..structurally.durable.sound.building.with.permanent.materials..cement.. == 1), ]
rm(map.permanent.shelter)
map.permanent.shelter <- googleeroad
map.permanent.shelter <- map.permanent.shelter +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.permanent.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- permanent.shelter ")
ggsave("out/map-permanent-shelter.png", map.permanent.shelter, width=8, height=6,units="in", dpi=300)
rm(map.permanent.shelter)

rm(map.permanent.shelter.n)
map.permanent.shelter.n <- northeroad
map.permanent.shelter.n <- map.permanent.shelter.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.permanent.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- permanent.shelter ")
ggsave("out/map-permanent-shelter-north.png", map.permanent.shelter.n, width=8, height=6,units="in", dpi=300)
rm(map.permanent.shelter.n)
rm(hve.permanent.shelter)



########### Transitional

rm(hve.transitional.shelter)
hve.transitional.shelter <- hve[(hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Transitional.Shelter..caravan..mud.hut..tin.or.wood.structure..scrap.material.. == 1), ]
rm(map.transitional.shelter)
map.transitional.shelter <- googleeroad
map.transitional.shelter <- map.transitional.shelter +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.transitional.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Low - # of Ind ") +
  ggtitle("Home Visit Analysis- transitional.shelter ")
ggsave("out/map-transitional-shelter.png", map.transitional.shelter, width=8, height=6,units="in", dpi=300)
rm(map.transitional.shelter)

rm(map.transitional.shelter.n)
map.transitional.shelter.n <- northeroad
map.transitional.shelter.n <- map.transitional.shelter.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.transitional.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- transitional.shelter ")
ggsave("out/map-transitional-shelter-north.png", map.transitional.shelter.n, width=8, height=6,units="in", dpi=300)
rm(map.transitional.shelter.n)
rm(hve.transitional.shelter)


########### Temporary

rm(hve.temporary.shelter)
hve.temporary.shelter <- hve[(hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Temporary..emergency.shelter..tent.. == 1), ]
rm(map.temporary.shelter)
map.temporary.shelter <- googleeroad
map.temporary.shelter <- map.temporary.shelter +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.temporary.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Low - # of Ind ") +
  ggtitle("Home Visit Analysis- temporary.shelter ")
ggsave("out/map-temporary-shelter.png", map.temporary.shelter, width=8, height=6,units="in", dpi=300)
rm(map.temporary.shelter)

rm(map.temporary.shelter.n)
map.temporary.shelter.n <- northeroad
map.temporary.shelter.n <- map.temporary.shelter.n +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.temporary.shelter, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- temporary.shelter ")
ggsave("out/map-temporary-shelter-north.png", map.temporary.shelter.n, width=8, height=6,units="in", dpi=300)
rm(map.temporary.shelter.n)
rm(hve.temporary.shelter)
rm(hve.temporary.shelter)



#################################
## Faceting on Predicted Welfare score for v3


rm(map.predictedwellfare.vw5.v3.class)
map.predictedwellfare.vw5.v3.class <- googleeroad
map.predictedwellfare.vw5.v3.class <- map.predictedwellfare.vw5.v3.class +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  facet_grid( . ~ predictedwellfare.vw5.v3.class ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Predicted Expenditure level - based on V3 ")
ggsave("out/map-predictedwellfare_vw5_v3_class.png", map.predictedwellfare.vw5.v3.class, width=8, height=6,units="in", dpi=300)
rm(map.predictedwellfare.vw5.v3.class)

rm(hve.severe.v3)
hve.severe.v3 <- hve[ which(hve$predictedwellfare.vw5.v3.class == "Severe"), ]
rm(map.severe.v3)
map.severe.v3 <- googleeroad
map.severe.v3 <- map.severe.v3 +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.severe.v3, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size for Severe welfare Cat") +
  ggtitle("Home Visit Analysis ")
ggsave("out/map-severev3.png", map.severe.v3, width=8, height=6,units="in", dpi=300)
rm(map.severe.v3)
rm(hve.severe.v3)

#################################
## Trying the same on Predicted Welfare score for v4

rm(map.predictedwellfare.vw5.v4.class)
map.predictedwellfare.vw5.v4.class <- googleeroad
map.predictedwellfare.vw5.v4.class <- map.predictedwellfare.vw5.v4.class +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  facet_grid( . ~ predictedwellfare.vw5.v4.class ) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "# of Ind ") +
  ggtitle("Home Visit Analysis- Predicted Expenditure level - based on v4 ")
ggsave("out/map-predictedwellfare_vw5_v4_class.png", map.predictedwellfare.vw5.v4.class, width=8, height=6,units="in", dpi=300)
rm(map.predictedwellfare.vw5.v4.class)


hve.severe.v4 <-hve[ which(hve$predictedwellfare.vw5.v4.class == "Severe"), ]
rm(map.severe.v4)
map.severe.v4 <- googleeroad
map.severe.v4 <- map.severe.v4 +
  stat_summary_hex(aes(x= long, y= lat, z = case.size.vaf), 
                   data=hve.severe.v4, 
                   fun = sum,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size for Severe welfare Cat") +
  ggtitle("Home Visit Analysis ")
ggsave("out/map-severev4.png", map.severe.v4, width=8, height=6,units="in", dpi=300)
rm(map.severe.v4)
rm(hve.severe.v4)

