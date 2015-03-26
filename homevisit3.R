require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)

## Extracted from https://www.unhcrmenadagdata.org/RaisJordan
## The export has two line of header so we need to skip the first one
homevisit <- read.csv("data/Home_visit_version3.csv", skip=1)

homevisit3 <- homevisit

## Label are not easily legible
## Labels have been manually reviewed
label <- read.csv("data/homevisit_label.csv", header=FALSE)

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit3) <- label[, 3]
# 
str(homevisit3)
rm(label)

## extracting coordinates from dataset
homevisit3$geo <- as.character(homevisit3$Household.information.address..)
options(digits = 15)
homevisit3$lat <- as.numeric(substr(homevisit3$geo , 1,13))
homevisit3$long <- as.numeric(substr( homevisit3$geo, 15,27))


### Eliminate record without coordinates
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["lat"])), ]
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["long"])), ]

# str(homevisit3)

## Create a spatial data frame with vaf records
rm(datasp)
datasp1 <-homevisit3[ , c("long", "lat")]
# summary(datasp)
coords <- cbind(datasp1$long, datasp1$lat)
datasp <- SpatialPointsDataFrame(coords, data= datasp1, proj4string=CRS("+proj=longlat"))

plot(datasp)

### Map Overlay in gplot2

require("ggmap")


googleterrain <- get_map(location = c(lon =37.22, lat = 31.32),
                         color = "color", source = "google",maptype = "terrain", zoom = 7)
googleeterrain <- ggmap(googleterrain)

rm(map1)
map1 <- googleeterrain
map1 <- map1 + 
  geom_point(aes(x = long, y = lat), data=datasp1, color="orangered3")+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Home Visit")+
  theme_bw()
ggsave("out/map1.png", map1, width=8, height=6,units="in", dpi=300)


homevisit4 <-homevisit3[!rowSums(is.na(homevisit3["Household.information.Family.Size"])), ]
require("hexbin")
rm(map2)
map2 <- googleeterrain
map2 <- map2+
  stat_summary_hex(aes(x= homevisit4$long,  y= homevisit4$lat, z = homevisit4$Household.information.Family.Size ), alpha = 7/10)+
  #coord_equal() +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Household.information.Family.Size") +
  ggtitle("Home Visit Analysis ")

# Save this!
ggsave("out/map2.png", map2, width=8, height=6,units="in", dpi=300)
