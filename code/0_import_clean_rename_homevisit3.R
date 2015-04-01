################################################################
#### Import data, Rename variables & Clean rows


################################################################
#### Import data
## Extracted from https://www.unhcrmenadagdata.org/RaisJordan
## The export has two line of header so we need to skip the first one
homevisit.v3 <- read.csv("data/Home_visit_version3.csv", skip=1)

#### Rename variables 
## Label are not easily legible
## Labels have been manually reviewed -- elimination special characters such / or ? or - 
## Labels are also  trimmed when necessary so that they are less than 256 char long - 
## This will allow the rest of the script to run smoothly
label.v3 <- read.csv("data/homevisit3_label.csv", na.strings="", stringsAsFactors=FALSE)


## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit.v3) <- label.v3[, 3]
#rm(label.v3)

# 
#str(homevisit)


##############################################################
## Home visit 4  doing the same with the variable dictionnary

homevisit.v4 <- read.csv("data/Home_visit_4.csv", skip=1)
label.v4 <- read.delim("data/homevisit4_label.tsv", stringsAsFactors=FALSE)
## Remove the lines from the dictionnary that are not in v4
label.v4 <- label.v4[ (label.v4$v4=="yes"), ]
names(homevisit.v4) <- label.v4[, 9]


############################################
#### Appending the 2 dataset

## Removing variables that are not in common so that we can bind dataset
#names(label.v4)
label.onlyv3 <- label.v4[ (label.v4$onlyv3=="yes"),9 ]
homevisit.v3.r <-homevisit.v3[,!(names(homevisit.v3) %in% label.onlyv3)]
homevisit.v3.r$dataset <- "homevisit3"

label.onlyv4 <- label.v4[ (label.v4$onlyv4=="yes"),9 ]
homevisit.v4.r <-homevisit.v4[,!(names(homevisit.v4) %in% label.onlyv4)]
homevisit.v4.r$dataset <- "homevisit4"

# rm(label.v4)
# rm(label.onlyv3)
# rm(label.onlyv4)

homevisit <- rbind(homevisit.v4.r, homevisit.v3.r)

## Remark for analysis in stata variables names shoudl be less 32 characters -- might need to rename a second time

################################################################
####  extracting coordinates from dataset
homevisit$geo <- as.character(homevisit$Household.information.address..)
options(digits = 15)
homevisit$lat <- as.numeric(substr(homevisit$geo , 1,13))
homevisit$long <- as.numeric(substr( homevisit$geo, 15,27))


### Eliminate record without coordinates
homevisit <-homevisit[!rowSums(is.na(homevisit["lat"])), ]
homevisit <-homevisit[!rowSums(is.na(homevisit["long"])), ]

# str(homevisit)


####### Recover correct Governorate & district using the coordinates
source("code/geo.R")

### Spatial join on district 
## getting correct district and gov from coordinates
districtgeo <- readOGR("geo/district.geojson", "OGRGeoJSON")

## Create a spatial data frame with vaf records
rm(datasp)
datasp <-homevisit[ , c("long", "lat")]
#3ploting for a quick check
##plot(datasp)
coords <- cbind(datasp$long, datasp$lat)
datasp0 <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

plot(datasp0)

## Intersection of points with district 
datasp1 <- IntersectPtWithPoly(datasp0, districtgeo)
datasp1@data$id <- rownames(datasp1@data)
rm(correct)
#View(districtgeo@data)
correct <- datasp1@data[ ,c("id","district_c","district","Gov_NAME","Gov_code" )]
homevisit$id <- rownames(homevisit)
homevisit <- merge(x=homevisit, y=correct, by="id")
rm(correct)

## Let's create distinc variables for each records on gov


######################################################
#Replace NA with 0 in data.frame:
# Replace Null with 0 in data.frame:

homevisit[is.na(homevisit)]<-0


############## incorrect family size, incorrect number of rooms)
#Eliminate Rows with Family Size is = 0 and Family Size is >= 20 (Total of 10 records present of family size of 0, 500 and 5000):
homevisit$Household.information.Family.Size <- as.numeric(homevisit$Household.information.Family.Size)
homevisit <- homevisit [!(homevisit$Household.information.Family.Size==0),]
homevisit <- homevisit [!(homevisit$Household.information.Family.Size >= 20),]
#View(homevisit)


#Eliminate Rows with Number of Rooms is = 0 and Number of Rooms is >= 10 (Total of 24 records present with 0, 14, 16 and 60 rooms):

#View(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)

homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities. <- as.numeric(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)
homevisit <- homevisit [!(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.== 0),]
homevisit <- homevisit [!(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities. >= 10),]


#View(vafbaseline$Volunteer..Case.Status...Available) ##  not present in Df... 
#vafbaseline <- vafbaseline [!(vafbaseline$Volunteer..Case.Status...Available==0),]



#View(homevisit$Financial.Situation.Total.Expenditure)
homevisit$Financial.Situation.Total.Expenditure<- as.numeric(homevisit$Financial.Situation.Total.Expenditure)
homevisit <- homevisit [!(homevisit$Financial.Situation.Total.Expenditure==0),]
homevisit <- homevisit [!(homevisit$Financial.Situation.Total.Expenditure > 1000),]



