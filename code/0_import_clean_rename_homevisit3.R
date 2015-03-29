################################################################
#### Import data, Rename variables & Clean rows


################################################################
#### Import data
## Extracted from https://www.unhcrmenadagdata.org/RaisJordan
## The export has two line of header so we need to skip the first one
homevisit <- read.csv("data/Home_visit_version3.csv", skip=1)



################################################################
#### Rename variables 
## Label are not easily legible
## Labels have been manually reviewed -- elimination special characters such / or ? or - 
## This will allow the rest of the script to run smoothly
label <- read.csv("data/homevisit_label.csv", na.strings="", stringsAsFactors=FALSE)

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit) <- label[, 3]

homevisit3 <- homevisit
# 
str(homevisit3)
rm(label)





################################################################
####  extracting coordinates from dataset
homevisit3$geo <- as.character(homevisit3$Household.information.address..)
options(digits = 15)
homevisit3$lat <- as.numeric(substr(homevisit3$geo , 1,13))
homevisit3$long <- as.numeric(substr( homevisit3$geo, 15,27))


### Eliminate record without coordinates
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["lat"])), ]
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["long"])), ]

# str(homevisit3)


######################################################
#Replace NA with 0 in data.frame:
# Replace Null with 0 in data.frame:

homevisit3[is.na(homevisit3)]<-0


############## incorrect family size, incorrect number of rooms)
#Eliminate Rows with Family Size is = 0 and Family Size is >= 20 (Total of 10 records present of family size of 0, 500 and 5000):
homevisit3$Household.information.Family.Size <- as.numeric(homevisit3$Household.information.Family.Size)
homevisit3 <- homevisit3 [!(homevisit3$Household.information.Family.Size==0),]
homevisit3 <- homevisit3 [!(homevisit3$Household.information.Family.Size >= 20),]
#View(homevisit3)


#Eliminate Rows with Number of Rooms is = 0 and Number of Rooms is >= 10 (Total of 24 records present with 0, 14, 16 and 60 rooms):

#View(homevisit3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)

homevisit3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities. <- as.numeric(homevisit3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)
homevisit3 <- homevisit3 [!(homevisit3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.== 0),]
homevisit3 <- homevisit3 [!(homevisit3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities. >= 10),]


#View(vafbaseline$Volunteer..Case.Status...Available) ##  not present in Df... 
#vafbaseline <- vafbaseline [!(vafbaseline$Volunteer..Case.Status...Available==0),]



#View(homevisit3$Financial.Situation.Total.Expenditure)
homevisit3$Financial.Situation.Total.Expenditure<- as.numeric(homevisit3$Financial.Situation.Total.Expenditure)
homevisit3 <- homevisit3 [!(homevisit3$Financial.Situation.Total.Expenditure==0),]

