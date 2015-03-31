################################################################
#### Import data, Rename variables & Clean rows


################################################################
#### Import data
## Extracted from https://www.unhcrmenadagdata.org/RaisJordan
## The export has two line of header so we need to skip the first one
homevisit.v3 <- read.csv("data/Home_visit_version3.csv", skip=1)



################################################################
#### Rename variables 
## Label are not easily legible
## Labels have been manually reviewed -- elimination special characters such / or ? or - 
## Labels are also  trimmed when necessary so that they are less than 256 char long - 
## This will allow the rest of the script to run smoothly
label.v3 <- read.csv("data/homevisit3_label.csv", na.strings="", stringsAsFactors=FALSE)


## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit.v3) <- label.v3[, 3]
rm(label.v3)

homevisit <- homevisit.v3
# 
#str(homevisit)


##############################################################
## Home visit 4  doing the same with the variable dictionnary

homevisit.v4 <- read.csv("data/Home_visit_4.csv", skip=1)
label.v4 <- read.delim("data/homevisit4_label.tsv", stringsAsFactors=FALSE)
## Remove the lines from the dictionnary that are not in v4
label.v4 <- label.v4[ (label.v4$v4=="yes"), ]
names(homevisit.v4) <- label.v4[, 9]
rm(label.v4)
homevisit <- homevisit.v4

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



