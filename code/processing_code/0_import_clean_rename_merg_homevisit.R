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
label.v3 <- read.csv("label/homevisit3_label.csv", na.strings="", stringsAsFactors=FALSE)


## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit.v3) <- label.v3[, 3]
#rm(label.v3)

# 
#str(homevisit)


##############################################################
## Home visit 4  doing the same with the variable dictionnary

#
homevisit.v4 <- read.csv("data/Home_visit_4.csv", skip=1)

#homevisit.v41 <- read.csv("data/Home_visit_4_3.csv", skip=1)
#write.csv(names(homevisit.v40), file="out/labelv40.csv")
#write.csv(names(homevisit.v41), file="out/labelv41.csv")

label.v4 <- read.delim("label/homevisit4_label.tsv", stringsAsFactors=FALSE)
## Remove the lines from the dictionnary that are not in v4
label.v4.only <- label.v4[ (label.v4$v4=="yes"), ]

## Checking that variables are well aligned
#label.v4.test <- as.data.frame(names(homevisit.v4))

#write.csv(label.v4.only, file="out/labelv4only.csv")
#write.csv(label.v4.test, file="out/labelv4test.csv")

names(homevisit.v4) <- label.v4.only[, 9]


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

homevisit.all <- rbind(homevisit.v4.r, homevisit.v3.r)

homevisit.all$dataset <- as.factor(homevisit.all$dataset)
#levels(homevisit.all$dataset)

summary(homevisit.all$dataset)

homevisit <- homevisit.all 


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


summary(homevisit$dataset)

### Eliminate record with obviously wrong coordinates
homevisit <-homevisit[(homevisit["lat"] <=  34), ]
homevisit <-homevisit[(homevisit["lat"] >= 29), ]
homevisit <-homevisit[(homevisit["long"] <= 40 ), ]
homevisit <-homevisit[(homevisit["long"] >= 34 ), ]
# str(homevisit)

summary(homevisit$dataset)

####### Recover correct Governorate & district using the coordinates
#source("code/processing_code/geo.R")



######################################################
#Replace NA with 0 in data.frame:
# Replace Null with 0 in data.frame:

homevisit[is.na(homevisit)]<-0
summary(homevisit$dataset)

############## incorrect family size, incorrect number of rooms)
#Eliminate Rows with Family Size is NA, 0 or >= 20 
homevisit$Household.information.Family.Size <- as.numeric(homevisit$Household.information.Family.Size)

homevisit <- homevisit [!(homevisit$Household.information.Family.Size==0),]
homevisit <- homevisit [!(homevisit$Household.information.Family.Size >= 20),]

homevisit <- homevisit[!rowSums(is.na(homevisit["Household.information.Family.Size"])), ]
summary(homevisit$dataset)
#View(homevisit)

homevisit.back <- homevisit 
homevisit <-homevisit.back
#Eliminate Rows with Number of Rooms is = 0 and Number of Rooms is >= 10 (Total of 24 records present with 0, 14, 16 and 60 rooms):

str(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)

homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.1 <- as.numeric(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)
summary(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.1)

hist(homevisit[homevisit$dataset=="homevisit4",]$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.1)

homevisit <- homevisit[!(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.1== 0),]
#homevisit <- homevisit[!(homevisit$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.1 >= 10),]
summary(homevisit$dataset)

#View(homevisit$Volunteer..Case.Status...Available) ##  not present in Df... 
homevisit <- homevisit [!(homevisit$Volunteer..Case.Status...Reachable == 0),]



#View(homevisit$Financial.Situation.Total.Expenditure)
homevisit$Financial.Situation.Total.Expenditure<- as.numeric(homevisit$Financial.Situation.Total.Expenditure)
homevisit <- homevisit [!(homevisit$Financial.Situation.Total.Expenditure==0),]
homevisit <- homevisit [!(homevisit$Financial.Situation.Total.Expenditure > 1000),]


#levels(homevisit$dataset)
summary(homevisit$dataset)
