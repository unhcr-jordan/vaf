## load packages
#
source("code/packages.R")

## For the record -- orginal dataset
#VAFdata <- read.csv("data/VAFdata.csv")
#vafnames <- as.data.frame(names(VAFdata))

### Extract from Marco
vaf.or <- read.csv("data/vaf-or2.csv")
names(vaf.or)

### Rename column

rm(vafout)
 vafout <-rename(vaf.or, c("X.Date.of.Visit." ="HVDate", "House.Crowding"="crowding", "Household.information.Family.Size"="size", 
                           "Entry.into.Jordan.When.did.the.members.of.your.family.arrive.from.Syria..First.arrival...first.family.member.."="EiJ",
                           "Welfare.Score"="welfare", "Household.information.address.."="geo",
                           "MOI.Service.Card.Are.you.still.in.the.governorate.where.you.registered.with.MOI...No" ="moino",
                           "MOI.Service.Card.Are.you.still.in.the.governorate.where.you.registered.with.MOI...Yes" ="moiyes", 
                           "UNHCR.Asylum.Seeker.Certificate.Do.you.have.a.valid.registration.with.UNHCR...Look.at.expiry.date...Yes"= "asyes",
                           "UNHCR.Asylum.Seeker.Certificate.Do.you.have.a.valid.registration.with.UNHCR...Look.at.expiry.date...No"= "asno",
                           "MOI.Service.Card.Do.you.have.a.valid.MOI.service.card...Yes"= "moicardyes",
                           "MOI.Service.Card.Do.you.have.a.valid.MOI.service.card...No"= "moicardno"
                           ))

## Try merge with ID for District and Govenorate
#dis <- read.csv("data/dis.csv")
#gov <- read.csv("data/gov.csv")

#vafout <- merge (x=vafout, y=gov, by.x="Household.information.Governorate.", by.y="gov", all.x=TRUE)

#vafout <- merge (x=vafout, y=dis, by.x="Household.information.District.", by.y="district", all.x=TRUE)
## -> possible wrong entry

## getting correct district and gov from coordinates
districtgeo <- readOGR("geo/district.geojson", "OGRGeoJSON")
## No need to load govenorate as we will get correct code for gov trhough the district
##governorategeo <- readOGR("geo/admin1.geojson", "OGRGeoJSON")

## extracting coordinates from dataset
vafout$geo <- as.character(vafout$geo)
options(digits = 15)
vafout$lat <- as.numeric(substr(vafout$geo , 1,13))
vafout$long <- as.numeric(substr( vafout$geo, 15,27))


### Eliminate record without coordinates
vafout <-vafout[!rowSums(is.na(vafout["lat"])), ]
vafout <-vafout[!rowSums(is.na(vafout["long"])), ]

# str(vafout)

## Create a spatial data frame with vaf records
rm(datasp)
datasp <-vafout[ , c("long", "lat")]
# summary(datasp)
coords <- cbind(datasp$long, datasp$lat)
datasp <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

#plot(datasp)

## Intersection of points with district 
datasp1 <- IntersectPtWithPoly(datasp, districtgeo)
datasp1@data$id <- rownames(datasp1@data)
rm(correct)
#View(districtgeo@data)
correct <- datasp1@data[ ,c("id","district_c","district","Gov_NAME","Gov_code" )]
vafout$id <- rownames(vafout)
vafout <- merge(x=vafout, y=correct, by="id")
rm(correct)


## Rename threeshold factor

# levels(vafout$Thresholds)
vafout$Thresholds <- revalue(vafout$Thresholds, c("extremely"="1-Extremely",  "highly"="2-Highly",     "low"  = "4-Low",      "moderately"= "3-Moderately"))

## Reformat date and substract records with empty date
vafout$EiJ <- as.Date(vafout$EiJ, "%m/%d/%Y")
vafout$EiJ <- format(vafout$EiJ, "%d/%m/%Y")
vafout$HVDate <- as.Date(vafout$HVDate, "%m/%d/%Y")
vafout$HVDate <- format(vafout$HVDate, "%d/%m/%Y")

vafout <-vafout[!rowSums(is.na(vafout["HVDate"])), ]
vafout <-vafout[!rowSums(is.na(vafout["EiJ"])), ]


### Compile new variable
vafout$ASCMOI[vafout$moino=='1']  <- "Same address" 
vafout$ASCMOI[is.na(vafout$moino)]  <- "Different address" 
vafout$ASCMOI[is.na(vafout$moino) & is.na(vafout$moiyes) ]  <- "Unknown" 


vafout$MOIREG[(vafout$asyes=='1') & (vafout$moicardyes=="1")]  <- "Both Card"
vafout$MOIREG[(vafout$asno=='1') & (vafout$moicardyes=="1")]  <- "Non valid UNHCR Card - Valid MoI Card"
vafout$MOIREG[(vafout$asyes=='1') & (is.na(vafout$moicardyes))]  <- "Only valid UNHCR Card - MoI Card unknwon"
vafout$MOIREG[(vafout$asno=='1') & (is.na(vafout$moicardyes))]  <- "Non valid UNHCR Card - MoI Card unknwon"

vafout$MOIREG[(vafout$asyes=='1') & (vafout$moicardno=="1")]  <- "Only valid UNHCR Card"
vafout$MOIREG[(vafout$asno=='1') & (vafout$moicardno=="1")]  <- "Non valid UNHCR Card - No MoI Card"

#summary(vafout$MOIREG)


#check <- vafout[ , c("asyes","asno","moicardyes","moicardno","MOIREG")]
#check <- check[is.na(check$MOIREG)]

# vafout <- read.csv("out/vafdata.csv")
# names(vafout)

vafout <- vafout[ , c( "HVDate", "EiJ"  ,"crowding" ,   "Income.Per.Capita" , "Debt.to.Expenditure.Ratio" , "size" ,  "geo" ,
                      "welfare"  , "Thresholds" ,  "lat"  , "long"  , "district_c", "district", "Gov_NAME" ,"Gov_code",
                       "MOIREG" ,  "ASCMOI", "Household.information.UNHCR.File.Number"   )]
vafout <- rename(vafout, c(  "Income.Per.Capita"="income" , "Debt.to.Expenditure.Ratio"="debtexp" ,
                           "district_c"="dcode", "Gov_code"= "gcode", "Gov_NAME"="Governorate", "district"="District" ))

progress <- read.csv("~/unhcr_r_project/homevisit/data/IMAGG.csv")
names(progress)


#### Now merging with proGres
## Rename progress case size 
progress <-rename(progress, c("Total" ="sizep"))

vafout <- merge (x=vafout, y=progress, by.x="Household.information.UNHCR.File.Number", by.y="UNHCRcaseID", all.x=TRUE)

uniqueprogress.vaf <- unique(vafout$Household.information.UNHCR.File.Number) ##8353
uniqueprogress.progress <- unique(progress$UNHCRcaseID) ## 8165

progress.vaf <- as.data.frame(vafout$Household.information.UNHCR.File.Number)

# Show the repeated progress case entries
progress.vaf.dup <- as.data.frame(progress.vaf[duplicated(progress.vaf),])
progress.vaf.dup <- rename(progress.vaf.dup, c("progress.vaf[duplicated(progress.vaf), ]" ="caseid"))
progress.vaf.dup <- progress.vaf.dup[order(caseid),] 
# Show unique repeat entries 
progress.vaf.dup.un <-unique(vafout[duplicated(vafout$progress.vaf),])

### identify VAF record related to same prograss code


vafout-nopro <-


vafout.p <- merge (x=vafout, y=progress, by.x="Household.information.UNHCR.File.Number", by.y="UNHCRcaseID")
vafout.v <- merge (x=progress,y=vafout,  by.x="UNHCRcaseID", by.y="Household.information.UNHCR.File.Number", all.x=TRUE)





## Create a counter
vafout$Total <- 1
#vafout$malebaby <- 1
#vafout$femalebaby <- 1
#vafout$malekid <- 1
#vafout$femalekid <- 1
#vafout$maleyouth <- 1
#vafout$femaleyouth <- 1
#vafout$maleadult <- 1
#vafout$femaleadult <- 1
#vafout$maleeldern <- 1
#vafout$femaleeldern <- 1

#names(vafout)


vafout$geo <- paste( formatC(vafout$lat, format="f", digits=4) , formatC(vafout$long, format="f", digits=4) , sep=",")

## anonymise coordiantes by rounding them
vafout$Latitude <-formatC(vafout$lat, format="f", digits=4)
vafout$Longitude <-formatC(vafout$long, format="f", digits=4)

vafout1 <- vafout[ , c("HVDate"  , "EiJ"  ,  "crowding" , "income" , "debtexp" ,  "size" ,   "welfare" , "Thresholds" ,      
 "dcode"   ,     "District"    , "Governorate",  "gcode"   ,     "MOIREG"   ,    "ASCMOI"   ,    "Total"   ,"geo", "lat", "long",  "PAgender",
                       "sizep"  ,      "malebaby",     "femalebaby"  , "malekid"    ,  "femalekid"   , "maleyouth"    ,"femaleyouth" , "maleadult"    ,"femaleadult",  "maleeldern"  , "femaleeldern"   )]

write.csv(vafout1, file="out/vafdata2.csv",row.names=F, na="")

vafout2 <- vafout[ , c("HVDate"  , "EiJ"  ,  "crowding" , "income" , "debtexp" ,  "size" ,   "welfare" , "Thresholds" ,  "geo",    
                       "dcode"   ,     "District"    , "Governorate",  "gcode"   ,     "MOIREG"   ,    "ASCMOI"   ,    "Total"   ,     "malebaby"  ,   "femalebaby" ,  "malekid"   ,
                       "femalekid"   , "maleyouth" ,   "femaleyouth" , "maleadult"  ,  "femaleadult" , "maleeldern"  , "femaleeldern"   )]                
                       
## write in a tsv file for the dataviz -- reason for the tsv is to keep the formatting of the column where coordinates are stored
write.table(vafout2, file='out/vafdata2.tsv', quote=FALSE, sep='\t', col.names = T, row.names = F)

# write.csv(vafout, file="/var/www/vulnerability/data/vafdata2.csv",row.names=F, na="")

