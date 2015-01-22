########### Clean Home Visit dataset and prepare the dataviz format
## Precleaned dataset 

source("code/packages.R")

rm(hv)
hv <- read.csv("data/HomeVisitFinal2.csv")
hv$id <- rownames(hv)

hv <-hv[!rowSums(is.na(hv["GPSLongitu"])), ]
hv <-hv[!rowSums(is.na(hv["GPSLatitud"])), ]


######## hexagrid
hexa <- readOGR("geo/hexagrid.geojson", "OGRGeoJSON")
#plot(hexa)

rm(datasp)
datasp <-hv[ , c("GPSLongitu", "GPSLatitud")]
summary(datasp)
coords <- cbind(datasp$GPSLongitu, datasp$GPSLatitud)
datasp <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

datasp1 <- IntersectPtWithPoly(datasp, hexa)
datasp1@data$id <- rownames(datasp1@data)
rm(correct)
correct <- datasp1@data[ ,c("id","gid" )]
hv <- merge(x=hv, y=correct, by="id")

#plot(datasp)

#View(datasp1@data)
#names(hv)
## Subset the right columns
dataviz <-hv[ , c("SexCode", "IndividualAge", "Case.Size",  
                  "Recommended_for_cash_Assessment" , "Decision_for_cash_Assistance"  ,
                   "gid", "Governorate.name", 
                  
                  # Income
                  "IncomefromWork" ,"IncomefromPension",   "IncomefromAssetsinSyria",
                  "IncomefromTransfers", "IncomefromOtherOrganizations" ,
                  "IncomefromDonations", "OtherIncome", 
                  
                  #Expenses
                  "BillsExpenses", "FoodExpenses"  , "RentExpenses" ,
                  "TreatmentExpenses", "EducationExpenses" ,"OtherExpenses"
                  
                  #Cost
                 # "FoodCost"  ,"waterCost", "HousingCost",  "Electricity_gasCost" ,
                 # "Medicalcost",  "Clothes_shoesCost", "HouseholditemCost"  , "EducationCost" ,
                 # "TransportationCost"
                  )]



## classify Governorate

levels(dataviz$Governoratename)
dataviz$Governoratename <- revalue(dataviz$Governoratename, c(
  "Ajlun"= "1.North/Ajlun",  "Amman" = "2.Center/Amman",  "Aqaba"= "3.South/Aqaba",   "Balqa"= "2.Center/Balqa",
  "Irbid"= "1.North/Irbid",   "Jarash"= "1.North/Jarash",  "Karak"= "3.South/Karak", 
  "Maan"= "3.South/Maan",    "Madaba"= "2.Center/Madaba",   "Mafraq"= "1.North/Mafraq",  "Tafilah"= "3.South/Tafileh",
  "Zarqa" = "2.Center/Zarqa"))

## classify age 

dataviz$IndividualAge1 <- as.factor(findCols(classIntervals(dataviz$IndividualAge, n = 5, style = "fixed", fixedBreaks = c(0,11, 17 , 35, 59, 110))))
dataviz$IndividualAge1 <- revalue(dataviz$IndividualAge1, c(`1` = "a.0-11", `2` = "b.12-17", `3` = "c.18-35", `4` = "d.36-59", `5` = "e.>60"))


## classify case

dataviz$CaseSize1 <- as.factor(findCols(classIntervals(dataviz$Case.Size, n = 8, style = "fixed", fixedBreaks = c(0,1, 2 , 3, 4, 5,7,9,10))))
dataviz$CaseSize1 <- revalue(dataviz$CaseSize1, c(`1` = "a.1", `2` = "b.2", `3` = "c.3", `4` = "d.4", `5` = "e.5", `6` = "f.5-6", `7`= "g.7-9", `8`="h.>10"))

### Classify budget
dataviz$IncomefromWork1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromWork, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromWorkcl <- revalue(dataviz$IncomefromWork1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$IncomefromPension1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromPension, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromPensioncl <- revalue(dataviz$IncomefromPensionk1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$IncomefromAssetsinSyria1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromAssetsinSyria, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromAssetsinSyriacl <- revalue(dataviz$IncomefromAssetsinSyria1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$IncomefromTransfers1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromTransfers, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromTransferscl <- revalue(dataviz$IncomefromTransfers1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$IncomefromOtherOrganizations1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromOtherOrganizations, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromOtherOrganizationscl <- revalue(dataviz$IncomefromOtherOrganizations1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$IncomefromDonations1cl <- as.factor(findCols(classIntervals(dataviz$IncomefromDonations, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$IncomefromDonationscl <- revalue(dataviz$IncomefromDonations1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$OtherIncome1cl <- as.factor(findCols(classIntervals(dataviz$OtherIncome, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$OtherIncomecl <- revalue(dataviz$OtherIncome1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))


# "BillsExpenses", "FoodExpenses"  , "RentExpenses" , "TreatmentExpenses", "EducationExpenses" ,"OtherExpenses",  

dataviz$BillsExpenses1cl <- as.factor(findCols(classIntervals(dataviz$BillsExpenses, n = 8, style = "fixed", fixedBreaks = c(0,0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$BillsExpensescl <- revalue(dataviz$BillsExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$FoodExpenses1cl <- as.factor(findCols(classIntervals(dataviz$FoodExpenses, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$FoodExpensescl <- revalue(dataviz$FoodExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$RentExpenses1cl <- as.factor(findCols(classIntervals(dataviz$RentExpenses, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$RentExpensescl <- revalue(dataviz$RentExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$TreatmentExpenses1cl <- as.factor(findCols(classIntervals(dataviz$TreatmentExpenses, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$TreatmentExpensescl <- revalue(dataviz$TreatmentExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$EducationExpenses1cl <-as.factor( findCols(classIntervals(dataviz$EducationExpenses, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$EducationExpensescl <- revalue(dataviz$EducationExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))

dataviz$OtherExpenses1cl <- as.factor(findCols(classIntervals(dataviz$OtherExpenses, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 25 , 50, 75, 100, 150, 200, 30000))))
dataviz$OtherExpensescl <- revalue(dataviz$OtherExpenses1cl, c(`1` = "a.No Value", `2` =  "b.1-10",`3` = "c.10-25", `4` = "d.25-50", `5` =  "e.50-75",`6` = "f.75-100", `7`= "g.100-150",`8`=  "h.150-200",`9`= "i.>200"))


dataviz$totalincome <- psum(dataviz$IncomefromWork ,dataviz$IncomefromPension,   dataviz$IncomefromAssetsinSyria,dataviz$IncomefromTransfers, dataviz$IncomefromOtherOrganizations,dataviz$IncomefromDonations, dataviz$OtherIncome, na.rm = TRUE)
dataviz$totalcost <- psum(dataviz$BillsExpenses ,dataviz$FoodExpenses ,dataviz$RentExpenses, 
                          dataviz$TreatmentExpenses ,dataviz$EducationExpenses ,dataviz$OtherExpenses, na.rm = TRUE)

dataviz$totalincome1cl <- as.factor(findCols(classIntervals(dataviz$totalincome, n = 8, style = "fixed", fixedBreaks = c(0, 0.1,10, 28 , 50, 68, 100, 150, 200, 30000))))
dataviz$totalincomecl <- revalue(dataviz$totalincome1cl, c(`1` = "a.No Value", `2` =  "b.1-10", `3` = "c.10-28", `4` = "d.28-50", `5` = "e.50-68", `6` = "f.68-100", `7` = "g.100-150", `8`= "h.150-200", `9`="i.>200"))

dataviz$totalcost1cl <- as.factor(findCols(classIntervals(dataviz$totalcost, n = 8, style = "fixed", fixedBreaks = c(0, 0.1, 10, 28 , 50, 68, 100, 150, 200, 30000))))
dataviz$totalcostcl <- revalue(dataviz$totalcost1cl, c(`1` = "a.No Value", `2` =  "b.1-10", `3` = "c.10-28", `4` = "d.28-50", `5` = "e.50-68", `6` = "f.68-100", `7` = "g.100-150", `8`= "h.150-200", `9`="i.>200"))

dataviz <-rename(dataviz, c("Case.Size" ="CaseSize", "Governorate.name"="Governoratename", 
                            "Decision_for_cash_Assistance"= "Decision","gid"="code"))

dataviz$cnt <- 1

#dataviz$totalgap <- dataviz$totalncome - dataviz$totalcost

#qplot(data = dataviz, x = FoodCost) + ylab("gap")
#qplot(data = dataviz, x = totalcost) + ylab("gap")
#qplot(data = dataviz, x = totalgap) + ylab("gap")
#qplot(data = dataviz, x = totalincome) + ylab("gap")

write.csv(dataviz, file="out/dataviz.csv",row.names=F, na="")

### Extraction for the map in the HV report

rm(map)
map <-hv[ , c("Case.Size",  
              "Recommended_for_cash_Assessment" , 
               "Governorate.name", "GPSLongitu", "GPSLatitud")]

map <-map[!rowSums(is.na(map["GPSLongitu"])), ]
map <-map[!rowSums(is.na(map["GPSLatitud"])), ]

map <-rename(map, c("Case.Size" ="CaseSize", "Governorate.name"="Governoratename", 
                            "Recommended_for_cash_Assessment"= "Recommended",
                            "GPSLongitu" ="long","GPSLatitud" ="lat"                    
                            ))

map$lat <- formatC(map$lat, format="f", digits=4)
map$long <- formatC(map$long, format="f", digits=4)
write.csv(map, file="out/vaf.csv",row.names=F, na="")

