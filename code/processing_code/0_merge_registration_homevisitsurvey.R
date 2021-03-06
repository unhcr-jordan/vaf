####################################################################################
## Noew let's merge home visit with progres data


#source("code/processing_code/0_import_collapse_merge_progres_data.R")
#source("code/processing_code/0_import_clean_rename_merg_homevisit.R")

source("code/processing_code/packages.R")

#View(homevisit3$Household.information.progres.case.File.Number)
#View(progres.case.max$ProcessingGroupNumber)

## We may need to clean the case number before joining
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


homevisit$clean.UNHCR.File.Number <- trim(homevisit$Household.information.UNHCR.File.Number)

homevisit.progres <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber", all.x=TRUE)

## Checking homevisits where no join was possible --
names(homevisit.progres)
#View(homevisit.progres$Num_Inds)
homevisit.progres.nojoin <- homevisit.progres[ which(is.na(homevisit.progres$Num_Inds)), c("Household.information.UNHCR.File.Number",
                                                                                           "Volunteer..Name.",  "Volunteer..Phone.Number.","dataset") ] 
summary(homevisit.progres.nojoin$dataset)

write.csv(homevisit.progres.nojoin, file = "out/progres-only/homevisit-progres-nojoin.csv",na="")

homevisit.progres.join <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber")
#homevisit.progres.join2 <- merge(x=homevisit, y= progres.case, by.x="clean.UNHCR.File.Number", by.y="ProcessingGroupNumber")


#names(homevisit.progres)
## Checking the result of the merge
#View(homevisit.progres$Num_Inds)
