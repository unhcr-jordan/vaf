####################################################################################
## Noew let's merge home visit with progres data


source("code/processing_code/0_import_collapse_merge_progres_data.R")
source("code/processing_code/0_import_clean_rename_merg_homevisit.R")

source("code/processing_code/packages.R")

#View(homevisit3$Household.information.progres.case.File.Number)
#View(progres.case.max$ProcessingGroupNumber)

homevisit.progres <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber", all.x=TRUE)
homevisit.progres.join <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber")


#names(homevisit.progres)
## Checking the result of the merge
View(homevisit.progres$Num_Inds)