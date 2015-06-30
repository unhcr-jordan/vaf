##########################################################################
#### Import data 

##source("code/processing_code/progres_connection.R")
## Extracted from Weekely DataWarehouse
progres.case <- read.csv("data/progrescase.csv")


###############################################
## Re-encoding proGres variables

# Legality of arrival:
progres.case$arr_legal <- ifelse(progres.case$arr_status == "Legal" | progres.case$arr_status ==  "Legal - Medical Visa", 1, 0)
progres.case$arr_illegal <- ifelse(progres.case$arr_status == "Informal" | progres.case$arr_status ==  "Smuggled", 1, 0)

# Age group:
progres.case$dem_age_grp1 <- ifelse(progres.case$dem_age < 35, 1, 0)
progres.case$dem_age_grp2 <- ifelse((progres.case$dem_age >= 35) & (progres.case$dem_age < 55), 1, 0)
progres.case$dem_age_grp3 <- ifelse(progres.case$dem_age >= 55, 1, 0)

progres.case$dem_PA_grp0 <- ifelse(progres.case$dem_age < 15, 1, 0)
progres.case$dem_PA_grp1 <- ifelse(progres.case$dem_age < 18, 1, 0)
progres.case$dem_PA_grp2 <- ifelse((progres.case$dem_age > 17) & (progres.case$dem_age < 60), 1, 0)
progres.case$dem_PA_grp3 <- ifelse(progres.case$dem_age > 59, 1, 0)

# Marital status:
progres.case$mar_widow <- ifelse(dem_marriage == "WD Widowed", 1, 0) # niente
progres.case$mar_single <- ifelse(dem_marriage == "SN Single", 1, 0) # significativa e buon R2
mar_divorced <- ifelse(dem_marriage == "DV Divorced", 1, 0) # significativa ma poco R2
progres.case$mar_married <- ifelse(dem_marriage == "MA Married", 1, 0) # significative e OK R2
progres.case$mar_engaged <- ifelse(dem_marriage == "EG Engaged", 1, 0) # significativa ma poco R2
progres.case$mar_g_divorced <- ifelse((dem_marriage == "DV Divorced" | dem_marriage == "SR Separated"), 1, 0) # significativa ma poco R2
progres.case$mar_g_married <- ifelse((dem_marriage == "MA Married" | dem_marriage == "CL Common Law Married" | dem_marriage == "EG Engaged"), 1, 0) # significativa e OK R2 ma meno rispetto a married-only

# Ethnicity, religion, birth:
progres.case$ethn_arab <- ifelse(dem_ethn == "Arab", 1, 0)
progres.case$rel_sunni <- ifelse(dem_religion == "SUN Sunni", 1, 0)
progres.case$bir_syria <- ifelse(dem_birth_country == "SYR", 1, 0)

# Gender PA:
progres.case$gender.male <- ifelse(dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(dem_sex == "Female", 1, 0)

# Educational attainment:
progres.case$edu.highest.grp1 <- ifelse((edu_highest == "NE No education" | edu_highest == "KG Kindergarten" | edu_highest == "1 year (or Grade 1)" | edu_highest == "2 years (or Grade 2)" | edu_highest == "3 years (or Grade 3)" | edu_highest == "4 years (or Grade 4)" | edu_highest == "5 years (or Grade 5)"), 1, 0)
progres.case$edu.highest.grp2 <- ifelse((edu_highest == "6 years (or Grade 6)" | edu_highest == "7 years (or Grade 7)" | edu_highest == "8 years (or Grade 8)"), 1, 0)
progres.case$edu.highest.grp3 <- ifelse((edu_highest == "9 years (or Grade 9)" | edu_highest == "10 years (or Grade 10)" | edu_highest == "11 years (or Grade 11)") , 1, 0)
progres.case$edu.highest.grp4 <- ifelse((edu_highest == "12 years (or Grade 12)" | edu_highest == "13 years (or Grade 13)" | edu_highest == "14 years (or Grade 14)") , 1, 0)
progres.case$edu.highest.grp5 <- ifelse((edu_highest == "PG Post university level" | edu_highest == "UG University level"), 1, 0)



####################################################################################
## Noew let's merge home visit with progres data

#View(homevisit3$Household.information.progres.case.File.Number)
#View(progres.case.max$ProcessingGroupNumber)

homevisit.progres <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber", all.x=TRUE)
homevisit.progres.join <- merge(x=homevisit, y= progres.case, by.x="Household.information.UNHCR.File.Number", by.y="ProcessingGroupNumber")


#names(homevisit.progres)
## Checking the result of the merge
View(homevisit.progres$Num_Inds)

## Checking duplicate unhcr file number in home visit
homevisit.uniquecase <- unique(homevisit$Household.information.UNHCR.File.Number) ##8353

progress.vaf <- as.data.frame(homevisit$Household.information.UNHCR.File.Number)

# Show the repeated progress case entries
progress.vaf.dup <- as.data.frame(progress.vaf[duplicated(progress.vaf),])
progress.vaf.dup <- rename(progress.vaf.dup, c("progress.vaf[duplicated(progress.vaf), ]" ="caseid"))
progress.vaf.dup <- progress.vaf.dup[order(caseid),] 
# Show unique repeat entries 
progress.vaf.dup.un <-unique(homevisit[duplicated(homevisit$progress.vaf),])

