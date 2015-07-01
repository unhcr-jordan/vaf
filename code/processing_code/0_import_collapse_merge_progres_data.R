##########################################################################
#### Import data 

##source("code/processing_code/progres_connection.R")
## Extracted from Weekely DataWarehouse
progres.case <- read.csv("data/progrescase.csv")


###############################################
## Re-encoding proGres variables

##########
# Legality of arrival:
progres.case$arr_legal <- ifelse(progres.case$arr_status == "Legal" |
                                   progres.case$arr_status ==  "Legal - Medical Visa", 1, 0)

progres.case$arr_illegal <- ifelse(progres.case$arr_status == "Informal" |
                                     progres.case$arr_status ==  "Smuggled", 1, 0)

##########
# Age group:
progres.case$dem_age_grp1 <- ifelse(progres.case$dem_age < 35, 1, 0)

progres.case$dem_age_grp2 <- ifelse((progres.case$dem_age >= 35) &
                                      (progres.case$dem_age < 55), 1, 0)

progres.case$dem_age_grp3 <- ifelse(progres.case$dem_age >= 55, 1, 0)

progres.case$dem_PA_grp0 <- ifelse(progres.case$dem_age < 15, 1, 0)

progres.case$dem_PA_grp1 <- ifelse(progres.case$dem_age < 18, 1, 0)

progres.case$dem_PA_grp2 <- ifelse((progres.case$dem_age > 17) &
                                     (progres.case$dem_age < 60), 1, 0)

progres.case$dem_PA_grp3 <- ifelse(progres.case$dem_age > 59, 1, 0)


##########
# Marital status:
progres.case$mar_widow <- ifelse(progres.case$dem_marriage == "WD Widowed", 1, 0) # niente
progres.case$mar_single <- ifelse(progres.case$dem_marriage == "SN Single", 1, 0) # significativa e buon R2
progres.case$mar_divorced <- ifelse(progres.case$dem_marriage == "DV Divorced", 1, 0) # significativa ma poco R2
progres.case$mar_married <- ifelse(progres.case$dem_marriage == "MA Married", 1, 0) # significative e OK R2
progres.case$mar_engaged <- ifelse(progres.case$dem_marriage == "EG Engaged", 1, 0) # significativa ma poco R2
progres.case$mar_g_divorced <- ifelse((progres.case$dem_marriage == "DV Divorced" | progres.case$dem_marriage == "SR Separated"), 1, 0) # significativa ma poco R2
progres.case$mar_g_married <- ifelse((progres.case$dem_marriage == "MA Married" | progres.case$dem_marriage == "CL Common Law Married" | progres.case$dem_marriage == "EG Engaged"), 1, 0) # significativa e OK R2 ma meno rispetto a married-only

##########
# Ethnicity, religion, birth:
progres.case$ethn_arab <- ifelse(dem_ethn == "Arab", 1, 0)
progres.case$rel_sunni <- ifelse(dem_religion == "SUN Sunni", 1, 0)
progres.case$bir_syria <- ifelse(dem_birth_country == "SYR", 1, 0)

##########
# Gender PA:
progres.case$gender.male <- ifelse(dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(dem_sex == "Female", 1, 0)

##########
# Educational attainment:
progres.case$edu.highest.grp1 <- ifelse((progres.case$edu_highest == "NE No education" | 
                                           progres.case$edu_highest == "KG Kindergarten" | 
                                           progres.case$edu_highest == "1 year (or Grade 1)" | 
                                           progres.case$edu_highest == "2 years (or Grade 2)" | 
                                           progres.case$edu_highest == "3 years (or Grade 3)" | 
                                           progres.case$edu_highest == "4 years (or Grade 4)" | 
                                           progres.case$edu_highest == "5 years (or Grade 5)"), 1, 0)

progres.case$edu.highest.grp2 <- ifelse((progres.case$edu_highest == "6 years (or Grade 6)" | 
                                           progres.case$edu_highest == "7 years (or Grade 7)" | 
                                           progres.case$edu_highest == "8 years (or Grade 8)"), 1, 0)

progres.case$edu.highest.grp3 <- ifelse((progres.case$edu_highest == "9 years (or Grade 9)" | 
                                           progres.case$edu_highest == "10 years (or Grade 10)" | 
                                           progres.case$edu_highest == "11 years (or Grade 11)") , 1, 0)

progres.case$edu.highest.grp4 <- ifelse((progres.case$edu_highest == "12 years (or Grade 12)" | 
                                           progres.case$edu_highest == "13 years (or Grade 13)" | 
                                           progres.case$edu_highest == "14 years (or Grade 14)") , 1, 0)

progres.case$edu.highest.grp5 <- ifelse((progres.case$edu_highest == "PG Post university level" | 
                                           progres.case$edu_highest == "UG University level"), 1, 0)


##########
# Arrival crosspoint:
progres.case$arr.crosspoint.grp1 <- ifelse((progres.case$arr_crosspoint == "C - Airport Queeen Alia - Aleppo" |
                                              progres.case$arr_crosspoint == "C - Airport Queeen Alia - Damascus" |
                                              progres.case$arr_crosspoint == "C - Airport Queeen Alia - Third countries"), 1, 0)

progres.case$arr.crosspoint.grp2 <- ifelse((progres.case$arr_crosspoint == "E - Ruwaished - Hadallat"), 1, 0)

progres.case$arr.crosspoint.grp3 <- ifelse((progres.case$arr_crosspoint == "W - Jaber (Nassib) - Official" |
                                              progres.case$arr_crosspoint == "W - Jaber (Nassib) unofficial"), 1, 0)

progres.case$arr.crosspoint.grp4 <- ifelse(progres.case$arr_crosspoint == "W - Altura - Tel Shehab (Tal Shihab)", 1, 0)

progres.case$arr.crosspoint.grp5 <- ifelse((progres.case$arr_crosspoint == "E - Al Karama" |
                                              progres.case$arr_crosspoint == "E - Al Rogoban (T22) - Sub Rogoban" |
                                              progres.case$arr_crosspoint == "E - Matwie" |
                                              progres.case$arr_crosspoint == "No Data" | 
                                              progres.case$arr_crosspoint == "S - Al Omari" |
                                              progres.case$arr_crosspoint == "S - Aqaba" |
                                              progres.case$arr_crosspoint == "W - Al Akaider - Dara" | 
                                              progres.case$arr_crosspoint == "W - Al Qaryatayn (Iretian )" |
                                              progres.case$arr_crosspoint == "W - Al Thunaibah - Wadi Shallalah - Al Hayt" |
                                              progres.case$arr_crosspoint == "W - Qaraqush - Unity Dam - Al Hayt" |
                                              progres.case$arr_crosspoint == "W - Qlaid - (Yarmouk river) Alshajarah" |
                                              progres.case$arr_crosspoint == "W - Ramtha - Dara" |
                                              progres.case$arr_crosspoint == "W - Sama Al Sarhan (Post 58) - Tiba (Al Taebah)" |
                                              progres.case$arr_crosspoint == "W - Um Esrab - Tiba - (Al Taebah)" | 
                                              progres.case$arr_crosspoint == "W - Umm Al qittayn - Khirbat Awwad - Al Mughayir" | 
                                              progres.case$arr_crosspoint == "NA's"), 1, 0)

##########
# Case Size:
progres.case$case.size.pg <- (progres.case$csize_act)
progres.case$case.size.pg.sq <- (progres.case$csize_act)^2

##########
# Case size as dummies:
progres.case$case.size.pg <- (progres.case$csize_act)
progres.case$case.size.pg.1 <- ifelse(progres.case$case.size.pg == 1, 1, 0)
progres.case$case.size.pg.2 <- ifelse(progres.case$case.size.pg == 2, 1, 0)
progres.case$case.size.pg.3 <- ifelse(progres.case$case.size.pg == 3, 1, 0)
progres.case$case.size.pg.4 <- ifelse(progres.case$case.size.pg == 4, 1, 0)
progres.case$case.size.pg.5 <- ifelse(progres.case$case.size.pg == 5, 1, 0)
progres.case$case.size.pg.6 <- ifelse(progres.case$case.size.pg == 6, 1, 0)
progres.case$case.size.pg.7 <- ifelse(progres.case$case.size.pg == 7, 1, 0)
progres.case$case.size.pg.8plus <- ifelse(progres.case$case.size.pg >= 8, 1, 0)


##########
# Destination:
progres.case$ajloun.pg <- ifelse(progres.case$admlevel1 == "Ajloun", 1, 0)
progres.case$amman.pg <- ifelse(progres.case$admlevel1 == "Amman", 1, 0)
progres.case$aqabah.pg <- ifelse(progres.case$admlevel1 == "Aqabah", 1, 0)
progres.case$balqa.pg <- ifelse(progres.case$admlevel1 == "Balqa", 1, 0)
progres.case$irbid.pg <- ifelse(progres.case$admlevel1 == "irbid", 1, 0)
progres.case$jarash.pg <- ifelse(progres.case$admlevel1 == "Jarash", 1, 0)
progres.case$karak.pg <- ifelse(progres.case$admlevel1 == "Karak", 1, 0)
progres.case$maan.pg <- ifelse(progres.case$admlevel1 == "Maan", 1, 0)
progres.case$madaba.pg <- ifelse(progres.case$admlevel1 == "Madaba", 1, 0)
progres.case$mafraq.pg <- ifelse(progres.case$admlevel1 == "Mafraq", 1, 0)
progres.case$tafilah.pg <- ifelse(progres.case$admlevel1 == "Tafilah", 1, 0)
progres.case$zarqa.pg <- ifelse(progres.case$admlevel1 == "Zarqa", 1, 0)




