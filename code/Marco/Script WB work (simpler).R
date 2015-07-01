################################################################################################

# Author: Marco Santacroce

# Data: VAF + PG
# Topic: Welfare Model estimation (post World Bank visit March 2015)


################################################################################################
## Import Data:
UNHCR <- read.csv("data/One_Big_list_vaf_with_progres_ANSI_UNIX.txt", na.strings = "")
attach(UNHCR)

# Observations: 26,290

################################################################################################
## Clean Data for HV Model:
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 == 0),] # lose 4 obs.
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 >= 20),] # lose 6 obs.

UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 == 0),] # lose 44 obs.
UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 >= 10),] # lose 24 obs.

UNHCR[is.na(UNHCR)]<-0
attach(UNHCR)

# Expenditure per capita:
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5) # for HV model

exp.pc.2[is.na(exp.pc.2)] <- 0 # for HV model

UNHCR <- UNHCR [!(exp.pc.2 == 0),] # for HV model
attach(UNHCR)
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5) # for HV model
ln.exppc <- log(exp.pc.2) # for HV model
attach(UNHCR)
UNHCR <- UNHCR [!(exp.pc.2 >= 1000),] # for HV model
attach(UNHCR)
exp.pc.2 <- (UNHCR$Financial.Situation.Total.Expenditure_124/UNHCR$Household.information.Family.Size_5) # for HV model
ln.exppc <- log(exp.pc.2) # for HV model

################################################################################################
## Clean Data for PG only Model:
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 == 0),] # lose 4 obs.
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 >= 20),] # lose 6 obs.

UNHCR[is.na(UNHCR)]<-0
attach(UNHCR)

# Expenditure per capita:
exp.pc.1 <- (Financial.Situation.Total.Expenditure_124/csize_act) # for PG model

exp.pc.1[is.na(exp.pc.1)] <- 0 # for PG model

UNHCR <- UNHCR [!(exp.pc.1 == 0),] # for PG model
attach(UNHCR)
exp.pc.1 <- (Financial.Situation.Total.Expenditure_124/csize_act) # for PG model
ln.exppc.pg <- log(exp.pc.1) # for PG model
attach(UNHCR)
UNHCR <- UNHCR [!(exp.pc.1 > 1000),] # for PG model
attach(UNHCR)
exp.pc.1 <- (Financial.Situation.Total.Expenditure_124/csize_act) # for PG model
ln.exppc.pg <- log(exp.pc.1) # for PG model

################################################################################################
## ProGres Variable Generation:

# Legality of arrival:
arr_legal <- ifelse(UNHCR$arr_status == "Legal" | UNHCR$arr_status ==  "Legal - Medical Visa", 1, 0)
arr_illegal <- ifelse(UNHCR$arr_status == "Informal" | UNHCR$arr_status ==  "Smuggled", 1, 0)

# Age group:
dem_PA_grp1 <- ifelse(UNHCR$dem_age < 18, 1, 0)
dem_PA_grp2 <- ifelse((UNHCR$dem_age > 17) & (UNHCR$dem_age < 60), 1, 0)
dem_PA_grp3 <- ifelse(UNHCR$dem_age > 59, 1, 0)

age.PA1 <- ifelse(UNHCR$dem_age < 35, 1, 0)
age.PA2 <- ifelse((UNHCR$dem_age > 34) & (UNHCR$dem_age < 55), 1, 0)
age.PA3 <- ifelse(UNHCR$dem_age > 54, 1, 0)

# Percentage of children:
p.child.grp1 <- ifelse(percentage_0_14 == 0, 1, 0)
p.child.grp2 <- ifelse((percentage_0_14 > 0) & (percentage_0_14 < 50), 1, 0)
p.child.grp3 <- ifelse((percentage_0_14 >= 50) & (percentage_0_14 < 75), 1, 0)
p.child.grp4 <- ifelse(percentage_0_14 >= 75, 1, 0)

# Marital status:
mar_widow <- ifelse(dem_marriage == "WD Widowed", 1, 0) 
mar_single <- ifelse(dem_marriage == "SN Single", 1, 0) 
mar_divorced <- ifelse(dem_marriage == "DV Divorced", 1, 0) 
mar_married <- ifelse(dem_marriage == "MA Married", 1, 0) 
mar_engaged <- ifelse(dem_marriage == "EG Engaged", 1, 0) 
mar_g_divorced <- ifelse((dem_marriage == "DV Divorced" | dem_marriage == "SR Separated"), 1, 0) # significativa ma poco R2
mar_g_married <- ifelse((dem_marriage == "MA Married" | dem_marriage == "CL Common Law Married" | dem_marriage == "EG Engaged"), 1, 0) # significativa e OK R2 ma meno rispetto a married-only
mar.grp1 <- mar_single
mar.grp2 <- mar_g_married
mar.grp3 <- mar_g_divorced
mar.grp4 <- mar_widow

# Ethnicity, religion, birth:
ethn_arab <- ifelse(dem_ethn == "Arab", 1, 0)
rel_sunni <- ifelse(dem_religion == "SUN Sunni", 1, 0)
bir_syria <- ifelse(dem_birth_country == "SYR", 1, 0)

# Gender PA:
gender.male <- ifelse(dem_sex == "Male", 1, 0)
gender.female <- ifelse(dem_sex == "Female", 1, 0)

# Educational attainment:
edu.highest.grp1 <- ifelse((edu_highest == "NE No education" | edu_highest == "KG Kindergarten" | edu_highest == "1 year (or Grade 1)" | edu_highest == "2 years (or Grade 2)" | edu_highest == "3 years (or Grade 3)" | edu_highest == "4 years (or Grade 4)" | edu_highest == "5 years (or Grade 5)"), 1, 0)
edu.highest.grp2 <- ifelse((edu_highest == "6 years (or Grade 6)" | edu_highest == "7 years (or Grade 7)" | edu_highest == "8 years (or Grade 8)"), 1, 0)
edu.highest.grp3 <- ifelse((edu_highest == "9 years (or Grade 9)" | edu_highest == "10 years (or Grade 10)" | edu_highest == "11 years (or Grade 11)") , 1, 0)
edu.highest.grp4 <- ifelse((edu_highest == "12 years (or Grade 12)" | edu_highest == "13 years (or Grade 13)" | edu_highest == "14 years (or Grade 14)") , 1, 0)
edu.highest.grp5 <- ifelse((edu_highest == "PG Post university level" | edu_highest == "UG University level"), 1, 0)
# maybe do group 0 with no education as reference category

# Arrival crosspoint:
arr.crosspoint.grp1 <- ifelse((arr_crosspoint == "C - Airport Queeen Alia - Aleppo" | arr_crosspoint == "C - Airport Queeen Alia - Damascus" | arr_crosspoint == "C - Airport Queeen Alia - Third countries"), 1, 0)
arr.crosspoint.grp2 <- ifelse((arr_crosspoint == "E - Ruwaished - Hadallat"), 1, 0)
arr.crosspoint.grp3 <- ifelse((arr_crosspoint == "W - Jaber (Nassib) - Official" | arr_crosspoint == "W - Jaber (Nassib) unofficial"), 1, 0)
arr.crosspoint.grp4 <- ifelse(arr_crosspoint == "W - Altura - Tel Shehab (Tal Shihab)", 1, 0)
arr.crosspoint.grp5 <- ifelse((arr_crosspoint == "E - Al Karama" | arr_crosspoint == "E - Al Rogoban (T22) - Sub Rogoban" | arr_crosspoint == "E - Matwie" | arr_crosspoint == "No Data" |  arr_crosspoint == "S - Al Omari" | arr_crosspoint == "S - Aqaba" | arr_crosspoint == "W - Al Akaider - Dara" | arr_crosspoint == "W - Al Qaryatayn (Iretian )" | arr_crosspoint == "W - Al Thunaibah - Wadi Shallalah - Al Hayt" | arr_crosspoint == "W - Qaraqush - Unity Dam - Al Hayt" | arr_crosspoint == "W - Qlaid - (Yarmouk river) Alshajarah" | arr_crosspoint == "W - Ramtha - Dara" | arr_crosspoint == "W - Sama Al Sarhan (Post 58) - Tiba (Al Taebah)" | arr_crosspoint == "W - Um Esrab - Tiba - (Al Taebah)" | arr_crosspoint == "W - Umm Al qittayn - Khirbat Awwad - Al Mughayir" | arr_crosspoint == "NA's"), 1, 0)

# Case Size:
case.size.pg <- (csize_act)
case.size.pg.sq <- (csize_act)^2

# Case size as dummies:
case.size.pg <- (csize_act)
case.size.pg.1 <- ifelse(case.size.pg == 1, 1, 0)
case.size.pg.2 <- ifelse(case.size.pg == 2, 1, 0)
case.size.pg.3 <- ifelse(case.size.pg == 3, 1, 0)
case.size.pg.4 <- ifelse(case.size.pg == 4, 1, 0)
case.size.pg.5 <- ifelse(case.size.pg == 5, 1, 0)
case.size.pg.6 <- ifelse(case.size.pg == 6, 1, 0)
case.size.pg.7 <- ifelse(case.size.pg == 7, 1, 0)
case.size.pg.8plus <- ifelse(case.size.pg >= 8, 1, 0)

# Destination:
ajloun.pg <- ifelse(admlevel1 == "Ajloun", 1, 0)
amman.pg <- ifelse(admlevel1 == "Amman", 1, 0)
aqabah.pg <- ifelse(admlevel1 == "Aqabah", 1, 0)
balqa.pg <- ifelse(admlevel1 == "Balqa", 1, 0)
irbid.pg <- ifelse(admlevel1 == "irbid", 1, 0)
jarash.pg <- ifelse(admlevel1 == "Jarash", 1, 0)
karak.pg <- ifelse(admlevel1 == "Karak", 1, 0)
maan.pg <- ifelse(admlevel1 == "Maan", 1, 0)
madaba.pg <- ifelse(admlevel1 == "Madaba", 1, 0)
mafraq.pg <- ifelse(admlevel1 == "Mafraq", 1, 0)
tafilah.pg <- ifelse(admlevel1 == "Tafilah", 1, 0)
zarqa.pg <- ifelse(admlevel1 == "Zarqa", 1, 0)

################################################################################################
## VAF Variable Generation:

# Case Size:
case.size.vaf <- (Household.information.Family.Size_5)
case.size.vaf.sq <- (Household.information.Family.Size_5)^2

# Case size as dummies:
case.size.vaf.1 <- ifelse(case.size.vaf == 1, 1, 0)
case.size.vaf.2 <- ifelse(case.size.vaf == 2, 1, 0)
case.size.vaf.3 <- ifelse(case.size.vaf == 3, 1, 0)
case.size.vaf.4 <- ifelse(case.size.vaf == 4, 1, 0)
case.size.vaf.5 <- ifelse(case.size.vaf == 5, 1, 0)
case.size.vaf.6 <- ifelse(case.size.vaf == 6, 1, 0)
case.size.vaf.7 <- ifelse(case.size.vaf == 7, 1, 0)
case.size.vaf.8plus <- ifelse(case.size.vaf >= 8, 1, 0)
case.size.vaf.8.11 <- ifelse((case.size.vaf >= 8) & (case.size.vaf <=11), 1, 0)
case.size.vaf.12plus <- ifelse(case.size.vaf >= 12, 1, 0)

# Household Size:
hh.size <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9)

# House crowding:
house.crowding.1 <- (csize_act/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)
house.crowding.2 <- (Household.information.Family.Size_5/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)
hh.crowding <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)
UNHCR <- cbind(UNHCR, hh.crowding)

# Rent occupancy:
occup.grp1 <- ifelse(Payment.Type.of.occupancy..For.rent_52 == 1, 1, 0)
occup.grp1[is.na(occup.grp1)] <- 0

occup.grp2 <- ifelse(Payment.Type.of.occupancy..Shelter.provided.through.humanita_53 == 1, 1, 0)
occup.grp2[is.na(occup.grp2)] <- 0

occup.grp3 <- ifelse(Payment.Type.of.occupancy..Hosted..for.free._54 == 1, 1, 0)
occup.grp3[is.na(occup.grp3)] <- 0

occup.grp4 <- ifelse(Payment.Type.of.occupancy..Shelter.provided.in.return.for.wo_55 == 1, 1, 0)
occup.grp4[is.na(occup.grp4)] <- 0

occup.grp5 <- ifelse(Payment.Type.of.occupancy..Squatter..illegal.occupation.of.s_56 == 1, 1, 0)
occup.grp5[is.na(occup.grp5)] <- 0

# Spices and condiments:
sp.co.cash <- ifelse(Over.the.last.7.days..how.many.days.did.you.consume.the.foll_271 == 1, 1, 0)
sp.co.cash[is.na(sp.co.cash)] <- 0

# Kitchen dummy (access to), significant good R2 in lnexp
house.kitchen.d <- (Type.of.Housing.Access.to.kitchen..Yes_15)
house.kitchen.d[is.na(house.kitchen.d)] <- 0

# All people (cases) living in house:
all.case.size <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9)
all.case.size.sq <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9)^2

# MOI registered governorate:
moi.amman <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462
moi.ajloun <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Ajlo_463
moi.aqabah <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Aqab_464
moi.balqa <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Balq_465
moi.irbid <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466
moi.jerash <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Jera_467
moi.karak <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Kara_468
moi.maan <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Maa_469
moi.madaba <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Mada_470
moi.mafraq <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Mafr_471
moi.tafilah <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Tafi_472
moi.zarqa <- MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Zarq_473

# Variables For Welfare Model (identified through stepwise regressions): 
wash.improved.latrine <- Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95
wash.wastewater.sewage <- Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98
wash.wastewater.regular.removal <- Environmental.Health.Solid.waste.removal..Regularly.with.suf_104
food.meat.wfp.assistance <- Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202
food.dairy.wfp.assistance <- Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245
edu.n.attending.school <- Children.Attending.school.Number.of.children.youth.attending_308
edu.public.school <- Currently..how.many.of.your.children.do.the.following.Public_314
vaccination.measles.NA <- Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398
entry.multiple.entries <- Entry.into.Jordan.Multiple.entries..Yes_436
work.documentation <- Documentation.Working.Family.Member.Yes_494
enumerator.judgement.notvulnerable <- Categorization.of.Vulnerabilities.Based.on.your.experience.w_630

################################################################################################
# Welfare Models:

# Welfare Model (full): HV & PG variables:
reg.full <- lm(ln.exppc~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+house.kitchen.d+rel_sunni+bir_syria+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+vaccination.measles.NA+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5, data=UNHCR)
# for WB full
reg.full <- lm(ln.exppc~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8.11+case.size.vaf.12plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+sp.co.cash+mar_single+mar_widow+mar_g_divorced+house.kitchen.d+rel_sunni+bir_syria+age.PA2+age.PA3+arr_legal+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+vaccination.measles.NA+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5, data=UNHCR)

# Welfare Model (reduced): HV & PG most important variables: 
reg.reduced <- lm(ln.exppc~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+mar_single+mar_g_married+mar_g_divorced+arr_illegal+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq, data=UNHCR)
# for WB reduced
reg.reduced <- lm(ln.exppc~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8.11+case.size.vaf.12plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+mar_single+mar_widow+mar_g_divorced+arr_legal+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq, data=UNHCR)

# Welfare Model: PG variables only:
reg.pg <- lm(ln.exppc.pg~arr_illegal+dem_PA_grp2+dem_PA_grp3+p.child.grp2+p.child.grp3+p.child.grp4+mar.grp1+mar.grp2+mar.grp3+rel_sunni+bir_syria+gender.male+edu.highest.grp2+edu.highest.grp3+edu.highest.grp4+edu.highest.grp5+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5+ajloun.pg+amman.pg+aqabah.pg+balqa.pg+jarash.pg+karak.pg+irbid.pg+maan.pg+mafraq.pg+tafilah.pg+zarqa.pg+case.size.pg.2+case.size.pg.3+case.size.pg.4+case.size.pg.5+case.size.pg.6+case.size.pg.7+case.size.pg.8plus)

# Probit Model:
Poor.ln <- ifelse(ln.exppc <= 3.912023, 1, 0)
Y <- cbind(Poor.ln)
reg.probit <- glm(Y~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+house.kitchen.d+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.wastewater.sewage+wash.wastewater.regular.removal+food.dairy.wfp.assistance+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5, family=binomial (link="probit"), data=UNHCR)
summary(reg.probit)
probit0 <- update(reg.probit, formula = Y ~ 1)
mcfadden <- 1-as.vector(logLik(reg.probit)/logLik(probit0))
mcfadden
table(true = Y, pred = round(fitted(reg.probit)))  
# for WB probit:
reg.probit <- glm(Y~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8.11+case.size.vaf.12plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+sp.co.cash+mar_single+mar_widow+mar_g_divorced+house.kitchen.d+age.PA2+age.PA3+arr_legal+wash.wastewater.sewage+wash.wastewater.regular.removal+food.dairy.wfp.assistance+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5, family=binomial (link="probit"), data=UNHCR)

prbit.frame <- as.data.frame(Poor.ln)
prbit.frame <- cbind(prbit.frame, case.size.vaf.2)
prbit.frame <- cbind(prbit.frame, case.size.vaf.3)
prbit.frame <- cbind(prbit.frame, case.size.vaf.4)
prbit.frame <- cbind(prbit.frame, case.size.vaf.5)
prbit.frame <- cbind(prbit.frame, case.size.vaf.6)
prbit.frame <- cbind(prbit.frame, case.size.vaf.7)
prbit.frame <- cbind(prbit.frame, case.size.vaf.8plus) 
prbit.frame <- cbind(prbit.frame, p.child.grp2)
prbit.frame <- cbind(prbit.frame, p.child.grp3)
prbit.frame <- cbind(prbit.frame, p.child.grp4)
prbit.frame <- cbind(prbit.frame, hh.crowding)
prbit.frame <- cbind(prbit.frame, edu.highest.grp5)
prbit.frame <- cbind(prbit.frame, edu.highest.grp4)
prbit.frame <- cbind(prbit.frame, edu.highest.grp3)
prbit.frame <- cbind(prbit.frame, edu.highest.grp2)
prbit.frame <- cbind(prbit.frame, occup.grp2)
prbit.frame <- cbind(prbit.frame, occup.grp3)
prbit.frame <- cbind(prbit.frame, occup.grp4)
prbit.frame <- cbind(prbit.frame, occup.grp5)
prbit.frame <- cbind(prbit.frame, gender.male)
prbit.frame <- cbind(prbit.frame, sp.co.cash)
prbit.frame <- cbind(prbit.frame, mar_single)
prbit.frame <- cbind(prbit.frame, mar_g_married)
prbit.frame <- cbind(prbit.frame, mar_g_divorced)
prbit.frame <- cbind(prbit.frame, house.kitchen.d)
prbit.frame <- cbind(prbit.frame, dem_PA_grp1)
prbit.frame <- cbind(prbit.frame, dem_PA_grp2)
prbit.frame <- cbind(prbit.frame, arr_illegal)
prbit.frame <- cbind(prbit.frame, wash.wastewater.sewage)
prbit.frame <- cbind(prbit.frame, wash.wastewater.regular.removal)
prbit.frame <- cbind(prbit.frame, food.dairy.wfp.assistance)
prbit.frame <- cbind(prbit.frame, entry.multiple.entries)
prbit.frame <- cbind(prbit.frame, moi.ajloun)
prbit.frame <- cbind(prbit.frame, moi.aqabah)
prbit.frame <- cbind(prbit.frame, moi.balqa)
prbit.frame <- cbind(prbit.frame, moi.irbid)
prbit.frame <- cbind(prbit.frame, moi.jerash)
prbit.frame <- cbind(prbit.frame, moi.karak)
prbit.frame <- cbind(prbit.frame, moi.maan)
prbit.frame <- cbind(prbit.frame, moi.madaba)
prbit.frame <- cbind(prbit.frame, moi.mafraq)
prbit.frame <- cbind(prbit.frame, moi.tafilah)
prbit.frame <- cbind(prbit.frame, moi.zarqa)
prbit.frame <- cbind(prbit.frame, work.documentation)
prbit.frame <- cbind(prbit.frame, enumerator.judgement.notvulnerable)
prbit.frame <- cbind(prbit.frame, all.case.size)
prbit.frame <- cbind(prbit.frame, all.case.size.sq)
prbit.frame <- cbind(prbit.frame, arr.crosspoint.grp2)
prbit.frame <- cbind(prbit.frame, arr.crosspoint.grp3)
prbit.frame <- cbind(prbit.frame, arr.crosspoint.grp4)
prbit.frame <- cbind(prbit.frame, arr.crosspoint.grp5)

newdata <- prbit.frame[complete.cases(prbit.frame),] 
attach(newdata)
reg.probit <- glm(Poor.ln~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+house.kitchen.d+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.wastewater.sewage+wash.wastewater.regular.removal+food.dairy.wfp.assistance+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp2+arr.crosspoint.grp3+arr.crosspoint.grp4+arr.crosspoint.grp5, family=binomial (link="probit"), data=newdata)
table(true = Poor.ln, pred = round(fitted(reg.probit)))  

write.csv(newdata, file="newdata.csv")

# Output Results In Separate Dataframe Table:
reg.full.frame <- as.data.frame(summary(reg.full)$coefficients[, 1:4])
reg.reduced.frame <- as.data.frame(summary(reg.reduced)$coefficients[, 1:4])
reg.pg.frame <- as.data.frame(summary(reg.pg)$coefficients[, 1:4])
reg.probit.frame <- as.data.frame(summary(reg.probit)$coefficients[, 1:4])

# Save Output Results as a CSV file:
write.csv(reg.full.frame, file="reg.full.frame.csv")
write.csv(reg.reduced.frame, file="reg.reduced.frame.csv")
write.csv(reg.pg.frame, file="reg.pg.frame.csv")
write.csv(reg.probit.frame, file="reg.probit.frame.csv")

################################################################################################
# Outlier Calculation and Exclusion:
outlier.data <- UNHCR[-c(9403, 25063, 2400, 21568, 18233, 12962, 25756, 20984, 22500, 1759)]
reg.reduced <- lm(ln.exppc~case.size.vaf.2+case.size.vaf.3+case.size.vaf.4+case.size.vaf.5+case.size.vaf.6+case.size.vaf.7+case.size.vaf.8plus+p.child.grp2+p.child.grp3+p.child.grp4+hh.crowding+occup.grp2+occup.grp3+occup.grp4+occup.grp5+gender.male+mar_single+mar_g_married+mar_g_divorced+arr_illegal+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq, data=outlier.data)

################################################################################################
# Welfare Model Predictions:

ln.predictions <- predict(reg.reduced) # create predictions object
# predictions.2 <- fitted(reg.reduced) # Option 2: fitted values are the same as predicted values
## the two functions above produce the same estimates

predictions.data.frame <- as.data.frame(ln.predictions) # simple name, must create predictions object first
# predictions.data.frame <- as.data.frame(predict(reg.reduced)) # Option 2: doesnt require object creation first

UNHCR <- cbind(UNHCR, hh.crowding)
UNHCR <- cbind(UNHCR, enumerator.judgement.notvulnerable)
UNHCR <- cbind(UNHCR, work.documentation)
UNHCR <- cbind(UNHCR, gender.male)
UNHCR <- cbind(UNHCR, mar_widow)
UNHCR <- cbind(UNHCR, mar_single)
UNHCR <- cbind(UNHCR, mar_g_divorced)
UNHCR <- cbind(UNHCR, mar_g_married)
UNHCR <- cbind(UNHCR, arr_illegal)
UNHCR <- cbind(UNHCR, all.case.size)
# For Excel export, first bind indicators in UNHCR, then merge UNHCR with Predictions, otherwise wont work as there is 1 observation missing

MERGE <- merge(UNHCR, predictions.data.frame, by.x = "row.names", by.y = "row.names") # single data frame now             
# UNHCR <- cbind(UNHCR, ln.predictions) # doesn't work because of different length, so instead we use "merge"

level.predictions <- exp(ln.predictions)
# N.B. exp(1) represents e and exp(2) represents e^2, exp is the exponentiation function with base e

MERGE <- cbind(MERGE, level.predictions)

################################################################################################
# Poverty Thresholds:

# Option 1 (necessary for calculations later): Poverty Thresholds: Severe < 28 ; High < 68; Moderate < 100;  Low > 100
poverty.severe.cases <- ifelse(level.predictions < 28, 1, 0)
poverty.high.cases <- ifelse((level.predictions >= 28) & (level.predictions < 68), 1, 0)
poverty.moderate.cases <- ifelse((level.predictions >= 68) & (level.predictions < 100), 1, 0)
poverty.low.cases <- ifelse(level.predictions >= 100, 1, 0)
# sum(mean(poverty.severe.cases), mean(poverty.high.cases), mean(poverty.moderate.cases), mean(poverty.low.cases)) # Equals 1, so calculations correct

# Option 2: Poverty Thresholds: Severe < 28 ; High < 68; Moderate < 100;  Low > 100
library(classInt) # used for 'findCols' function
library(plyr) # used for 'revalue' function
poverty.thresholds.cases.numeric <- as.factor(findCols(classIntervals(level.predictions, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
poverty.thresholds.cases.class <- revalue(poverty.thresholds.cases.numeric, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
poverty.thresholds.cases  <- factor(poverty.thresholds.cases.class, levels = c("Severe", "High", "Moderate", "Low"))

################################################################################################
# Data export in CSV with predictions:

MERGE <- cbind(MERGE, poverty.thresholds.cases)

Welfare.Model.Analysis <- MERGE[ , c(
  "Household.information.address.._3" ,                                                                                                                                                                                                                                                     
  "CaseID" ,
  "PGCID" ,
  "ln.predictions",
  "level.predictions" ,
  "Household.information.Family.Size_5" ,
  "poverty.thresholds.cases" ,
  "percentage_0_14" ,
  "dem_marriage" ,
  "Categorization.of.Vulnerabilities.Based.on.your.experience.w_630" ,
  "Documentation.Working.Family.Member.Yes_494" ,
  "Type.of.Housing.Number.of.family.members.in.the.house..both._9" , 
  "arr_illegal" ,
  "gender.male" ,
  "hh.crowding" ,
  "Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10" ,
  "Payment.Type.of.occupancy..For.rent_52" ,
  "Payment.Type.of.occupancy..Shelter.provided.through.humanita_53" ,
  "Payment.Type.of.occupancy..Hosted..for.free._54" ,
  "Payment.Type.of.occupancy..Shelter.provided.in.return.for.wo_55" ,
  "Payment.Type.of.occupancy..Squatter..illegal.occupation.of.s_56" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Ajlo_463" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Aqab_464" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Balq_465" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Jera_467" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Kara_468" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Maa_469" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Mada_470" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Mafr_471" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Tafi_472" ,
  "MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Zarq_473" ,
  "mar_widow" ,
  "mar_single" ,
  "mar_g_divorced" ,
  "mar_g_married"
)]
# In Welfare.Model.Analysis ln.predictions variable doesn't match with level.prediction variable; level.predictions 
# appear to be correct, but ln.predictions don't. Check head(ln.predictions) and head(level.predictions), the values 
# listed will be different than the values listed in Welfare.Model.Analysis dataframe; the head values appear to be correct

write.csv(Welfare.Model.Analysis, file="Welfare.Model.Analysis.csv")

################################################################################################
# Calculations of Number of Cases and People in Different Poverty Thresholds:

# Percentage of Cases in each Poverty Threshold (wrt sample):
table(poverty.thresholds.cases)/nrow(MERGE)

# Percentage of Cases Below the Poverty Line (Severe + High Vulnerability):
sum(mean(poverty.severe.cases), mean(poverty.high.cases))

# Number of Cases Below the Poverty Line (Severe + High Vulnerability):
sum(poverty.severe.cases, poverty.high.cases)

# Percentage of People in each Poverty Threshold (wrt sample):
population.severe <- case.size.vaf[poverty.thresholds.cases == "Severe"]
population.high <- case.size.vaf[poverty.thresholds.cases == "High"]
population.moderate <- case.size.vaf[poverty.thresholds.cases == "Moderate"]
population.low <- case.size.vaf[poverty.thresholds.cases == "Low"]
population.object <- c(sum(population.severe), sum(population.high), sum(population.moderate), sum(population.low))
total.population <- sum(case.size.vaf)
(population.object)/(total.population)

# Percentage of People Below the Poverty Line (Severe + High Vulnerability):
sum(sum(population.severe), sum(population.high))/(total.population)

# Number of People Below the Poverty Line (Severe + High Vulnerability):
sum(sum(population.severe), sum(population.high))

## Histograms:
poverty.thresholds.numeric <- as.numeric(poverty.thresholds.numeric)
hist(poverty.thresholds.numeric,col="red", freq=T,xlim=c(1,4)) # Option 1
hist(poverty.thresholds.numeric) # Option 2

## Testing that prediction function works:
row.one <- UNHCR[1,]
write.csv(row.one, file="row.one.csv")
# then calculate Welfare score in Excel

min.model = lm(ln.exppc ~ 1, data=MERGE)
biggest <- formula(lm(ln.exppc~., UNHCR))
biggest
fwd.model = step(min.model, direction='both', scope=(~ case.size.vaf + gender.male + mar.grp2))

confint(reg.reduced)
predict(reg.reduced, list(case.size.vaf=10, mar.grp2=0, gender.male=1))
paris(data.frame)

################################################################################################
## Extra: replication of statistics in World Bank paper:
UNHCR <- read.csv("data/One_Big_list_vaf_with_progres_ANSI_UNIX.txt", na.strings = "")
attach(UNHCR)

UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 == 0),] # lose 4 obs.
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 >= 20),] # lose 6 obs.

# Figure 1:
EPC <- (UNHCR$Financial.Situation.Total.Expenditure_124/UNHCR$Household.information.Family.Size_5)
YPC <- (UNHCR$Financial.Situation.Total.income.._131/UNHCR$Household.information.Family.Size_5)
length(which(EPC == 0)) # 2046
length(which(YPC == 0)) # 17706
summary(EPC) # NA's = 1
summary(YPC) # NA's = 2

UNHCR <- cbind(UNHCR, EPC)
UNHCR <- cbind(UNHCR, YPC)

UNHCR <- UNHCR [!(EPC == 0),]
UNHCR <- UNHCR [!(YPC == 0),]

LN.EPC <- log(EPC)
sd(EPC, na.rm=TRUE) # 72.12162
sd(YPC, na.rm=TRUE) # 96.39654

UNHCR <- UNHCR[!(EPC == 0),]

length(which(exp.pc.2<50)) # 13383
length(which(exp.pc.2>=50)) # 10782
# total = 24165

poor.predicted <- ifelse(level.predictions < 50, 1, 0)
poor.observed <- ifelse(exp.pc.2 < 50, 1, 0)
table(poor.predicted) # 0 = 9955; 1 = 14209; Total = 24164
table (poor.observed) # 0 = 10782; 1 = 13383; Total = 24165

predictions.data.frame <- cbind(predictions.data.frame, poor.predicted)
UNHCR <- cbind(UNHCR, poor.observed)
UNHCR <- cbind(UNHCR, exp.pc.2)
MERGE <- merge(UNHCR, predictions.data.frame, by.x = "row.names", by.y = "row.names") # single data frame now             
Predictions.Reduced <- MERGE [ , c(                                                                                                                                                                                                                                                     
    "CaseID" ,
    "exp.pc.2" ,
    "poor.observed" ,
    "level.predictions" ,
    "poor.predicted"
)]
write.csv(Predictions.Reduced, file="Predictions.Reduced")

rel_sunni <- na.omit(rel_sunni)
rel_sunni <- rel_sunni[!is.na(rel_sunni)]
mean(exp.pc.2[rel_sunni==0]) # doesnt work, not sure why
by(exp.pc.2, rel_sunni, mean) # works
mean(rel_sunni, na.rm = TRUE)

mean(exp.pc.2[gender.male==0])
mean(exp.pc.2[gender.male==1])
by(exp.pc.2, gender.male, mean) # Best, provides both options 
by(exp.pc.2, ethn_arab, mean) 
by(exp.pc.2, edu.highest.grp5, mean) 
by(exp.pc.2, amman, mean) 
by(exp.pc.2, arr_legal, mean) 
mean(exp.pc.2[edu.highest.grp5==1])

length(which(ethn_arab==1))/nrow(UNHCR)

e <- ifelse((ethn_arab == 1) & (exp.pc.2 < 50), 1, 0)
e <- ifelse((ethn_arab == 0) & (exp.pc.2 < 50), 1, 0)

std.error(VAR) #automatic using plotrix package
se1 <- function(x) sd(x)/sqrt(length(x)) #manual option 1
se2 <- function(x) sqrt(var(x)/length(x)) #manual option 2

std.error(case.size.vaf) #automatic
se.j <- sd(case.size.vaf)/sqrt(length(case.size.vaf)) #manual option 1
se.j2 <- sqrt(var(Household.information.Family.Size)/length(Household.information.Family.Size)) #manual option 2
# error check: same results YES.
mu.j <- mean(case.size.vaf)

se.i <- sd(case.size.pg)/sqrt(length(case.size.pg)) #manual option 1
mu.i <- mean(case.size.pg)

p <- nrow(UNHCR)/nrow(UNHCR)

md <- abs(mu.i - mu.j)
den <- (sqrt(se.i)^2+(1-2*p)*(se.j)^2)
ttest1 <- md/den #option 1
ttest2 <- abs(mu.i - mu.j)/(sqrt(se.i)^2+(1-2*p)*(se.j)^2) #option 2
# error check: same results YES; t = 136.72



#######################################################################
# For PG Model:
ln.predictions.pg <- predict(reg.pg)
predictions.data.frame <- as.data.frame(ln.predictions.pg)
level.predictions <- exp(ln.predictions.pg)
predictions.data.frame <- cbind(predictions.data.frame, level.predictions)

library(classInt) # used for 'findCols' function
library(plyr) # used for 'revalue' function
poverty.thresholds.cases.numeric <- as.factor(findCols(classIntervals(level.predictions, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
poverty.thresholds.cases.class <- revalue(poverty.thresholds.cases.numeric, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
poverty.thresholds.cases  <- factor(poverty.thresholds.cases.class, levels = c("Severe", "High", "Moderate", "Low"))
predictions.data.frame <- cbind(predictions.data.frame, poverty.thresholds.cases)

MERGE <- merge(UNHCR, predictions.data.frame, by.x = "row.names", by.y = "row.names")

Welfare.Model.Analysis.PG <- MERGE[ , c(
  "Household.information.address.._3" ,                                                                                                                                                                                                                                                     
  "CaseID" ,
  "PGCID" ,
  "ln.predictions.pg",
  "level.predictions" ,
  "Household.information.Family.Size_5" ,
  "poverty.thresholds.cases"
)]

write.csv(Welfare.Model.Analysis.PG, file="Welfare.Model.Analysis.PG.csv")

#######################################################################
# Poverty Thresholds for World Bank Study (50 JD per month):
# Thresholds Poor and Non-Poor:
poverty.poor.cases <- ifelse(level.predictions < 50, 1, 0)
poverty.notpoor.cases <- ifelse(level.predictions >= 50, 1, 0)

# Percentage of Cases in each Poverty Threshold (wrt sample):
table(poverty.poor.cases)/nrow(MERGE) 

# Percentage of People in each Poverty Threshold (wrt sample):
population.poor <- case.size.vaf[poverty.poor.cases == 1]
population.notpoor <- case.size.vaf[poverty.notpoor.cases == 1]
population.object <- c(sum(population.poor), sum(population.notpoor))
total.population <- sum(case.size.vaf)
(population.object)/(total.population) 


