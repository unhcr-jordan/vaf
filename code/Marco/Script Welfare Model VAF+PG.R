## ==========================================

# Welfare Model (VAF + ProGres)

# Author: Marco Santacroce
# Date started: 26 March 2015
# Date finised: 30 March 2015

## ==========================================
# The Data
## ==========================================

# Data Merge:
#horizontal merge (columns)
Merged.Data <- merge(VAF, PG, by.x = "CaseID", by.y = "PGCID") #unexplained loss in records (only about 13,000), better to use SQL for data match, SQL script available with Shadi (DAG)
total <- merge(data frameA,data frameB,by="ID")
total <- merge(data frameA,data frameB,by=c("ID","Country")

#vertical merge (rows)              
total <- rbind(data frameA, data frameB)
               
# Dataset Used:
UNHCR <- read.csv("data/One_Big_list_vaf_with_progres_ANSI_UNIX.txt", na.strings = "")
attach(UNHCR)

# Data Explanation:
# ProGres dataset = almost 150 columns (data provided directly by World Bank with renamed column headings)
# VAF data = about 600 columns (text columns excluded prior to merge)
# Number of observations = 26,290

# Data Exploration:
mean(csize_act) # 4.133245
mean(csize_decl) # NA
mean(Household.information.Family.Size_5) # 90201.97
mean(Num_Inds) # 4.129441

bind ? might have to
class ? no need when you generate variables with ifelse, transforms factor to numeric automatically

## ==========================================
# Data Cleaning:
## ==========================================

UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 == 0),] # lose 4 obs.
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 >= 20),] # lose 6 obs.
# now HV case size and PG case size almost identical: mean(PGsize) = 4.134, mean(HVsize) = 4.127

UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 == 0),] # lose 44 obs.
UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 >= 10),] # lose 24 obs.

# Option which we won't use as total area not a key predictor:
# Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_11[is.na(UNHCR$Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_11)] <- 0
# UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_11 = 0),]
# UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_11 < 1),]

UNHCR[is.na(UNHCR)]<-0
attach(UNHCR)

# Expenditure per capita:
exp.pc.1 <- (Financial.Situation.Total.Expenditure_124/csize_act)
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5)
UNHCR <- cbind(UNHCR, exp.pc.2)
UNHCR <- UNHCR [!(exp.pc.2 == 0),] # lose about 2,000 records
UNHCR <- UNHCR [!(exp.pc.2 > 1000),] # lose 3 records
attach(UNHCR)
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5) # will correct for excluded rows, no longer need UNHCR$ to use variable

# Log expenditure per capita:
ln.exppc <- log(exp.pc.2)

## ==========================================
# Variable Generation:
## ==========================================

# ProGres Variables:

# Legality of arrival:
arr_legal <- ifelse(UNHCR$arr_status == "Legal" | UNHCR$arr_status ==  "Legal - Medical Visa", 1, 0)
arr_illegal <- ifelse(UNHCR$arr_status == "Informal" | UNHCR$arr_status ==  "Smuggled", 1, 0)

# Age group:
dem_age_grp1 <- ifelse(UNHCR$dem_age < 35, 1, 0)
dem_age_grp2 <- ifelse((UNHCR$dem_age >= 35) & (UNHCR$dem_age < 55), 1, 0)
dem_age_grp3 <- ifelse(UNHCR$dem_age >= 55, 1, 0)

dem_PA_grp0 <- ifelse(UNHCR$dem_age < 15, 1, 0)
dem_PA_grp1 <- ifelse(UNHCR$dem_age < 18, 1, 0)
dem_PA_grp2 <- ifelse((UNHCR$dem_age > 17) & (UNHCR$dem_age < 60), 1, 0)
dem_PA_grp3 <- ifelse(UNHCR$dem_age > 59, 1, 0)

# Marital status:
mar_widow <- ifelse(dem_marriage == "WD Widowed", 1, 0) # niente
mar_single <- ifelse(dem_marriage == "SN Single", 1, 0) # significativa e buon R2
mar_divorced <- ifelse(dem_marriage == "DV Divorced", 1, 0) # significativa ma poco R2
mar_married <- ifelse(dem_marriage == "MA Married", 1, 0) # significative e OK R2
mar_engaged <- ifelse(dem_marriage == "EG Engaged", 1, 0) # significativa ma poco R2
mar_g_divorced <- ifelse((dem_marriage == "DV Divorced" | dem_marriage == "SR Separated"), 1, 0) # significativa ma poco R2
mar_g_married <- ifelse((dem_marriage == "MA Married" | dem_marriage == "CL Common Law Married" | dem_marriage == "EG Engaged"), 1, 0) # significativa e OK R2 ma meno rispetto a married-only

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

### do missed education crosstabulation


# VAF Variables:
# Case Size:
case.size.pg <- (csize_act)
case.size.vaf <- (Household.information.Family.Size_5)
case.size.vaf.sq <- (Household.information.Family.Size_5)^2

# Household Size:
hh.size <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9)

# House crowding:
house.crowding.1 <- (csize_act/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)
house.crowding.2 <- (Household.information.Family.Size_5/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)
hh.crowding <- (Type.of.Housing.Number.of.family.members.in.the.house..both._9/Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10)

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

# Area/person < 3.5m²
house.less <- (Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_12)
house.less[is.na(house.less)] <- 0

# Area/person = 3.5m²
house.equal <- (Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_13)
house.equal[is.na(house.equal)] <- 0

# Area/person > 3.5m²
house.more <- (Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_14)
house.more[is.na(house.more)] <- 0

# Kitchen dummy (access to), significant good R2 in lnexp
house.kitchen.d <- (Type.of.Housing.Access.to.kitchen..Yes_15)
house.kitchen.d[is.na(house.kitchen.d)] <- 0

# Sanitary dummy (access to), significant good R2 in lnexp
house.sanitary.d <- (Type.of.Housing.Access.to.sanitary.facilities..Yes_18)
house.sanitary.d[is.na(house.sanitary.d)] <- 0

# Ventilation dummy (access to), no
house.ventilation.d <- (Type.of.Housing.Ventilation..Yes_21)
house.ventilation.d[is.na(house.ventilation.d)] <- 0

# Electricity dummy (access to), significant
house.electricity.d <- (Type.of.Housing.Access.to.electricity..Yes_25)
house.electricity.d[is.na(house.electricity.d)] <- 0

house.interaction <- (house.kitchen.d*house.sanitary.d*house.electricity.d)
house.composite <- (house.kitchen.d + house.sanitary.d + house.ventilation.d + house.electricity.d)

# Square meter area:
house.sq.area <- (Type.of.Housing.Total.area.excluding.the.kitchen...WASH.faci_11)

# Concrete house:
concrete.house <- ifelse(Type.of.Housing.Type.of.Housing..Based.on.the.volunteer_s.ob_6 == 1, 1, 0)

# Water sources:
water.piped <- (Water.What.are.your.most.important.sources.of.water.in.your._71)
water.shop.private <- (Water.What.are.your.most.important.sources.of.water.in.your._76 + Water.What.are.your.most.important.sources.of.water.in.your._77)

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

# Variables for welfare model: 
wash.improved.latrine <- Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95
wash.wastewater.sewage <- Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98
wash.wastewater.regular.removal <- Environmental.Health.Solid.waste.removal..Regularly.with.suf_104
food.meat.wfp.assistance <- Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202
food.pulses.wfp.assistance <- Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232
food.dairy.wfp.assistance <- Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245
edu.n.attending.school <- Children.Attending.school.Number.of.children.youth.attending_308
edu.public.school <- Currently..how.many.of.your.children.do.the.following.Public_314

edu.missed.none <- How.many.of.your.children..between.6.17.years..have.missed.e_345
edu.missed.0to1year <- How.many.of.your.children..between.6.17.years..have.missed.e_346
edu.missed.1to3.years <- How.many.of.your.children..between.6.17.years..have.missed.e_347
edu.missed.morethan3years <- How.many.of.your.children..between.6.17.years..have.missed.e_348

edu.interest.recreational.activities <- Children.Not.Attending.School.Which.types.of.informal.educat_354
lactating.women.problem.NA <- Access.to.Health.Services.If.there.are.any.lactating.women.._396
vaccination.measles.NA <- Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398
separated.family.member.mother <- If.no..specify.family.members.that.separated.and.their.locat_424
separated.family.member.son <- If.no..specify.family.members.that.separated.and.their.locat_425
separated.family.member.daughter <- If.no..specify.family.members.that.separated.and.their.locat_426
entry.multiple.entries <- Entry.into.Jordan.Multiple.entries..Yes_436
work.documentation <- Documentation.Working.Family.Member.Yes_494
work.agriculture <- Working.Family.Member.What.kind.of.job..Agriculture_496
specific.needs.present <- Explanation.of.Specific.Needs.Any.of.the.family.member.have._563
specific.needs.observed.childatrisk <- Specific.Needs.Specific.Need..observed....Child.at.risk_564
specific.needs.observed.singleparent <- Specific.Needs.Specific.Need..observed....Single.parent.or.c_567
enumerator.judgement.notvulnerable <- Categorization.of.Vulnerabilities.Based.on.your.experience.w_630

prop.child <- (Child_0_14/case.size.vaf)

# Arrival crosspoint:
arr.crosspoint.grp1 <- ifelse((arr_crosspoint == "C - Airport Queeen Alia - Aleppo" | arr_crosspoint == "C - Airport Queeen Alia - Damascus" | arr_crosspoint == "C - Airport Queeen Alia - Third countries"), 1, 0)
arr.crosspoint.grp2 <- ifelse((arr_crosspoint == "E - Ruwaished - Hadallat"), 1, 0)
arr.crosspoint.grp3 <- ifelse((arr_crosspoint == "W - Jaber (Nassib) - Official" | arr_crosspoint == "W - Jaber (Nassib) unofficial"), 1, 0)

# Difference HH size to Case size:
diff.family.size <- (all.case.size - case.size.vaf)
prop.family.size <- (all.case.size/case.size.vaf)

# Interaction terms:
child.t.case <- (Child_0_14*case.size.vaf)
prop.t.case <- (percentage_0_14*case.size.vaf)
child.t.prop <- (Child_0_14*percentage_0_14)


## ==========================================
# Bivariate Analysis:
## ==========================================

# double vs. single peak
r <- lm(exp.pc.2~Child_0_14) # 0.06498 # 0.1985
r <- lm(exp.pc.2~Child_0_17) # 0.06868 # 0.2123
r <- lm(exp.pc.2~Child_0_18) # 0.06908 # 0.214
r <- lm(exp.pc.2~percentage_0_14) # 0.06597 # 0.2018
r <- lm(exp.pc.2~percentage_0_17) # 0.07052 # 0.2208
r <- lm(exp.pc.2~percentage_0_18) # 0.06726 # 0.2108
r <- lm(exp.pc.2~AVG_Age) # 0.01995 # 0.08434
r <- lm(exp.pc.2~Median_Age) # 0.02658 # 0.1043
r <- lm(exp.pc.2~case.size.vaf) # 0.07164 # 0.2386
r <- lm(exp.pc.2~house.crowding.2) # -  # 0.1474
r <- lm(exp.pc.2~hh.crowding) # -  # 0.07684
r <- lm(exp.pc.2~house.sq.area) # -  # 0.0001417 & not significant

# single peak
r <- lm(ln.exppc~Child_0_14) # 0.2922
r <- lm(ln.exppc~Child_0_17) # 0.3114
r <- lm(ln.exppc~Child_0_18) # 0.3129
r <- lm(ln.exppc~percentage_0_14) # 0.2478
r <- lm(ln.exppc~percentage_0_17) # 0.2661
r <- lm(ln.exppc~percentage_0_18) # 0.2536
r <- lm(ln.exppc~AVG_Age) # 0.1059
r <- lm(ln.exppc~Median_Age) # 0.1314
r <- lm(ln.exppc~case.size.vaf) # 0.3274
r <- lm(ln.exppc~house.crowding.2) # 0.2349 
r <- lm(ln.exppc~hh.crowding) # 0.1471
r <- lm(ln.exppc~house.sq.area) #0.00073 & not significant
r <- lm(ln.exppc~child.t.case) #0.2541
r <- lm(ln.exppc~prop.t.case) #0.2976
r <- lm(ln.exppc~child.t.prop) #0.2466

summary(r)

reg <- lm(ln.exppc~case.size.vaf+percentage_0_14+hh.crowding+AVG_Age) # 0.3885
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age) # 0.3941

## ==========================================
# Regressions Manual:
## ==========================================

r <- lm(exp.pc.2~hh.crowding)
r <- lm(exp.pc.2~house.crowding.2)

summary(r)

reg <- lm(exp.pc.2~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+house.crowding.2) # 7%
reg <- lm(exp.pc.2~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+house.crowding.2+case.size.vaf) # 10% ma HC perde potere significativo
reg <- lm(exp.pc.2~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf) # 11% entrambi HC mantiene potere esplicativo
reg <- lm(exp.pc.2~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+case.size.vaf.sq) # 12%
reg <- lm(exp.pc.2~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+case.size.vaf.sq+mar_widow+mar_single+mar_g_married) # 12.5% seems like divorcees do best

summary(reg)

reg <- lm(ln.exppc~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+mar_widow+mar_single+mar_g_married) # 45% without FS2 (divorcees ref cat)
reg <- lm(ln.exppc~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+case.size.vaf.sq+mar_widow+mar_single+mar_g_married) # 47% with FS2 (divorced ref cat)
reg <- lm(ln.exppc~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+case.size.vaf.sq+mar_widow+mar_single+mar_g_married+AVG_Age) # 47% with FS2 (divorced ref cat) Median_Age same as AVG_Age
reg <- lm(ln.exppc~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+hh.crowding+case.size.vaf+case.size.vaf.sq+mar_widow+mar_single+mar_g_married+AVG_Age+percentage_0_17) # 47% with FS2 (divorced ref cat) Median_Age same as AVG_Age

reg <- lm(ln.exppc~edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+house.crowding.2+case.size.vaf+case.size.vaf.sq+mar_widow+mar_single+mar_g_married) # 45% with FS2, case-crowding (divorced ref cat)

summary(reg)

reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+mar_widow+mar_single+mar_g_married) # 0.4767
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+mar_widow+mar_single+mar_g_married+concrete.house+house.kitchen.d+house.sanitary.d) #  0.4768
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+occup.grp1+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria) # 0.4786
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal) # 0.4901
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98) # 0.4913
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98+Environmental.Health.Solid.waste.removal..Regularly.with.suf_104+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245+Children.Attending.school.Number.of.children.youth.attending_308+Currently..how.many.of.your.children.do.the.following.Public_314+How.many.of.your.children..between.6.17.years..have.missed.e_347+Children.Not.Attending.School.Which.types.of.informal.educat_354+Access.to.Health.Services.If.there.are.any.lactating.women.._396+Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398+If.no..specify.family.members.that.separated.and.their.locat_424+If.no..specify.family.members.that.separated.and.their.locat_425+If.no..specify.family.members.that.separated.and.their.locat_426+Entry.into.Jordan.Multiple.entries..Yes_436) # 0.4991
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98+Environmental.Health.Solid.waste.removal..Regularly.with.suf_104+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245+Children.Attending.school.Number.of.children.youth.attending_308+Currently..how.many.of.your.children.do.the.following.Public_314+How.many.of.your.children..between.6.17.years..have.missed.e_347+Children.Not.Attending.School.Which.types.of.informal.educat_354+Access.to.Health.Services.If.there.are.any.lactating.women.._396+Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398+If.no..specify.family.members.that.separated.and.their.locat_424+If.no..specify.family.members.that.separated.and.their.locat_425+If.no..specify.family.members.that.separated.and.their.locat_426+Entry.into.Jordan.Multiple.entries..Yes_436+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466+Documentation.Working.Family.Member.Yes_494+Working.Family.Member.What.kind.of.job..Agriculture_496+Explanation.of.Specific.Needs.Any.of.the.family.member.have._563+Specific.Needs.Specific.Need..observed....Child.at.risk_564+Specific.Needs.Specific.Need..observed....Single.parent.or.c_567+Categorization.of.Vulnerabilities.Based.on.your.experience.w_630) # 0.5173

# with FS all case numbers 0.5385
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98+Environmental.Health.Solid.waste.removal..Regularly.with.suf_104+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245+Children.Attending.school.Number.of.children.youth.attending_308+Currently..how.many.of.your.children.do.the.following.Public_314+How.many.of.your.children..between.6.17.years..have.missed.e_347+Children.Not.Attending.School.Which.types.of.informal.educat_354+Access.to.Health.Services.If.there.are.any.lactating.women.._396+Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398+If.no..specify.family.members.that.separated.and.their.locat_424+If.no..specify.family.members.that.separated.and.their.locat_425+If.no..specify.family.members.that.separated.and.their.locat_426+Entry.into.Jordan.Multiple.entries..Yes_436+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466+Documentation.Working.Family.Member.Yes_494+Working.Family.Member.What.kind.of.job..Agriculture_496+Explanation.of.Specific.Needs.Any.of.the.family.member.have._563+Specific.Needs.Specific.Need..observed....Child.at.risk_564+Specific.Needs.Specific.Need..observed....Single.parent.or.c_567+Categorization.of.Vulnerabilities.Based.on.your.experience.w_630+Type.of.Housing.Number.of.family.members.in.the.house..both._9+Type.of.Housing.If.Yes...Inside.the.house_19) # 0.5385
summary(reg)

# with FS all case numbers & how much paying for rent 0.5821
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98+Environmental.Health.Solid.waste.removal..Regularly.with.suf_104+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245+Children.Attending.school.Number.of.children.youth.attending_308+Currently..how.many.of.your.children.do.the.following.Public_314+How.many.of.your.children..between.6.17.years..have.missed.e_347+Children.Not.Attending.School.Which.types.of.informal.educat_354+Access.to.Health.Services.If.there.are.any.lactating.women.._396+Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398+If.no..specify.family.members.that.separated.and.their.locat_424+If.no..specify.family.members.that.separated.and.their.locat_425+If.no..specify.family.members.that.separated.and.their.locat_426+Entry.into.Jordan.Multiple.entries..Yes_436+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466+Documentation.Working.Family.Member.Yes_494+Working.Family.Member.What.kind.of.job..Agriculture_496+Explanation.of.Specific.Needs.Any.of.the.family.member.have._563+Specific.Needs.Specific.Need..observed....Child.at.risk_564+Specific.Needs.Specific.Need..observed....Single.parent.or.c_567+Categorization.of.Vulnerabilities.Based.on.your.experience.w_630+Type.of.Housing.Number.of.family.members.in.the.house..both._9+Type.of.Housing.If.Yes...Inside.the.house_19+Payment.How.much._62) # 0.5821

# with neither FS all case numbers or how much rent 0.5174
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+Wastewater.What.kind.of.latrine.toilet.facility.does.your.ho_95+Wastewater.Wastewater.collection.disposal..Network.sewage.sy_98+Environmental.Health.Solid.waste.removal..Regularly.with.suf_104+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_202+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_232+Over.the.last.7.days..how.many.days.did.you.consume.the.foll_245+Children.Attending.school.Number.of.children.youth.attending_308+Currently..how.many.of.your.children.do.the.following.Public_314+How.many.of.your.children..between.6.17.years..have.missed.e_347+Children.Not.Attending.School.Which.types.of.informal.educat_354+Access.to.Health.Services.If.there.are.any.lactating.women.._396+Vaccination.Do.you.have.a.child.under.5.years.who.was.not.im_398+If.no..specify.family.members.that.separated.and.their.locat_424+If.no..specify.family.members.that.separated.and.their.locat_425+If.no..specify.family.members.that.separated.and.their.locat_426+Entry.into.Jordan.Multiple.entries..Yes_436+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Amma_462+MOI.Service.Card.Governorate.recorded.on.the.MOI.card...Irbi_466+Documentation.Working.Family.Member.Yes_494+Working.Family.Member.What.kind.of.job..Agriculture_496+Explanation.of.Specific.Needs.Any.of.the.family.member.have._563+Specific.Needs.Specific.Need..observed....Child.at.risk_564+Specific.Needs.Specific.Need..observed....Single.parent.or.c_567+Categorization.of.Vulnerabilities.Based.on.your.experience.w_630) # 0.5173

# same regression as above, but with all MOI dummies (Amman ref.) 0.5227
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable) #0.5227
summary(reg)

# same regression as above, but all case FS rather than case size 0.5248:
reg <- lm(ln.exppc~all.case.size+all.case.size.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable)

# same regression as above, but with case size & all case family size 0.5425
reg <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size)


## Final regression models (1. with only case size; 2. with only all cases size; 3. with case size and all cases size)

# First must bind all covariates in order to drop from dataset:
UNHCR <- cbind(UNHCR, ln.exppc, case.size.vaf, case.size.vaf.sq, hh.crowding, edu.highest.grp5, edu.highest.grp4, edu.highest.grp3, edu.highest.grp2, occup.grp1, occup.grp2, occup.grp3, occup.grp4, gender.male, sp.co.cash, mar_single, mar_g_married, mar_g_divorced, concrete.house, house.kitchen.d, house.sanitary.d, ethn_arab, rel_sunni, bir_syria, dem_age_grp2, dem_age_grp3, arr_illegal, wash.improved.latrine, wash.wastewater.sewage, wash.wastewater.regular.removal, food.meat.wfp.assistance, food.pulses.wfp.assistance, food.dairy.wfp.assistance, edu.n.attending.school, edu.public.school, edu.missed.1to3.years, edu.missed.morethan3years, edu.interest.recreational.activities, lactating.women.problem.NA, vaccination.measles.NA, separated.family.member.mother, separated.family.member.son, separated.family.member.daughter, entry.multiple.entries, moi.ajloun, moi.aqabah, moi.balqa, moi.irbid, moi.jerash, moi.karak, moi.maan, moi.madaba, moi.mafraq, moi.tafilah, moi.zarqa, work.documentation, work.agriculture, specific.needs.present, specific.needs.observed.childatrisk, specific.needs.observed.singleparent, enumerator.judgement.notvulnerable, all.case.size, all.case.size.sq)

# 1. 0.5227 then 0.5401
reg1 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=UNHCR) 
outlier <- UNHCR[-c(8661, 478, 14843, 2202, 23035, 16776, 19829, 11931, 19290, 23671),]
reg1 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=outlier) 

# 2. 0.5248 then 0.525
reg2 <- lm(ln.exppc~all.case.size+all.case.size.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=UNHCR)
outlier <- UNHCR[-c(9403, 520, 12962, 25063, 21568, 2400, 20984, 25756, 8918, 9842),]
reg2 <- lm(ln.exppc~all.case.size+all.case.size.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=outlier)

# 3. 0.5425 0.5553 with sq; then 0.5428 0.5555 with sq
reg3 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq, data=UNHCR)
outlier <- UNHCR[-c(9403, 520, 25063, 21568, 2400, 18233, 12962, 20984, 25756, 22500),]
reg3 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp3+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq, data=outlier)

## Run models without statistically insignificant dummies:
# below is the model with reference categories (occup grp3 ref., edu grp5 ref., moi.amman ref.)
reg2 <- lm(ln.exppc~all.case.size+all.case.size.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+edu.highest.grp1+occup.grp1+occup.grp2+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=UNHCR)

# new: with arrival crosspoint dummies as well (group 4 is ref.)
reg3 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp1+arr.crosspoint.grp2+arr.crosspoint.grp3, data=UNHCR)

# wrong: took out insignificant dummies, same regression as above, now all variables significant at 1%
reg2 <- lm(ln.exppc~all.case.size+all.case.size.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+edu.highest.grp1+occup.grp1+occup.grp2+gender.male+sp.co.cash+mar_g_divorced+house.kitchen.d+bir_syria+dem_age_grp2+dem_age_grp3+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.dairy.wfp.assistance+edu.public.school+edu.interest.recreational.activities+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.zarqa+work.documentation+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable, data=UNHCR)

## ==========================================


####### FINAL MODEL (before taking out insignificant variables)
reg3 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+AVG_Age+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+concrete.house+house.kitchen.d+house.sanitary.d+ethn_arab+rel_sunni+bir_syria+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.pulses.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp1+arr.crosspoint.grp2+arr.crosspoint.grp3, data=UNHCR)

####### FINAL MODEL (after taking out insignificant variables; only insignificant dummy variables kept as required for refernce category)
reg3 <- lm(ln.exppc~case.size.vaf+case.size.vaf.sq+percentage_0_14+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+house.kitchen.d+rel_sunni+bir_syria+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.childatrisk+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp1+arr.crosspoint.grp2+arr.crosspoint.grp3, data=UNHCR)
# ref. categoires: (occup grp3 ref.; edu grp1 ref.; widows ref.; moi.amman ref.; crosspoint grp4 ref.)

#### FINAL (same as above, but with dummies for missed education, and no childatrisk indicator)
reg3 <- lm(ln.exppc~case.size.vaf+
             case.size.vaf.sq+
             percentage_0_14+hh.crowding+edu.highest.grp5+edu.highest.grp4+edu.highest.grp3+edu.highest.grp2+occup.grp1+occup.grp2+occup.grp4+gender.male+sp.co.cash+mar_single+mar_g_married+mar_g_divorced+house.kitchen.d+rel_sunni+bir_syria+dem_PA_grp1+dem_PA_grp2+arr_illegal+wash.improved.latrine+wash.wastewater.sewage+wash.wastewater.regular.removal+food.meat.wfp.assistance+food.dairy.wfp.assistance+edu.n.attending.school+edu.public.school+edu.missed.0to1year+edu.missed.1to3.years+edu.missed.morethan3years+edu.interest.recreational.activities+lactating.women.problem.NA+vaccination.measles.NA+separated.family.member.mother+separated.family.member.son+separated.family.member.daughter+entry.multiple.entries+moi.ajloun+moi.aqabah+moi.balqa+moi.irbid+moi.jerash+moi.karak+moi.maan+moi.madaba+moi.mafraq+moi.tafilah+moi.zarqa+work.documentation+work.agriculture+specific.needs.present+specific.needs.observed.singleparent+enumerator.judgement.notvulnerable+all.case.size+all.case.size.sq+arr.crosspoint.grp1+arr.crosspoint.grp2+arr.crosspoint.grp3, data=UNHCR)
# ref. categoires: (occup grp3 ref.; edu grp1 ref.; widows ref.; moi.amman ref.; crosspoint grp4 ref.; no children have missed education ref.)
#   this is the final model

## ==========================================



## ==========================================
# Regression Loop:
## ==========================================

options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 313
while (i<374) 
{
  
  reg <- lm(ln.exppc~UNHCR[,i], data=UNHCR);
  rsquared <- summary(reg)$r.squared;
  col1 <- as.character (i);
  col2 <- as.character (colnames(UNHCR)[i]);
  col3 <- rsquared;
  if (rsquared >= 0.01) 
  {
    hvAR <- rbind(hvAR,c(col1, col2, col3))
    
  }
  i <- i + 1;
}

## ==========================================

## ELSE ATTEMPT
if(dem_age < 35) {
  dem_age_group <- 1
} else if(dem_age >= 35 & dem_age < 55){
  dem_age_group <- 2
} else {
  dem_age_group <- 3
}
VAR <- ifelse(dem_age < 35, 1, ifelse(dem_age >= 35) & (dem_age < 55), 2, 3)
##

