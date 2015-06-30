## ==========================================

# Welfare Model (VAF + ProGres)

# Simplified Script
#   What is included:
#   1. Data cleaning
#   2. Variable generation for welfare model
#   3. Final welfare model

## ==========================================
# 1. Data cleaning
## ==========================================
UNHCR <- read.csv("data/One_Big_list_vaf_with_progres_ANSI_UNIX.txt", na.strings = "")

UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 == 0),]
UNHCR <- UNHCR [!(UNHCR$Household.information.Family.Size_5 >= 20),] 

UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 == 0),] 
UNHCR <- UNHCR [!(UNHCR$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH_10 >= 10),] 

UNHCR[is.na(UNHCR)]<-0
attach(UNHCR)



## ==========================================
# 2. Variable generation
## ==========================================

## PG Variables:
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

# Case Size:
case.size.pg <- (csize_act)





#########################################################################
## VAF Variables:


# Expenditure per capita:
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5)
UNHCR <- cbind(UNHCR, exp.pc.2)
UNHCR <- UNHCR [!(exp.pc.2 == 0),]
UNHCR <- UNHCR [!(exp.pc.2 > 1000),]
attach(UNHCR)
exp.pc.2 <- (Financial.Situation.Total.Expenditure_124/Household.information.Family.Size_5)

# Log expenditure per capita:
ln.exppc <- log(exp.pc.2)


# Case Size:
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

# Arrival crosspoint:
arr.crosspoint.grp1 <- ifelse((arr_crosspoint == "C - Airport Queeen Alia - Aleppo" | arr_crosspoint == "C - Airport Queeen Alia - Damascus" | arr_crosspoint == "C - Airport Queeen Alia - Third countries"), 1, 0)
arr.crosspoint.grp2 <- ifelse((arr_crosspoint == "E - Ruwaished - Hadallat"), 1, 0)
arr.crosspoint.grp3 <- ifelse((arr_crosspoint == "W - Jaber (Nassib) - Official" | arr_crosspoint == "W - Jaber (Nassib) unofficial"), 1, 0)





## ==========================================
# 3. Final welfare model
## ==========================================

welfaremodel <- lm(ln.exppc~
                     case.size.vaf +
                     case.size.vaf.sq +
                     percentage_0_14 +
                     hh.crowding +
                     edu.highest.grp5 +
                     edu.highest.grp4 +
                     edu.highest.grp3 +
                     edu.highest.grp2 +
                     occup.grp1 +
                     occup.grp2 +
                     occup.grp4 +
                     gender.male +
                     sp.co.cash +
                     mar_single +
                     mar_g_married +
                     mar_g_divorced +
                     house.kitchen.d +
                     rel_sunni +
                     bir_syria +
                     dem_PA_grp1 +
                     dem_PA_grp2 +
                     arr_illegal +
                     wash.improved.latrine+wash.wastewater.sewage +
                     wash.wastewater.regular.removal +
                     food.meat.wfp.assistance +
                     food.dairy.wfp.assistance +
                     edu.n.attending.school +
                     edu.public.school +
                     edu.missed.0to1year +
                     edu.missed.1to3.years +
                     edu.missed.morethan3years +
                     edu.interest.recreational.activities +
                     lactating.women.problem.NA +
                     vaccination.measles.NA +
                     separated.family.member.mother +
                     separated.family.member.son +
                     separated.family.member.daughter +
                     entry.multiple.entries +
                     moi.ajloun +
                     moi.aqabah +
                     moi.balqa +
                     moi.irbid +
                     moi.jerash +
                     moi.karak +
                     moi.maan +
                     moi.madaba +
                     moi.mafraq +
                     moi.tafilah +
                     moi.zarqa +
                     work.documentation +
                     work.agriculture +
                     specific.needs.present +
                     specific.needs.observed.singleparent +
                     enumerator.judgement.notvulnerable +
                     all.case.size +
                     all.case.size.sq +
                     arr.crosspoint.grp1 +
                     arr.crosspoint.grp2 +
                     arr.crosspoint.grp3,
                   data=UNHCR)
#summary(welfaremodel)

welfaremodel.summary.coeff <- as.data.frame(summary(welfaremodel)$coefficients[, 1:4])

UNHCR$predictedwellfare.modelregfull <- ( 
    
    UNHCR$case.size.vaf * welfaremodel.summary.coeff[2,1]) +
  (UNHCR$case.size.vaf.sq * welfaremodel.summary.coeff[3,1]) +
  (UNHCR$percentage_0_14 * welfaremodel.summary.coeff[4,1]) +
  (UNHCR$hh.crowding * welfaremodel.summary.coeff[5,1]) +
  (UNHCR$edu.highest.grp5 * welfaremodel.summary.coeff[6,1]) +
  (UNHCR$edu.highest.grp4 * welfaremodel.summary.coeff[7,1]) +
  (UNHCR$edu.highest.grp3 * welfaremodel.summary.coeff[8,1]) +
  (UNHCR$edu.highest.grp2 * welfaremodel.summary.coeff[9,1]) +
  (UNHCR$occup.grp1 * welfaremodel.summary.coeff[10,1]) +
  (UNHCR$occup.grp2 * welfaremodel.summary.coeff[11,1]) +
  (UNHCR$occup.grp4 * welfaremodel.summary.coeff[12,1]) +
  ( UNHCR$gender.male  * welfaremodel.summary.coeff[13,1]) +
  ( UNHCR$sp.co.cash  * welfaremodel.summary.coeff[14,1]) +
  ( UNHCR$mar_single  * welfaremodel.summary.coeff[15,1]) +
  ( UNHCR$mar_g_married  * welfaremodel.summary.coeff[16,1]) +
  ( UNHCR$mar_g_divorced  * welfaremodel.summary.coeff[17,1]) +
  ( UNHCR$house.kitchen.d  * welfaremodel.summary.coeff[18,1]) +
  ( UNHCR$rel_sunni  * welfaremodel.summary.coeff[19,1]) +
  ( UNHCR$bir_syria  * welfaremodel.summary.coeff[20,1]) +
  ( UNHCR$dem_PA_grp1  * welfaremodel.summary.coeff[21,1]) +
  ( UNHCR$dem_PA_grp2  * welfaremodel.summary.coeff[22,1]) +
  ( UNHCR$arr_illegal  * welfaremodel.summary.coeff[23,1]) +
  ( UNHCR$wash.improved.latrine+wash.wastewater.sewage  * welfaremodel.summary.coeff[24,1]) +
  ( UNHCR$wash.wastewater.regular.removal  * welfaremodel.summary.coeff[25,1]) +
  ( UNHCR$food.meat.wfp.assistance  * welfaremodel.summary.coeff[26,1]) +
  ( UNHCR$food.dairy.wfp.assistance  * welfaremodel.summary.coeff[27,1]) +
  ( UNHCR$edu.n.attending.school  * welfaremodel.summary.coeff[28,1]) +
  ( UNHCR$edu.public.school  * welfaremodel.summary.coeff[29,1]) +
  ( UNHCR$edu.missed.0to1year  * welfaremodel.summary.coeff[30,1]) +
  ( UNHCR$edu.missed.1to3.years  * welfaremodel.summary.coeff[31,1]) +
  ( UNHCR$edu.missed.morethan3years  * welfaremodel.summary.coeff[32,1]) +
  ( UNHCR$edu.interest.recreational.activities  * welfaremodel.summary.coeff[33,1]) +
  ( UNHCR$lactating.women.problem.NA  * welfaremodel.summary.coeff[34,1]) +
  ( UNHCR$vaccination.measles.NA  * welfaremodel.summary.coeff[35,1]) +
  ( UNHCR$separated.family.member.mother  * welfaremodel.summary.coeff[36,1]) +
  ( UNHCR$separated.family.member.son  * welfaremodel.summary.coeff[37,1]) +
  ( UNHCR$separated.family.member.daughter  * welfaremodel.summary.coeff[38,1]) +
  ( UNHCR$entry.multiple.entries  * welfaremodel.summary.coeff[39,1]) +
  ( UNHCR$moi.ajloun  * welfaremodel.summary.coeff[40,1]) +
  ( UNHCR$moi.aqabah  * welfaremodel.summary.coeff[41,1]) +
  ( UNHCR$moi.balqa  * welfaremodel.summary.coeff[42,1]) +
  ( UNHCR$moi.irbid  * welfaremodel.summary.coeff[43,1]) +
  ( UNHCR$moi.jerash  * welfaremodel.summary.coeff[44,1]) +
  ( UNHCR$moi.karak  * welfaremodel.summary.coeff[45,1]) +
  ( UNHCR$moi.maan  * welfaremodel.summary.coeff[46,1]) +
  ( UNHCR$moi.madaba  * welfaremodel.summary.coeff[47,1]) +
  ( UNHCR$moi.mafraq  * welfaremodel.summary.coeff[48,1]) +
  ( UNHCR$moi.tafilah  * welfaremodel.summary.coeff[49,1]) +
  ( UNHCR$moi.zarqa  * welfaremodel.summary.coeff[50,1]) +
  ( UNHCR$work.documentation  * welfaremodel.summary.coeff[51,1]) +
  ( UNHCR$work.agriculture  * welfaremodel.summary.coeff[52,1]) +
  ( UNHCR$specific.needs.present  * welfaremodel.summary.coeff[53,1]) +
  ( UNHCR$specific.needs.observed.singleparent  * welfaremodel.summary.coeff[54,1]) +
  ( UNHCR$enumerator.judgement.notvulnerable  * welfaremodel.summary.coeff[55,1]) +
  ( UNHCR$all.case.size  * welfaremodel.summary.coeff[56,1]) +
  ( UNHCR$all.case.size.sq  * welfaremodel.summary.coeff[57,1]) +
  ( UNHCR$arr.crosspoint.grp1  * welfaremodel.summary.coeff[58,1]) +
  ( UNHCR$arr.crosspoint.grp2  * welfaremodel.summary.coeff[59,1]) +
  ( UNHCR$arr.crosspoint.grp3  * welfaremodel.summary.coeff[60,1]) 
