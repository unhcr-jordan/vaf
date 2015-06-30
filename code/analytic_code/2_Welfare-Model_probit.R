##########################################################################
#PROBIT MODEL:
# Limited Dependent Variable (LDV) models, more specifically the Probit and the Logit models are binary classification models
# and the outcome variable takes value 0 for ‘non-poor’ or 1 for ‘poor’. 
# The poverty threshold is set at 50 JD per person per month. 
# The estimated response probabilities are strictly between zero and one. 
# Values above 0.5 denote ‘poor’. The Probit model is based on the standard normal cumulative
# distribution function (cdf), whilst the standard logistic cdf yields the Logit model. 
##########################################################################
#Poor Variable (if expenditure per capita less than or equal to 50 then 1, otherwise 0):

#Probit: 7 Variables:
Poor<- ifelse( Expenditure.Per.Capita<=50, 1, 0)
Y <- cbind(Poor)
X <- cbind( hve4$House.Crowding,
            hve4$House.Crowding.Squared,
            hve4$Coping.Strategies.Basic.Food.Needs,
            hve4$Coping.Strategies.Basic.Food.Needs.Squared,
            hve4$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian,
            hve4$Saving.Per.Family.Member,
            hve4$Debt.To.Expenditure,
            hve4$Income.Per.Capita,
            hve4$Income.Per.Capita.Squared,
            hve4$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..,
            hve4$Family.Size.All.File.Numbers.Squared
)

probit <- glm(Y~X, family=binomial (link="probit"), data=outlier)

summary(probit)

table(true = Y, pred = round(fitted(probit)))

#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding,
           House.Crowding.Squared,
           Coping.Strategies.Basic.Food.Needs,
           Coping.Strategies.Basic.Food.Needs.Squared,
           hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian,
           Saving.Per.Family.Member,
           Debt.To.Expenditure,
           Income.Per.Capita,
           Income.Per.Capita.Squared,
           hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..,
           Family.Size.All.File.Numbers.Squared,
           Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash,
           Payment.Type.of.occupancy..For.rent
)

probit2 <- glm(Y~X, family=binomial (link="probit"), data=outlier)
summary(probit2)
table(true = Y, pred = round(fitted(probit2)))


##########################################################################
#LOGIT MODEL:
##########################################################################

#Logit: 7 Variables:
Y <- cbind(Poor)
X <- cbind(
  House.Crowding,
  House.Crowding.Squared,
  Coping.Strategies.Basic.Food.Needs,
  Coping.Strategies.Basic.Food.Needs.Squared,
  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian,
  Saving.Per.Family.Member,
  Debt.To.Expenditure,
  Income.Per.Capita,
  Income.Per.Capita.Squared,
  hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..,
  Family.Size.All.File.Numbers.Squared,
  data=outlier)

logit<-glm(Poor~
             House.Crowding +
             House.Crowding.Squared +
             Coping.Strategies.Basic.Food.Needs +
             Coping.Strategies.Basic.Food.Needs.Squared +
             Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
             Saving.Per.Family.Member+Debt.To.Expenditure +
             Income.Per.Capita +
             Income.Per.Capita.Squared +
             Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
             Family.Size.All.File.Numbers.Squared,
           family=binomial (link="logit"), data=outlier)
summary(logit)
table(true = Y, pred = round(fitted(logit)))

#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(
  House.Crowding,
  House.Crowding.Squared,
  Coping.Strategies.Basic.Food.Needs,
  Coping.Strategies.Basic.Food.Needs.Squared,
  Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian,
  Saving.Per.Family.Member,
  Debt.To.Expenditure,
  Income.Per.Capita,
  Income.Per.Capita.Squared,
  Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..,
  Family.Size.All.File.Numbers.Squared,
  Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash,
  Payment.Type.of.occupancy..For.rent,
  data=outlier)
logit2<-glm(Y~X, family=binomial (link="logit"), data=outlier)
summary(logit2)
table(true = Y, pred = round(fitted(logit2)))


##### Wordlbank model
## This model

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
                     wash.improved.latrine+
                     wash.wastewater.sewage +
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
                   data=homevisit)


welfaremodel.summary.coeff <- as.data.frame(summary(welfaremodel)$coefficients[, 1:4])

homevisit$predictedwellfare.modelregfull <- (
  
  homevisit$case.size.vaf * welfaremodel.summary.coeff[2,1]) +
  (homevisit$case.size.vaf.sq * welfaremodel.summary.coeff[3,1]) +
  (homevisit$hh.crowding * welfaremodel.summary.coeff[5,1]) +
  
  ### Variables from progres
  
  (homevisit$percentage_0_14 * welfaremodel.summary.coeff[4,1]) +
  (homevisit$edu.highest.grp5 * welfaremodel.summary.coeff[6,1]) +
  (homevisit$edu.highest.grp4 * welfaremodel.summary.coeff[7,1]) +
  (homevisit$edu.highest.grp3 * welfaremodel.summary.coeff[8,1]) +
  (homevisit$edu.highest.grp2 * welfaremodel.summary.coeff[9,1]) +
  (homevisit$occup.grp1 * welfaremodel.summary.coeff[10,1]) +
  (homevisit$occup.grp2 * welfaremodel.summary.coeff[11,1]) +
  (homevisit$occup.grp4 * welfaremodel.summary.coeff[12,1]) +
  ( homevisit$gender.male  * welfaremodel.summary.coeff[13,1]) +
  ( homevisit$sp.co.cash  * welfaremodel.summary.coeff[14,1]) +
  ( homevisit$mar_single  * welfaremodel.summary.coeff[15,1]) +
  ( homevisit$mar_g_married  * welfaremodel.summary.coeff[16,1]) +
  ( homevisit$mar_g_divorced  * welfaremodel.summary.coeff[17,1]) +
  ( homevisit$house.kitchen.d  * welfaremodel.summary.coeff[18,1]) +
  ( homevisit$rel_sunni  * welfaremodel.summary.coeff[19,1]) +
  ( homevisit$bir_syria  * welfaremodel.summary.coeff[20,1]) +
  ( homevisit$dem_PA_grp1  * welfaremodel.summary.coeff[21,1]) +
  ( homevisit$dem_PA_grp2  * welfaremodel.summary.coeff[22,1]) +
  ( homevisit$arr_illegal  * welfaremodel.summary.coeff[23,1]) +
  ( homevisit$wash.improved.latrine* welfaremodel.summary.coeff[24,1]) +
  ( homevisit$wash.wastewater.sewage  * welfaremodel.summary.coeff[24,1]) +
  ( homevisit$wash.wastewater.regular.removal  * welfaremodel.summary.coeff[25,1]) +
  ( homevisit$food.meat.wfp.assistance  * welfaremodel.summary.coeff[26,1]) +
  ( homevisit$food.dairy.wfp.assistance  * welfaremodel.summary.coeff[27,1]) +
  ( homevisit$edu.n.attending.school  * welfaremodel.summary.coeff[28,1]) +
  ( homevisit$edu.public.school  * welfaremodel.summary.coeff[29,1]) +
  ( homevisit$edu.missed.0to1year  * welfaremodel.summary.coeff[30,1]) +
  ( homevisit$edu.missed.1to3.years  * welfaremodel.summary.coeff[31,1]) +
  ( homevisit$edu.missed.morethan3years  * welfaremodel.summary.coeff[32,1]) +
  ( homevisit$edu.interest.recreational.activities  * welfaremodel.summary.coeff[33,1]) +
  ( homevisit$lactating.women.problem.NA  * welfaremodel.summary.coeff[34,1]) +
  ( homevisit$vaccination.measles.NA  * welfaremodel.summary.coeff[35,1]) +
  ( homevisit$separated.family.member.mother  * welfaremodel.summary.coeff[36,1]) +
  ( homevisit$separated.family.member.son  * welfaremodel.summary.coeff[37,1]) +
  ( homevisit$separated.family.member.daughter  * welfaremodel.summary.coeff[38,1]) +
  ( homevisit$entry.multiple.entries  * welfaremodel.summary.coeff[39,1]) +
  ( homevisit$moi.ajloun  * welfaremodel.summary.coeff[40,1]) +
  ( homevisit$moi.aqabah  * welfaremodel.summary.coeff[41,1]) +
  ( homevisit$moi.balqa  * welfaremodel.summary.coeff[42,1]) +
  ( homevisit$moi.irbid  * welfaremodel.summary.coeff[43,1]) +
  ( homevisit$moi.jerash  * welfaremodel.summary.coeff[44,1]) +
  ( homevisit$moi.karak  * welfaremodel.summary.coeff[45,1]) +
  ( homevisit$moi.maan  * welfaremodel.summary.coeff[46,1]) +
  ( homevisit$moi.madaba  * welfaremodel.summary.coeff[47,1]) +
  ( homevisit$moi.mafraq  * welfaremodel.summary.coeff[48,1]) +
  ( homevisit$moi.tafilah  * welfaremodel.summary.coeff[49,1]) +
  ( homevisit$moi.zarqa  * welfaremodel.summary.coeff[50,1]) +
  ( homevisit$work.documentation  * welfaremodel.summary.coeff[51,1]) +
  ( homevisit$work.agriculture  * welfaremodel.summary.coeff[52,1]) +
  ( homevisit$specific.needs.present  * welfaremodel.summary.coeff[53,1]) +
  ( homevisit$specific.needs.observed.singleparent  * welfaremodel.summary.coeff[54,1]) +
  ( homevisit$enumerator.judgement.notvulnerable  * welfaremodel.summary.coeff[55,1]) +
  ( homevisit$all.case.size  * welfaremodel.summary.coeff[56,1]) +
  ( homevisit$all.case.size.sq  * welfaremodel.summary.coeff[57,1]) +
  ( homevisit$arr.crosspoint.grp1  * welfaremodel.summary.coeff[58,1]) +
  ( homevisit$arr.crosspoint.grp2  * welfaremodel.summary.coeff[59,1]) +
  ( homevisit$arr.crosspoint.grp3  * welfaremodel.summary.coeff[60,1])
