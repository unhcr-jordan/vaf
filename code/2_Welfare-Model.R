##########################################################################
#### Import data & Rename variable
#source("code/1_composite_variable_homevisit.R")



##########################################################################
#PACKAGES TO ATTACH FOR ANALYSIS:
##########################################################################
#install.packages("car")
#install.packages("tseries")
#install.packages("lme4")
#install.packages("lmtest")

library(car)
library(tseries)
library(lme4)
library(lmtest)
library(classInt)
library(plyr)


##################################################################
## Welfare model : The objective is predict the variable "Expenditure.Per.Capita"
## In order to perform this, the first step is to identify variables with biggest explanatory power
## See script:  2_Welfare-Model_automated_regression.R 
## then some liner  regression in order to find the weight to be used for the prediction


######### First step subset the dataframe in order to keep the column that make snese for the regression.

hve.model <- hve
#names(hve.model)

hve.model3 <-hve[ hve$dataset == "homevisit3",  ]
hve.model4 <-hve[ hve$dataset == "homevisit4",  ]
summary(hve.model4$Family.Size)
##########################################################################
#REGRESSION MODELS:
##########################################################################


#Final - VAF Only - Welfare Model: vw5 -- on dataset v3 -- ie.e full dataset -snowball sample
# Regressions Full:
vw5.v3 <- lm(Expenditure.Per.Capita ~
            Debt.To.Expenditure + ## This variable is changing
            House.Crowding +
            House.Crowding.Squared +
            Income.Per.Capita +
            Income.Per.Capita.Squared +
            Family.Size +
            Family.Size.Squared +
            Spices.And.Condiments.Bought.With.Cash +
            Rent.Occupancy,
          data=hve.model3
)
#summary(vw5)
vw5.v3.summary.coeff <- as.data.frame(summary(vw5.v3)$coefficients[, 1:4])


### Generate predicted welfare index based on variables of the model generated through v3 dataset
hve$predictedwellfare.vw5.v3 <- ( hve$Debt.To.Expenditure * vw5.v3.summary.coeff[2,1]) +
  ( hve$House.Crowding * vw5.v3.summary.coeff[3,1]) +
  ( hve$House.Crowding.Squared * vw5.v3.summary.coeff[4,1]) +
  ( hve$Income.Per.Capita * vw5.v3.summary.coeff[5,1]) +
  ( hve$Income.Per.Capita.Squared * vw5.v3.summary.coeff[6,1]) +
  ( hve$Family.Size * vw5.v3.summary.coeff[7,1]) +
  ( hve$Family.Size.Squared * vw5.v3.summary.coeff[8,1]) +
  ( hve$Spices.And.Condiments.Bought.With.Cash * vw5.v3.summary.coeff[9,1]) +
  ( hve$Rent.Occupancy* vw5.v3.summary.coeff[10,1])

summary(hve$predictedwellfare.vw5.v3)
hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue",
     main = "Histogram of Welfare Model -vw5- estimated on V3 dataset", xlab = "Expected welfare Score ")

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$predictedwellfare.vw5.v3.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v3, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1000))))
hve$predictedwellfare.vw5.v3.class <- revalue(hve$predictedwellfare.vw5.v3.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$predictedwellfare.vw5.v3.class  <- factor(hve$predictedwellfare.vw5.v3.class, levels = c("Severe", "High", "Moderate", "Low"))
View(hve$predictedwellfare.vw5.v3.class)


#Final Welfare Model: vw5 -- on dataset v4 -- i.e. representative sample
# Regressions Full:
vw5.v4 <- lm(Expenditure.Per.Capita ~
               Debt.To.Expenditure + ## This variable is changing
               House.Crowding +
               House.Crowding.Squared +
               Income.Per.Capita +
               Income.Per.Capita.Squared +
               Family.Size +
               Family.Size.Squared +
               Spices.And.Condiments.Bought.With.Cash +
               Rent.Occupancy,
             data=hve.model4
)
#summary(vw5)
vw5.v4.summary.coeff <- as.data.frame(summary(vw5.v4)$coefficients[, 1:4])
### Generate predicted welfare index based on variables of the model generated through v4 dataset
hve$predictedwellfare.vw5.v4 <- ( hve$Debt.To.Expenditure * vw5.v4.summary.coeff[2,1]) +
  ( hve$House.Crowding * vw5.v4.summary.coeff[3,1]) +
  ( hve$House.Crowding.Squared * vw5.v4.summary.coeff[4,1]) +
  ( hve$Income.Per.Capita * vw5.v4.summary.coeff[5,1]) +
  ( hve$Income.Per.Capita.Squared * vw5.v4.summary.coeff[6,1]) +
  ( hve$Family.Size * vw5.v4.summary.coeff[7,1]) +
  ( hve$Family.Size.Squared * vw5.v4.summary.coeff[8,1]) +
  ( hve$Spices.And.Condiments.Bought.With.Cash * vw5.v4.summary.coeff[9,1]) +
  ( hve$Rent.Occupancy* vw5.v4.summary.coeff[10,1])

summary(hve$predictedwellfare.vw5.v4)
hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue",
     main = "Histogram of Welfare Model -vw5- estimated on V3 dataset", xlab = "Expected welfare Score ")

#View(hve$predictedwellfare.vw5.v3)
## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$predictedwellfare.vw5.v4.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v4 , n = 4, style = "fixed", fixedBreaks = c(-10000, 28, 68 , 100))))
hve$predictedwellfare.vw5.v4.class <- revalue(hve$predictedwellfare.vw5.v4.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$predictedwellfare.vw5.v4.class  <- factor(hve$predictedwellfare.vw5.v4.class, levels = c("Severe", "High", "Moderate", "Low"))




######################################################################
#Bonferroni outliers : Eliminate records
####################################################################

outlierTest(reg2)

#Leverage & Hat-values outliers:
K<-length(coef(reg2))
N<-nrow(reg2$model)
avghat<-K/N
unname(which(hatvalues(reg2)>2*avghat))

#Cooks Distance outliers:
unname(which(cooks.distance(reg2)>4/reg2$df.residual))

#Matching Outliers:
outlier <- hve4[-c(4247,2444,4167,823,616,2716,4210,733),]

#THE WELFARE MODEL: 7 Variables = 55.87
regoutlier1 <- lm( Expenditure.Per.Capita ~
                     House.Crowding +
                     House.Crowding.Squared +
                     Coping.Strategies.Basic.Food.Needs +
                     Coping.Strategies.Basic.Food.Needs.Squared +
                     Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
                     Saving.Per.Family.Member+Debt.To.Expenditure +
                     Income.Per.Capita+Income.Per.Capita.Squared +
                     Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
                     Family.Size.All.File.Numbers.Squared,
                   data=outlier)
summary(regoutlier1)
regoutlier1.summary.coeff <- as.data.frame(summary(regoutlier1)$coefficients[, 1:4])

#THE WELFARE MODEL: 9 Variables = 57.13
regoutlier2 <- lm( Expenditure.Per.Capita ~
                     House.Crowding +
                     House.Crowding.Squared +
                     Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared +
                     Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
                     Saving.Per.Family.Member +
                     Debt.To.Expenditure+Income.Per.Capita +
                     Income.Per.Capita.Squared +
                     Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
                     Family.Size.All.File.Numbers.Squared +
                     Payment.Type.of.occupancy..For.rent +
                     Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash,
                   data=outlier)
summary(regoutlier2)
regoutlier2.summary.coeff <- as.data.frame(summary(regoutlier2)$coefficients[, 1:4])


##########################################################################
#PROBIT MODEL:
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
