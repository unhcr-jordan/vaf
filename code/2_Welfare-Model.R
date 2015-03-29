##########################################################################
#### Import data & Rename variable
source("code/1_composite_variable_homevisit.R")



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


##################################################################
## Welfare model : The objective is predict the variable "Expenditure.Per.Capita"
## In order to perform this, the first step is to identify variables with biggest explanatory power 
## then some liner  regression in order to find the weight to be used for the prediction


##########################################################################
#AUTOMATED REGRESSIONS:
##########################################################################
## Allow to double check explantory power - rsquared-  of the variables for the expenditure per capita


options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 1
while (i<702) 
{
  
  reg <- lm(Expenditure.Per.Capita~hve3[,i], data=hve3);
  rsquared <- summary(reg)$r.squared;
  col1 <- as.character (i);
  col2 <- as.character (colnames(hve3)[i]);
  col3 <- rsquared;
  if (rsquared >= 0.01) 
  {
    hvAR <- rbind(hvAR,c(col1, col2, col3))
    
  }
  i <- i + 1;
}

##########################################################################
#REGRESSION MODELS: 
##########################################################################
#Preliminary Welfare Model:
reg <- lm(Expenditure.Per.Capita ~ 
            House.Crowding +
            House.Crowding.Squared +
            Coping.Strategies.Basic.Food.Needs +
            Coping.Strategies.Basic.Food.Needs.Squared +
            hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian. +
            Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared +
            hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
            Family.Size.All.File.Numbers.Squared
          )

#Preliminary Welfare Model 2:
reg2 <- lm( Expenditure.Per.Capita ~
              House.Crowding +
              House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs +
              Coping.Strategies.Basic.Food.Needs.Squared +
              hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian. +
              Saving.Per.Family.Member+Debt.To.Expenditure +
              Income.Per.Capita +
              Income.Per.Capita.Squared+hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared +
              hve3$Payment.Type.of.occupancy..For.rent +
              hve3$Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash
            )

#Bonferroni outliers:  
outlierTest(reg2)

#Leverage & Hat-values outliers:
K<-length(coef(reg2))
N<-nrow(reg2$model)
avghat<-K/N
unname(which(hatvalues(reg2)>2*avghat))

#Cooks Distance outliers:
unname(which(cooks.distance(reg2)>4/reg2$df.residual))

#Matching Outliers:
outlier <- hve3[-c(4247,2444,4167,823,616,2716,4210,733),]

#THE WELFARE MODEL: 7 Variables = 55.87  
reg <- lm( Expenditure.Per.Capita ~
              House.Crowding +
              House.Crowding.Squared +
              Coping.Strategies.Basic.Food.Needs +
              Coping.Strategies.Basic.Food.Needs.Squared +
              outlier$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian. +
              Saving.Per.Family.Member+Debt.To.Expenditure +
              Income.Per.Capita+Income.Per.Capita.Squared +
              outlier$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
              Family.Size.All.File.Numbers.Squared,            
           data=outlier)

summary(reg)

#THE WELFARE MODEL: 9 Variables = 57.13
reg2 <- lm( Expenditure.Per.Capita ~
              House.Crowding +
              House.Crowding.Squared +
              Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared +
              outlier$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+
              Saving.Per.Family.Member +
              Debt.To.Expenditure+Income.Per.Capita +
              Income.Per.Capita.Squared +
              outlier$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+
              Family.Size.All.File.Numbers.Squared+Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash+Payment.Type.of.occupancy..For.rent,
            data=outlier)

summary(reg2)


##########################################################################
#PROBIT MODEL:
##########################################################################
#Poor Variable (if expenditure per capita less than or equal to 50 then 1, otherwise 0):

#Probit: 7 Variables:
Poor<- ifelse( Expenditure.Per.Capita<=50, 1, 0)
Y <- cbind(Poor)
X <- cbind( House.Crowding, 
            House.Crowding.Squared, 
            Coping.Strategies.Basic.Food.Needs, 
            Coping.Strategies.Basic.Food.Needs.Squared, 
            hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., 
            Saving.Per.Family.Member, 
            Debt.To.Expenditure, 
            Income.Per.Capita, 
            Income.Per.Capita.Squared, 
            hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., 
            Family.Size.All.File.Numbers.Squared
            )

probit<-glm(Y~X, family=binomial (link="probit"), data=outlier)
summary(probit)
table(true = Y, pred = round(fitted(probit)))  

#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash, Payment.Type.of.occupancy..For.rent)

probit2 <- glm(Y~X, family=binomial (link="probit"), data=outlier)
summary(probit2)
table(true = Y, pred = round(fitted(probit2)))


##########################################################################
#LOGIT MODEL:
##########################################################################

#Logit: 7 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, data=outlier)
logit<-glm(Poor~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared, family=binomial (link="logit"), data=outlier)
summary(logit)
table(true = Y, pred = round(fitted(logit)))  

#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash, Payment.Type.of.occupancy..For.rent, data=outlier)
logit2<-glm(Y~X, family=binomial (link="logit"), data=outlier)
summary(logit2)
table(true = Y, pred = round(fitted(logit2)))

##########################################################################
