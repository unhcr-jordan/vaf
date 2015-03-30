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


######### First step subset the dataframe in order to keep the column that make snese for the regression.


str(hve3)
names(hve3)

#Eliminate Columns with Arabic Text or potential ID:
### Those variable are already identified in the label info in CSV - column 5 --

labeldata <- read.csv("data/homevisit_label.csv", stringsAsFactors=FALSE)
names(labeldata)
dropvariable.homevisit <- labeldata[labeldata$identify=="yes",] 

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
droplabel.homevisit <- dropvariable.homevisit[, 3]


# str(droplabel.homevisit)
rm(labeldata)  

#hve4 <-hve3[,!(names(hve3) %in% droplabel.homevisit)]

hve4 <- hve3[, c( "Expenditure.Per.Capita" ,                                                                                                                                                                                                                                 
                  "ln.Expenditure.Per.Capita" ,                                                                                                                                                                                                                              
                  "Income.Per.Capita"       ,                                                                                                                                                                                                                                
                  "Income.Per.Capita.Squared"  ,                                                                                                                                                                                                                             
                  "Total.Expenditure2"    ,                                                                                                                                                                                                                                  
                  "Debt.To.Expenditure"  ,                                                                                                                                                                                                                                   
                  "Total.Income2"    ,                                                                                                                                                                                                                                       
                  "Debt.To.Income"  ,  
                  "Family.Size" ,
                  "Family.Size.Squared"  ,                                                                                                                                                                                                                                   
                  "Family.Size.All.File.Numbers.Squared" ,                                                                                                                                                                                                                   
                  "Coping.Strategies.Used.Last.Six.Months",                                                                                                                                                                                                                 
                  "Coping.Strategies.Used.Last.Six.Months.Squared",                                                                                                                                                                                                          
                  "House.Assets"  ,                                                                                                                                                                                                                                          
                  "House.Assets.To.Family.Size" ,                                                                                                                                                                                                                            
                  "House.Poor.Conditions" ,                                                                                                                                                                                                                                  
                  "Chronic.Diseases"   ,                                                                                                                                                                                                                                     
                  "Vaccinations.Not.Received" ,                                                                                                                                                                                                                              
                  "Disability.In.Family",                                                                                                                                                                                                                                    
                  "House.Luxury.Assets"  ,                                                                                                                                                                                                                                   
                  "House.Crowding"     ,                                                                                                                                                                                                                                     
                  "House.Crowding.Squared"   ,                                                                                                                                                                                                                               
                  "House.Crowding.squared"  ,                                                                                                                                                                                                                                
                  "House.Crowding.v2"   ,                                                                                                                                                                                                                                    
                  "House.Crowding.v2.Squared" ,                                                                                                                                                                                                                              
                  "Saving.Per.Family.Member" ,                                                                                                                                                                                                                               
                  "Age.Related.Disability",                                                                                                                                                                                                                                  
                  "Debt.Per.Capita"     ,                                                                                                                                                                                                                                    
                  "Rent.Occupancy"     ,                                                                                                                                                                                                                                     
                  "Spices.And.Condiments.Bought.With.Cash",
                  #"Coping.Strategies.Basic.Food.Needs",
                  #"Coping.Strategies.Basic.Food.Needs.Squared",
                  "Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian",
                  "Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..",
                  "Payment.Type.of.occupancy..For.rent",
                  "Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash"
                  )]

str(hve4)
names(hve4)

##########################################################################
#AUTOMATED REGRESSIONS:
##########################################################################
## Allow to double check explanatory power - rsquared-  of the variables for the expenditure per capita
## Caution: this is a heavy calculation -> we need to split the Df to make it manageable

options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 1
while (i<29) 
{
  reg <- lm(Expenditure.Per.Capita ~ hve4[,i], data=hve4);
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

## Order the hVar dataframe by descending rsquared
hvAR <- hvAR[order(- rsquared),]

## We keep a backup of this: 
write.csv(hvAR, file = "out/hvAR.csv")

## Let's check the top 10 variables with the biggest Rsquarred -- This will give us the variable to be included for further regression
head(hvAR, n=10)


##########################################################################
#REGRESSION MODELS: 
##########################################################################
#Preliminary Welfare Model:
reg1 <- lm(  Expenditure.Per.Capita ~ 
              House.Crowding +
              House.Crowding.Squared +
             # Coping.Strategies.Basic.Food.Needs +
             # Coping.Strategies.Basic.Food.Needs.Squared +
              Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
              Saving.Per.Family.Member +
              Debt.To.Expenditure +
              Income.Per.Capita +
              Income.Per.Capita.Squared +
              Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
              Family.Size.All.File.Numbers.Squared,            
            data=hve4
          )

### Write results in a dataframe to calculte predicted welfare index:
########################################################
#sink("out/summaryreg1.txt")
#summary(reg1)
#sink()
#str(summary(reg1))
#reg1.r.squared  <- as.data.frame(summary(reg1)$r.squared )
#reg1.adj.r.squared <- as.data.frame(summary(reg1)$adj.r.squared)
reg1.summary.coeff <- as.data.frame(summary(reg1)$coefficients[, 1:4])


#Preliminary Welfare Model 2:
reg2 <- lm( Expenditure.Per.Capita ~
              House.Crowding +
              House.Crowding.Squared +
            #  Coping.Strategies.Basic.Food.Needs +
             # Coping.Strategies.Basic.Food.Needs.Squared +
              Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
              Saving.Per.Family.Member +
              Debt.To.Expenditure +
              Income.Per.Capita +
              Income.Per.Capita.Squared+
              Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. +
              Family.Size.All.File.Numbers.Squared +
              Payment.Type.of.occupancy..For.rent + ## Additional variables
              Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash,   ## additional variables         
            data=hve4
            )

summary(reg2)
reg2.summary.coeff <- as.data.frame(summary(reg2)$coefficients[, 1:4])



#Final Welfare Model 1:
# Regressions Full:
reg.full <- lm(Expenditure.Per.Capita ~                 
                 Debt.To.Expenditure + ## This variable is changing
                 House.Crowding +
                 House.Crowding.Squared +
                 Income.Per.Capita +
                 Income.Per.Capita.Squared +
                 Family.Size +
                 Family.Size.Squared +
                 Spices.And.Condiments.Bought.With.Cash +
                 Rent.Occupancy,            
               data=hve4
              )
summary(reg.full)
reg.full.summary.coeff <- as.data.frame(summary(reg.full)$coefficients[, 1:4])

#Final Welfare Model 2:
reg.full.2 <- lm(Expenditure.Per.Capita ~
                   Debt.Per.Capita + ## This variable is changing
                   House.Crowding +
                   House.Crowding.Squared +
                   Income.Per.Capita +
                   Income.Per.Capita.Squared +
                   Family.Size +
                   Family.Size.Squared +
                   Spices.And.Condiments.Bought.With.Cash +
                   Rent.Occupancy,            
                 data=hve4
               )
summary(reg.full.2) 
reg.full.2.summary.coeff <- as.data.frame(summary(reg.full.2)$coefficients[, 1:4])





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

