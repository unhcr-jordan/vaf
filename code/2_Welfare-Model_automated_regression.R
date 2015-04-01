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

hve.model <- hve

#str(hve.model)
#names(hve.model)

#Eliminate Columns with Arabic Text or potential ID:
### Those variable are already identified in the label info in CSV - column 5 --

#labeldata <- read.csv("data/homevisit_label.csv", stringsAsFactors=FALSE)
#names(labeldata)
#dropvariable.homevisit <- labeldata[labeldata$identify=="yes",]

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#droplabel.homevisit <- dropvariable.homevisit[, 3]


# str(droplabel.homevisit)
#rm(labeldata)

#hve4 <-hve3[,!(names(hve3) %in% droplabel.homevisit)]

hve.model1 <- hve.model [ , c( "dataset" ,
                               "lat" ,
                               "long" ,
                               "Expenditure.Per.Capita" , "ln.Expenditure.Per.Capita" ,
                               "Income.Per.Capita" ,
                               "Income.Per.Capita.Squared" ,
                               "Total.Expenditure2" ,
                               "Debt.To.Expenditure" ,
                               "Total.Income2" ,
                               "Debt.To.Income"  ,
                               "Family.Size" ,
                               "Family.Size.Squared"  ,
                               "Family.Size.All.File.Numbers.Squared",
                               "Coping.Strategies.Used.Last.Six.Months",
                               "Coping.Strategies.Used.Last.Six.Months.Squared"  ,
                               "House.Assets"  ,
                               "House.Assets.To.Family.Size" ,
                               "House.Poor.Conditions" ,
                               "Chronic.Diseases"  ,
                               "Vaccinations.Not.Received" ,
                               "Disability.In.Family" ,
                               "House.Luxury.Assets"  ,
                               "House.Crowding"     ,
                               "House.Crowding.Squared"   ,
                               "House.Crowding.squared"  ,
                               "House.Crowding.v2"   ,
                               "House.Crowding.v2.Squared" ,
                               "Saving.Per.Family.Member" ,
                               "Age.Related.Disability" ,
                               "Debt.Per.Capita"     ,
                               "Rent.Occupancy"     ,
                               "Rent.Occupancy.grp1" ,
                               "Rent.Occupancy.grp2" ,
                               "Rent.Occupancy.grp3" ,
                               "Rent.Occupancy.grp4"  ,
                               "Rent.Occupancy.grp5" ,
                               #"Coping.Strategies.Basic.Food.Needs",
                               #"Coping.Strategies.Basic.Food.Needs.Squared",
                               # "Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian",
                               # "Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..",
                               # "Payment.Type.of.occupancy..For.rent",
                               #  "Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash",
                               "Spices.And.Condiments.Bought.With.Cash"
)]

str(hve.model)
names(hve.model)

##########################################################################
#AUTOMATED REGRESSIONS:
##########################################################################
## Allow to double check explanatory power - rsquared-  of the variables for the expenditure per capita
## Caution: this is a heavy calculation -> we need to split the Df to make it manageable

options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 1
while (i<650)
{
  reg <- lm(Expenditure.Per.Capita ~ hve.model[,i], data=hve.model);
  rsquared <- summary(reg)$r.squared;
  col1 <- as.character (i);
  col2 <- as.character (colnames(hve.model)[i]);
  col3 <- rsquared;
  # if (rsquared >= 0.01)
  # {
  #   hvAR <- rbind(hvAR,c(col1, col2, col3))
  # }
  # i <- i + 1;
}

## Order the hVar dataframe by descending rsquared
hvAR <- hvAR[order(- rsquared),]

## We keep a backup of this:
write.csv(hvAR, file = "out/hvAR.csv")

## Let's check the top 10 variables with the biggest Rsquarred -- This will give us the variable to be included for further regression
head(hvAR, n=10)


#Preliminary Welfare Model 2:
vw2 <- lm(  Expenditure.Per.Capita ~
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
            data=hve
)

### Write results in a dataframe to calculte predicted welfare index:
########################################################
#sink("out/summaryvw2.txt")
#summary(vw2)
#sink()
#str(summary(vw2))
#vw2.r.squared  <- as.data.frame(summary(vw2)$r.squared )
#vw2.adj.r.squared <- as.data.frame(summary(vw2)$adj.r.squared)
vw2.summary.coeff <- as.data.frame(summary(vw2)$coefficients[, 1:4])


#Preliminary Welfare Model 3:
vw3 <- lm( Expenditure.Per.Capita ~
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

summary(vw3)
vw3.summary.coeff <- as.data.frame(summary(vw3)$coefficients[, 1:4])



#Preliminary Welfare Model 4:
vw4 <- lm(Expenditure.Per.Capita ~
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
summary(vw4)
vw4.summary.coeff <- as.data.frame(summary(vw4)$coefficients[, 1:4])


