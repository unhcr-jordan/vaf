##########################################################################
#### Import data & Rename variable
#source("code/processing_code/1_composite_variable_homevisit.R")



##########################################################################
#PACKAGES TO ATTACH FOR ANALYSIS:
##########################################################################
#install.packages("car")
#install.packages("tseries")
#install.packages("lme4")
#install.packages("lmtest")

library(car)
library(lme4)
library(lmtest)
library(classInt)
library(plyr)
library(ggplot2)


##################################################################
## Welfare model : The objective is predict the variable "Expenditure.Per.Capita"
# econometrical estimation computed using Ordinary Least Squares (OLS) analysis. 
## In order to perform this, the first step is to identify variables with biggest explanatory power
## See script:  2_Automated_stepwise_regression.R 
## then some linear regression in order to find the weight to be used for the prediction


######### First step subset the dataframe in order to keep the column that make snese for the regression.

hve.model <- hve
levels(hve.model$dataset)

summary(hve.model$dataset)
names(hve.model)


hve.model3 <-hve.model[ hve.model$dataset == "homevisit3", c("Expenditure.Per.Capita",
                                                 "Debt.To.Expenditure",
                                                 "House.Crowding",
                                                 "House.Crowding.Squared",
                                                 "Income.Per.Capita",
                                                 "Income.Per.Capita.Squared",
                                                 "Family.Size",
                                                 "Family.Size.Squared",
                                                 "Spices.And.Condiments.Bought.With.Cash",
                                                 "Rent.Occupancy")  ]

hve.model4 <-hve.model[ hve.model$dataset == "homevisit4", c("Expenditure.Per.Capita",
                                                 "Debt.To.Expenditure",
                                                 "House.Crowding",
                                                 "House.Crowding.Squared",
                                                 "Income.Per.Capita",
                                                 "Income.Per.Capita.Squared",
                                                 "Family.Size",
                                                 "Family.Size.Squared",
                                                 "Spices.And.Condiments.Bought.With.Cash",
                                                 "Rent.Occupancy")  ]

#summary(hve.model4$Family.Size)
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

## Get summary -  for interpretation look at http://blog.yhathq.com/posts/r-lm-summary.html
summary(vw5.v3)
vw5.v3.summary.coeff <- as.data.frame(summary(vw5.v3)$coefficients[, 1:4])

# Other useful functions for regression review
png(filename="out/regression-vw5-v3.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(vw5.v3)
dev.off()


############### Eliminate outliers to improve the model - on vw5.v3
## outlier test - Bonferroni error - within car package

outlierTest(vw5.v3, n.max=10)
#outlier.bonferroni.v3 <-  as.data.frame(outlierTest(vw5.v3, n.max=10000)$coefficients[, 1:4])

#Leverage & Hat-values outliers: Hat-points identify influential observations (have a high impact on the predictor variables)
outlier.avghat.v3 <- as.data.frame(unname(which(hatvalues(vw5.v3)>2* (length(coef(vw5.v3)) / nrow(vw5.v3$model)) )))

# Cooks Distance outliers: Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizided residuals are the residuals divided by their estimated standard deviation as a way to standardized
# Bonferroni test to identify outliers
# NOTE: If an observation is an outlier and influential (high leverage) then that observation can change the fit 
# of the linear model, it is advisable to remove it. 

outlier.cookdistance.v3 <- as.data.frame(unname(which(cooks.distance(vw5.v3) > 4 /vw5.v3$df.residual)))

# Subsetting outlier from the dataset
#hve.model3.outlier <- hve.model3[-c(4247,2444,4167,823,616,2716,4210,733),]

vw5.v3.out <- lm( Expenditure.Per.Capita ~
                    Debt.To.Expenditure + ## This variable is changing
                    House.Crowding +
                    House.Crowding.Squared +
                    Income.Per.Capita +
                    Income.Per.Capita.Squared +
                    Family.Size +
                    Family.Size.Squared +
                    Spices.And.Condiments.Bought.With.Cash +
                    Rent.Occupancy,
                   data=hve.model3)
vw5.v3.out.summary.coeff <- as.data.frame(summary(vw5.v3.out)$coefficients[, 1:4])

write.csv(vw5.v3.out.summary.coeff, file="out/vw5v3outsummary.coeff.csv")



vw5.v3.coefficients <- coefficients(vw5.v3) # model coefficients
vw5.v3.confint <- confint(vw5.v3, level=0.95) # CIs for model parameters 
vw5.v3.fitted <- as.data.frame(fitted(vw5.v3)) # fitted values based on the model
vw5.v3.residuals <- as.data.frame(residuals(vw5.v3)) # residuals
vw5.v3.anova <- as.data.frame(anova(vw5.v3)) # anova table 
vw5.v3.vcov <- as.data.frame(vcov(vw5.v3)) # covariance matrix for model parameters 
vw5.v3.influence <- as.data.frame( influence(vw5.v3)) # regression diagnostics

### Generate predicted welfare index based on variables of the model generated through v4 dataset
# http://connectmv.com/tutorials/r-tutorial/extracting-information-from-a-linear-model-in-r/

hve$predictedwellfare.vw5.v3  <- predict(vw5.v3, newdata=hve) 

### Generate predicted welfare index based on variables of the model generated through v3 dataset
hve$predictedwellfare.vw5.v3a <- vw5.v3.out.summary.coeff[1,1] +  
  ( hve$Debt.To.Expenditure * vw5.v3.out.summary.coeff[2,1]) +
  ( hve$House.Crowding * vw5.v3.out.summary.coeff[3,1]) +
  ( hve$House.Crowding.Squared * vw5.v3.out.summary.coeff[4,1]) +
  ( hve$Income.Per.Capita * vw5.v3.out.summary.coeff[5,1]) +
  ( hve$Income.Per.Capita.Squared * vw5.v3.out.summary.coeff[6,1]) +
  ( hve$Family.Size * vw5.v3.out.summary.coeff[7,1]) +
  ( hve$Family.Size.Squared * vw5.v3.out.summary.coeff[8,1]) +
  ( hve$Spices.And.Condiments.Bought.With.Cash * vw5.v3.out.summary.coeff[9,1]) +
  ( hve$Rent.Occupancy* vw5.v3.out.summary.coeff[10,1])

# summary(hve$predictedwellfare.vw5.v3)

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$predictedwellfare.vw5.v3.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v3, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
hve$predictedwellfare.vw5.v3.class <- revalue(hve$predictedwellfare.vw5.v3.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$predictedwellfare.vw5.v3.class  <- factor(hve$predictedwellfare.vw5.v3.class, levels = c("Severe", "High", "Moderate", "Low"))
#View(hve$predictedwellfare.vw5.v3.class)


######################################################################################################
######################################################################################################
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
#summary(vw5vw5.v4)

vw5.v4.summary.coeff <- as.data.frame(summary(vw5.v4)$coefficients[, 1:4])


# Other useful functions for regression review
# Other useful functions for regression review
png(filename="out/regression-vw5-v4.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(vw5.v4)
dev.off()



#write.csv(vw5.v4.out.summary.coeff, file="out/vw5v4outsummary.coeff.csv")

############### Eliminate outliers to improve the model - on vw5.v4
## outlier test - Bonferroni error - within car package
#outlierTest(vw5.v4, n.max=10000)
#outlier.bonferroni.v4 <-  as.data.frame(outlierTest(vw5.v4, n.max=10000)$coefficients[, 1:4])
#Leverage & Hat-values outliers:
#outlier.avghat.v4 <- as.data.frame(unname(which(hatvalues(vw5.v4)>2* (length(coef(vw5.v4)) / nrow(vw5.v4$model)) )))
#Cooks Distance outliers:
#outlier.cookdistance.v4 <- as.data.frame(unname(which(cooks.distance(vw5.v4)>4/vw5.v4$df.residual)))

# Subsetting outlier from the dataset
#hve.model4.outlier <- hve.model4[-c(4247,2444,4167,823,616,2716,4210,733),]

vw5.v4.out <- lm( Expenditure.Per.Capita ~
                    Debt.To.Expenditure + ## This variable is changing
                    House.Crowding +
                    House.Crowding.Squared +
                    Income.Per.Capita +
                    Income.Per.Capita.Squared +
                    Family.Size +
                    Family.Size.Squared +
                    Spices.And.Condiments.Bought.With.Cash +
                    Rent.Occupancy,
                  data=hve.model4)
vw5.v4.out.summary.coeff <- as.data.frame(summary(vw5.v4.out)$coefficients[, 1:4])


vw5.v4.coefficients <- coefficients(vw5.v4) # model coefficients
vw5.v4.confint <- confint(vw5.v4, level=0.95) # CIs for model parameters 
vw5.v4.fitted <- as.data.frame(fitted(vw5.v4)) # predicted values
vw5.v4.residuals <- as.data.frame(residuals(vw5.v4)) # residuals
vw5.v4.anova <- as.data.frame(anova(vw5.v4)) # anova table 
vw5.v4.vcov <- as.data.frame(vcov(vw5.v4)) # covariance matrix for model parameters 
vw5.v4.influence <- as.data.frame( influence(vw5.v4)) # regression diagnostics


### Generate predicted welfare index based on variables of the model generated through v4 dataset
hve$predictedwellfare.vw5.v4  <- predict(vw5.v4, newdata=hve) 


### Generate predicted welfare index based on variables of the model generated through v4 dataset
hve$predictedwellfare.vw5.v4b <- vw5.v4.out.summary.coeff[1,1] +  
  ( hve$Debt.To.Expenditure * vw5.v4.out.summary.coeff[2,1]) +
  ( hve$House.Crowding * vw5.v4.out.summary.coeff[3,1]) +
  ( hve$House.Crowding.Squared * vw5.v4.out.summary.coeff[4,1]) +
  ( hve$Income.Per.Capita * vw5.v4.out.summary.coeff[5,1]) +
  ( hve$Income.Per.Capita.Squared * vw5.v4.out.summary.coeff[6,1]) +
  ( hve$Family.Size * vw5.v4.out.summary.coeff[7,1]) +
  ( hve$Family.Size.Squared * vw5.v4.out.summary.coeff[8,1]) +
  ( hve$Spices.And.Condiments.Bought.With.Cash * vw5.v4.out.summary.coeff[9,1]) +
  ( hve$Rent.Occupancy* vw5.v4.out.summary.coeff[10,1])

#summary(hve$predictedwellfare.vw5.v4)

#View(hve$predictedwellfare.vw5.v3)
## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$predictedwellfare.vw5.v4.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v4 , n = 4, style = "fixed", fixedBreaks = c(-170, 28, 68 , 100, 5000))))
hve$predictedwellfare.vw5.v4.class <- revalue(hve$predictedwellfare.vw5.v4.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$predictedwellfare.vw5.v4.class  <- factor(hve$predictedwellfare.vw5.v4.class, levels = c("Severe", "High", "Moderate", "Low"))


#names(hve)

###############################
################################################################################################
# Welfare Models: Revised versions - May 2015
## Those revisions include the addition of variable from registration
#### The new approach is also to predict the logarythmic expenditure per capita


# Welfare Model: Home visit + PG Registration variables: 57 Variables
## This includes all variables that can increase the R squarred

reg.full <- lm(ln.exppc ~
                 
                 ### Progres - Registration variables in the model
                 p.child.grp2 + 
                 p.child.grp3 +
                 p.child.grp4 +
                 edu.highest.grp5 +
                 edu.highest.grp4 +
                 edu.highest.grp3 +
                 edu.highest.grp2 +
                 occup.grp2 +
                 occup.grp3 +
                 occup.grp4 +
                 occup.grp5 +
                 gender.male +
                 mar_single +
                 mar_g_married +
                 mar_g_divorced +
                 rel_sunni +
                 bir_syria +
                 dem_PA_grp1 +
                 dem_PA_grp2 +
                 arr_illegal + +
                 arr.crosspoint.grp2 +
                 arr.crosspoint.grp3 +
                 arr.crosspoint.grp4 +
                 arr.crosspoint.grp5 +
                 
                 ### Home Visit - VAF variables in the model
                 enumerator.judgement.notvulnerable +
                 case.size.vaf.2 +
                 case.size.vaf.3 +
                 case.size.vaf.4 +
                 case.size.vaf.5 +
                 case.size.vaf.6 +
                 case.size.vaf.7 +
                 case.size.vaf.8plus +                 
                 hh.crowding +
                 sp.co.cash +
                 house.kitchen.d +
                 wash.wastewater.sewage +
                 wash.wastewater.regular.removal +
                 food.meat.wfp.assistance +
                 food.dairy.wfp.assistance +
                 edu.n.attending.school +
                 edu.public.school +
                 vaccination.measles.NA +
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
                 all.case.size +
                 all.case.size.sq,
               data=hve)

# for WB full
reg.full <- lm(ln.exppc ~ 
                 ### Progres - Registration variables in the model
                 p.child.grp2 +
                 p.child.grp3 +
                 p.child.grp4 +
                 hh.crowding +
                 edu.highest.grp5 +
                 edu.highest.grp4 +
                 edu.highest.grp3 +
                 edu.highest.grp2 +
                 occup.grp2 +
                 occup.grp3 +
                 occup.grp4 +
                 occup.grp5 +
                 gender.male +
                 mar_single +
                 mar_widow +
                 mar_g_divorced +
                 age.PA2 +
                 age.PA3 +
                 arr_legal +
                 rel_sunni +
                 bir_syria +
                 arr.crosspoint.grp2 +
                 arr.crosspoint.grp3 +
                 arr.crosspoint.grp4 +
                 arr.crosspoint.grp5 +
                 
                 ### Home Visit - VAF variables in the model  
                 enumerator.judgement.notvulnerable +               
                 case.size.vaf.2 +
                 case.size.vaf.3 +
                 case.size.vaf.4 +
                 case.size.vaf.5 +
                 case.size.vaf.6 +
                 case.size.vaf.7 +
                 case.size.vaf.8.11 +
                 case.size.vaf.12plus +
                 house.kitchen.d +  
                 sp.co.cash +
                 wash.wastewater.sewage +
                 wash.wastewater.regular.removal +
                 food.meat.wfp.assistance +
                 food.dairy.wfp.assistance +
                 edu.n.attending.school +
                 edu.public.school +
                 vaccination.measles.NA +
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
                 all.case.size +
                 all.case.size.sq,
               
               data=hve)

#################
# Welfare Model (reduced): HV & PG most important variables: 
reg.reduced <- lm(ln.exppc ~
                    
                    ### Progres - Registration variables in the model
                    p.child.grp2 +
                    p.child.grp3 +
                    p.child.grp4 +
                    hh.crowding +
                    occup.grp2 +
                    occup.grp3 +
                    occup.grp4 +
                    occup.grp5 +
                    gender.male +
                    mar_single +
                    mar_g_married +
                    mar_g_divorced +
                    arr_illegal +
                                        
                    ### Home Visit - VAF variables in the model 
                    enumerator.judgement.notvulnerable +
                    case.size.vaf.2 +
                    case.size.vaf.3 +
                    case.size.vaf.4 +
                    case.size.vaf.5 +
                    case.size.vaf.6 +
                    case.size.vaf.7 +
                    case.size.vaf.8plus +
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
                    all.case.size +
                    all.case.size.sq, 
                  data=hve)

# for WB reduced
reg.reduced2 <- lm(ln.exppc ~
                    
                    ### Progres - Registration variables in the model
                    p.child.grp2 +
                    p.child.grp3 +
                    p.child.grp4 +
                    hh.crowding +
                    occup.grp2 +
                    occup.grp3 +
                    occup.grp4 +
                    occup.grp5 +
                    gender.male +
                    mar_single +
                    mar_widow +
                    mar_g_divorced +
                    arr_legal +
                                        
                    ### Home Visit - VAF variables in the model 
                    enumerator.judgement.notvulnerable +
                    case.size.vaf.2 +
                    case.size.vaf.3 +
                    case.size.vaf.4 +
                    case.size.vaf.5 +
                    case.size.vaf.6 +
                    case.size.vaf.7 +
                    case.size.vaf.8.11 +
                    case.size.vaf.12plus +
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
                    all.case.size +
                    all.case.size.sq, 
                  
                  data=hve)




