##########################################################################
#### Import data & Rename variable
#source("code/processing_code/1_composite_variable_homevisit.R")

#source("code/processing_code/0_import_collapse_merge_progres_data.R")
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


####################################
# Welfare Model: PG Registration variables only:
reg.pg <- lm(ln.exppc.pg ~
               gender.male +
               arr_illegal +
               case.size.pg.2 +
               case.size.pg.3 +
               case.size.pg.4 +
               case.size.pg.5 +
               case.size.pg.6 +
               case.size.pg.7 +
               case.size.pg.8plus +
               dem_PA_grp2 +
               dem_PA_grp3 +
               p.child.grp2 +
               p.child.grp3 +
               p.child.grp4 +
               mar.grp1 +
               mar.grp2 +
               mar.grp3 +
               rel_sunni +
               bir_syria +
               edu.highest.grp2 +
               edu.highest.grp3 +
               edu.highest.grp4 +
               edu.highest.grp5 +
               arr.crosspoint.grp2 +
               arr.crosspoint.grp3 +
               arr.crosspoint.grp4 +
               arr.crosspoint.grp5 +
               ajloun.pg +
               amman.pg +
               aqabah.pg +
               balqa.pg +
               jarash.pg +
               karak.pg +
               irbid.pg +
               maan.pg +
               mafraq.pg +
               tafilah.pg +
               zarqa.pg, 
             
             data=hve)

## Get summary -  for interpretation look at http://blog.yhathq.com/posts/r-lm-summary.html
summary(reg.pg)
reg.pg.summary.coeff <- as.data.frame(summary(reg.pg)$coefficients[, 1:4])

# Other useful functions for regression review
png(filename="out/regression-reg.pg.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(reg.pg)
dev.off()

## We can now predict expenditure per capita on the whole proGres dataset
### Generate predicted welfare index based on variables of the model generated through v4 dataset
# http://connectmv.com/tutorials/r-tutorial/extracting-information-from-a-linear-model-in-r/

progres.case$predictedwellfare  <- predict(reg.pg, newdata=progres.case) 

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
progres.case$predictedwellfare.class <- as.factor(findCols(classIntervals(progres.case$predictedwellfare, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class <- revalue(progres.case$predictedwellfare.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class  <- factor(progres.case$predictedwellfare.class, levels = c("Severe", "High", "Moderate", "Low"))
#View(hve$predictedwellfare.vw5.v3.class)
