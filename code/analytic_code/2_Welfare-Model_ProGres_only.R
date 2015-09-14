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

hve <- read.csv("out/hve.csv")

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
               mar_single+
               mar_g_married+
               mar_g_divorced +
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

reg.pg.h4 <- lm(ln.exppc.pg ~
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
               mar_single+
               mar_g_married+
               mar_g_divorced +
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
             
             data=hve[hve$dataset=="homevisit4",])

reg.pg.h3 <- lm(ln.exppc.pg ~
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
                  mar_single+
                  mar_g_married+
                  mar_g_divorced +
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
                
                data=hve[hve$dataset=="homevisit3",])

reg.pg.aug <- lm(ln.exppc.pg ~
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
                  mar_single+
                  mar_g_married+
                  mar_g_divorced +
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
                
                data=hve[hve$HV_Date > "2015-08-01",])


## Get summary -  for interpretation look at http://blog.yhathq.com/posts/r-lm-summary.html
summary(reg.pg)
reg.pg.summary.coeff <- as.data.frame(summary(reg.pg)$coefficients[, 1:4])

# Other useful functions for regression review
png(filename="out/regression-reg.pg.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(reg.pg)
dev.off()


png(filename="out/regression-reg.pgh3.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(reg.pg.h3)
dev.off()

png(filename="out/regression-reg.pgh4.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(reg.pg.h4)
dev.off()

png(filename="out/regression-reg.pgaug.png")
layout(matrix(c(1,2,3,4),2,2))  
plot(reg.pg.aug)
dev.off()


## We can now predict expenditure per capita on the whole proGres dataset
### Generate predicted welfare index based on variables of the model generated through v4 dataset
# http://connectmv.com/tutorials/r-tutorial/extracting-information-from-a-linear-model-in-r/

progres.case$ln.predictedwellfare  <- predict(reg.pg, newdata=progres.case) 

progres.case$predictedwellfare <- exp(progres.case$ln.predictedwellfare)
#summary(progres.case$predictedwellfare)

progres.case$predictedwellfare.h3 <- exp(predict(reg.pg.h3, newdata=progres.case) )
progres.case$predictedwellfare.h4 <- exp(predict(reg.pg.h4, newdata=progres.case) )
progres.case$predictedwellfare.aug <- exp(predict(reg.pg.aug, newdata=progres.case) )

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
progres.case$predictedwellfare.class <- as.factor(findCols(classIntervals(progres.case$predictedwellfare, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class <- revalue(progres.case$predictedwellfare.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class  <- factor(progres.case$predictedwellfare.class, levels = c("Severe", "High", "Moderate", "Low"))
#View(hve$predictedwellfare.vw5.v3.class)

progres.case$predictedwellfare.class.h3 <- as.factor(findCols(classIntervals(progres.case$predictedwellfare.h3, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class.h3 <- revalue(progres.case$predictedwellfare.class.h3, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class.h3  <- factor(progres.case$predictedwellfare.class.h3, levels = c("Severe", "High", "Moderate", "Low"))

progres.case$predictedwellfare.class.h4 <- as.factor(findCols(classIntervals(progres.case$predictedwellfare.h4, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class.h4 <- revalue(progres.case$predictedwellfare.class.h4, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class.h4  <- factor(progres.case$predictedwellfare.class.h4, levels = c("Severe", "High", "Moderate", "Low"))

progres.case$predictedwellfare.class.aug <- as.factor(findCols(classIntervals(progres.case$predictedwellfare.aug, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class.aug <- revalue(progres.case$predictedwellfare.class.aug, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class.aug  <- factor(progres.case$predictedwellfare.class.aug, levels = c("Severe", "High", "Moderate", "Low"))


### Let merge with cas beneficiaries list
cash <- read.csv("data/cash14092015.csv", header=TRUE)

## Check duplicate in that list...
uniqueFilenumber <- as.data.frame(unique(cash$File.Number))
names(uniqueFilenumber) <- "File.Number"
## Keeping only unique one
cash2 <- cash[!duplicated(cash$File.Number), ]

progres.case <- merge(x=progres.case, y=cash2, by.x="ProcessingGroupNumber", by.y="File.Number", all.x=TRUE)
#names(progres.case)


### Let's check how many case were visited through home visit
## Keeping only home visit unique to case
names(hve)
hve.unique <- hve[!duplicated(hve$Household.information.UNHCR.File.Number), ]

progres.case <- merge(x=progres.case, y=hve.unique, by.x="ProcessingGroupNumber", by.y="Household.information.UNHCR.File.Number", all.x=TRUE)
# names(progres.case)
# names(homevisit)
# View(progres.case$ProcessingGroupNumber)
# View(homevisit$Household.information.UNHCR.File.Number)

## Addd a tag for visited or not
##progres.case.hve$visited <- with(progres.case.hve, ifelse(is.na(progres.case.hve$dataset)), paste0("Not visited"), paste0("Visited")))

progres.case$visited <- "Not visited"
progres.case$visited <- with(progres.case,
                                 ifelse( ( progres.case$dataset == "homevisit3"),
                                         paste0("homevisit3"), progres.case$visited ))
progres.case$visited <- with(progres.case,
                                 ifelse( ( progres.case$dataset == "homevisit4"),
                                         paste0("homevisit4"), progres.case$visited ))



write.csv(progres.case, file = "out/progres-only/progrescase-with-prediction.csv",na="")


########## Let's visualise the results

## See  code/presentation_code/4_plot_welfare-on-progres-only.R