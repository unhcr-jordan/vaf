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

progres.case$ln.predictedwellfare  <- predict(reg.pg, newdata=progres.case) 

progres.case$predictedwellfare <- exp(progres.case$ln.predictedwellfare)

summary(progres.case$predictedwellfare)

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
progres.case$predictedwellfare.class <- as.factor(findCols(classIntervals(progres.case$predictedwellfare, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1100))))
progres.case$predictedwellfare.class <- revalue(progres.case$predictedwellfare.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
progres.case$predictedwellfare.class  <- factor(progres.case$predictedwellfare.class, levels = c("Severe", "High", "Moderate", "Low"))
#View(hve$predictedwellfare.vw5.v3.class)



write.csv(progres.case, file = "out/progres-only/progrescase-with-prediction.csv",na="")


########## Let's visualise the results

## Expenditure per capita  - predicted
rm(boxplot.expenditurecapita)
boxplot.expenditurecapita <- ggplot(progres.case, aes(x=Num_Inds, y=predictedwellfare)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ggtitle("Boxplot: Comparison of prediucted expenditure per capita for proGres data")
ggsave("out/progres-only/boxplot-expenditurecapita-progres.png", boxplot.expenditurecapita, width=8, height=6,units="in", dpi=300)
rm(boxplot.expenditurecapita)



# Histogram overlaid with Expenditure.Per.Capita
rm(histo.expenditurecapita)
histo.expenditurecapita <- ggplot(progres.case, aes(x=progres.case$predictedwellfare)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(1, 28, 68 , 100, 1000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(predictedwellfare, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(admlevel1 ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for predicted Expenditure per capita for all progres cases between Governorates")
ggsave("out/progres-only/histogram-expenditurecapita-proGres.png", histo.expenditurecapita, width=8, height=6,units="in", dpi=300)
rm(histo.expenditurecapita)


## Doing the same but facetting on adm1_name  -- Governorates
rm(boxplot.expenditurecapita.gov)
boxplot.expenditurecapita.gov <- ggplot(progres.case, aes(x=admlevel1, y=predictedwellfare, fill=admlevel1)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ggtitle("Boxplot: Comparison of expenditure per capita")
ggsave("out/progres-only/boxplot-expenditurecapitagov-progres.png", boxplot.expenditurecapita.gov, width=8, height=6,units="in", dpi=300)
rm(boxplot.expenditurecapita.gov)

# Histogram overlaid with Expenditure.Per.Capita
rm(histo.expenditurecapita.gov)
histo.expenditurecapita.gov <- ggplot(hve, aes(x=progres.case$predictedwellfare)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(1, 28, 68 , 100, 1000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(predictedwellfare, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(adm1_name ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for Expenditure per capita per Governorate")
ggsave("out/progres-only/histogram-expenditurecapitagov-progres.png", histo.expenditurecapita.gov, width=8, height=6,units="in", dpi=300)
rm(histo.expenditurecapita.gov)

#### Bar graph to show repartition by class for expenditure per capita
rm(bar.Expenditure.Per.Capita.class)
bar.Expenditure.Per.Capita.class <- ggplot(data=progres.case, 
                                           aes(x=predictedwellfare.class , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("Expenditure.Per.Capita.class")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres.png", bar.Expenditure.Per.Capita.class, width=8, height=6,units="in", dpi=300)
rm(bar.Expenditure.Per.Capita.class)


### Let's check how many case were visited through home visit
progres.case.hve <- merge(x=progres.case, y=homevisit, by.x="ProcessingGroupNumber", by.y="Household.information.UNHCR.File.Number", all.x=TRUE)
# names(progres.case)
# names(homevisit)
# View(progres.case$ProcessingGroupNumber)
# View(homevisit$Household.information.UNHCR.File.Number)

## Addd a tag for visited or not
##progres.case.hve$visited <- with(progres.case.hve, ifelse(is.na(progres.case.hve$dataset)), paste0("Not visited"), paste0("Visited")))

progres.case.hve$visited <- "Not visited"
progres.case.hve$visited <- with(progres.case.hve,
                    ifelse( ( progres.case.hve$dataset == "homevisit3"),
                            paste0("homevisit3"), progres.case.hve$visited ))
progres.case.hve$visited <- with(progres.case.hve,
                                 ifelse( ( progres.case.hve$dataset == "homevisit4"),
                                         paste0("homevisit4"), progres.case.hve$visited ))

#### Bar graph to show repartition by class for expenditure per capita
rm(bar.Expenditure.Per.Capita.class.hve)
bar.Expenditure.Per.Capita.class.hve <- ggplot(data=progres.case.hve, 
                                           aes(x=predictedwellfare.class , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  facet_grid(visited ~ .) +
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("Expenditure.Per.Capita.class")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres-visited.png", bar.Expenditure.Per.Capita.class.hve, width=8, height=6,units="in", dpi=300)
rm(bar.Expenditure.Per.Capita.class.hve)
