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
summary(vw5.v3)
vw5.v3.summary.coeff <- as.data.frame(summary(vw5.v3)$coefficients[, 1:4])


############### Eliminate outliers to improve the model - on vw5.v3
## outlier test - Bonferroni error - within car package
outlierTest(vw5.v3, n.max=10000)
#outlier.bonferroni.v4 <-  as.data.frame(outlierTest(vw5.v3, n.max=10000)$coefficients[, 1:4])
#Leverage & Hat-values outliers:
outlier.avghat.v3 <- as.data.frame(unname(which(hatvalues(vw5.v3)>2* (length(coef(vw5.v3)) / nrow(vw5.v4$model)) )))
#Cooks Distance outliers:
outlier.cookdistance.v3 <- as.data.frame(unname(which(cooks.distance(vw5.v3) >4 /vw5.v3$df.residual)))

# Subsetting outlier from the dataset
hve.model3.outlier <- hve.model3[-c(4247,2444,4167,823,616,2716,4210,733),]

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

### Generate predicted welfare index based on variables of the model generated through v3 dataset
hve$predictedwellfare.vw5.v3 <- ( hve$Debt.To.Expenditure * vw5.v3.out.summary.coeff[2,1]) +
  ( hve$House.Crowding * vw5.v3.out.summary.coeff[3,1]) +
  ( hve$House.Crowding.Squared * vw5.v3.out.summary.coeff[4,1]) +
  ( hve$Income.Per.Capita * vw5.v3.out.summary.coeff[5,1]) +
  ( hve$Income.Per.Capita.Squared * vw5.v3.out.summary.coeff[6,1]) +
  ( hve$Family.Size * vw5.v3.out.summary.coeff[7,1]) +
  ( hve$Family.Size.Squared * vw5.v3.out.summary.coeff[8,1]) +
  ( hve$Spices.And.Condiments.Bought.With.Cash * vw5.v3.out.summary.coeff[9,1]) +
  ( hve$Rent.Occupancy* vw5.v3.out.summary.coeff[10,1])

#summary(hve$predictedwellfare.vw5.v3)


## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$predictedwellfare.vw5.v3.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v3, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1000))))
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

############### Eliminate outliers to improve the model - on vw5.v4
## outlier test - Bonferroni error - within car package
outlierTest(vw5.v4, n.max=10000)
#outlier.bonferroni.v4 <-  as.data.frame(outlierTest(vw5.v4, n.max=10000)$coefficients[, 1:4])
#Leverage & Hat-values outliers:
outlier.avghat.v4 <- as.data.frame(unname(which(hatvalues(vw5.v4)>2* (length(coef(vw5.v4)) / nrow(vw5.v4$model)) )))
#Cooks Distance outliers:
outlier.cookdistance.v4 <- as.data.frame(unname(which(cooks.distance(vw5.v4)>4/vw5.v4$df.residual)))

# Subsetting outlier from the dataset
hve.model4.outlier <- hve.model4[-c(4247,2444,4167,823,616,2716,4210,733),]

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


### Generate predicted welfare index based on variables of the model generated through v4 dataset
hve$predictedwellfare.vw5.v4 <- ( hve$Debt.To.Expenditure * vw5.v4.out.summary.coeff[2,1]) +
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
hve$predictedwellfare.vw5.v4.class <- as.factor(findCols(classIntervals(hve$predictedwellfare.vw5.v4 , n = 4, style = "fixed", fixedBreaks = c(-10000, 28, 68 , 100))))
hve$predictedwellfare.vw5.v4.class <- revalue(hve$predictedwellfare.vw5.v4.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$predictedwellfare.vw5.v4.class  <- factor(hve$predictedwellfare.vw5.v4.class, levels = c("Severe", "High", "Moderate", "Low"))


#######################################################################
######### A few plot to visualise the results

## Let's check the expenditure per capita variable in this 2 sets witha box plot

boxplot.expenditurecapita <- ggplot(hve, aes(x=dataset, y=Expenditure.Per.Capita, fill=dataset)) +
                                      geom_boxplot() +
                                      guides(fill=FALSE) +
                                      ggtitle("Boxplot: Comparison of expenditure per capita")
ggsave("out/boxplot-expenditurecapita.png", boxplot.expenditurecapita, width=8, height=6,units="in", dpi=300)

summary(hve$Expenditure.Per.Capita)
# Histogram overlaid with Expenditure.Per.Capita
histo.expenditurecapita <- ggplot(hve, aes(x=hve$Expenditure.Per.Capita)) + 
                                      geom_histogram(aes(y =..density..), 
                                                     breaks=c(1, 28, 68 , 100, 1000), + xlim(0, 250),
                                                     #binwidth=.5, 
                                                     colour="dark blue", fill="light blue", alpha = .2) +
                                      geom_density(col =2, alpha=.2, fill="#FF6666") + 
                                      geom_vline(aes(xintercept=mean(Expenditure.Per.Capita, na.rm=T)), color="red", linetype="dashed", size=1) + 
                                      facet_grid(dataset ~ .) +
                                      labs(x="Expenditure per capita", y="Count of cases")  +
                                      ggtitle("Histogramm for Expenditure per capita")
ggsave("out/histogram-expenditurecapita.png", histo.expenditurecapita, width=8, height=6,units="in", dpi=300)

# Histogram for prediucted welfare on v3
ggplot(hve, aes(x=predictedwellfare.vw5.v3)) + 
  geom_histogram(aes(x=predictedwellfare.vw5.v3),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="light blue", fill="dark blue") +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(predictedwellfare.vw5.v3, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)



##################
hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue",
     main = "Histogram of Welfare Model -vw5- estimated on V4 dataset", xlab = "Expected welfare Score ")


hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue",
     main = "Histogram of Welfare Model -vw5- estimated on V3 dataset", xlab = "Expected welfare Score ")