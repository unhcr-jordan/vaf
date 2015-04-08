
##########################################################################
#### Import data & Rename variable
#source("code/2_Welfare-model.R")

require(ggplot2)

#names(hve)

#######################################################################
######### A few plot to visualise the results

## Expenditure per capita variable

boxplot.expenditurecapita <- ggplot(hve, aes(x=dataset, y=Expenditure.Per.Capita, fill=dataset)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ggtitle("Boxplot: Comparison of expenditure per capita")
ggsave("out/boxplot-expenditurecapita.png", boxplot.expenditurecapita, width=8, height=6,units="in", dpi=300)



boxplot.expenditurecapita.size <- ggplot(hve, aes(x=case.size.vaf, y=Expenditure.Per.Capita )) +
  geom_boxplot() + 
  ggtitle("Boxplot: Expenditure per capita vs case size") + 
  theme(axis.text.x=element_text(angle=90, size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("Case size")
ggsave("out/boxplot-expenditurecapita_size.png", boxplot.expenditurecapita.size, width=8, height=6,units="in", dpi=300)


scatterplot.expenditurecapita.size <-ggplot(hve, aes(x=case.size.vaf, y=Expenditure.Per.Capita)) +
  geom_point(size=3) + 
  facet_grid(dataset ~ .) +
  ggtitle("Scatterplot: Expenditure per capita vs case size") + 
  theme(axis.text.x=element_text(size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("Case size") + 
  stat_smooth(se=FALSE)
ggsave("out/scatterplot-expenditurecapita_size.png", scatterplot.expenditurecapita.size, width=8, height=6,units="in", dpi=300)


scatterplot.expenditurecapita.housecrowding <-ggplot(hve, aes(x=case.size.vaf, y=House.Crowding)) +
  geom_point(size=3) + 
  facet_grid(dataset ~ .) +
  ggtitle("Scatterplot: Expenditure per capita vs House.Crowding") + 
  theme(axis.text.x=element_text(size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("House.Crowding") + 
  stat_smooth(se=FALSE)
ggsave("out/scatterplot-expenditurecapita_housecrowding.png", scatterplot.expenditurecapita.housecrowding, width=8, height=6,units="in", dpi=300)

scatterplot.expenditurecapita.income <-ggplot(hve, aes(x=case.size.vaf, y=Income.Per.Capita)) +
  geom_point(size=3) + 
  facet_grid(dataset ~ .) +
  ggtitle("Scatterplot: Expenditure per capita vs Income.Per.Capita") + 
  theme(axis.text.x=element_text(size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("Income.Per.Capita") + 
  stat_smooth(se=FALSE)
ggsave("out/scatterplot-expenditurecapita_income.png", scatterplot.expenditurecapita.income, width=8, height=6,units="in", dpi=300)

scatterplot.expenditurecapita.spice <-ggplot(hve, aes(x=case.size.vaf, y=Spices.And.Condiments.Bought.With.Cash)) +
  geom_point(size=3) + 
  facet_grid(dataset ~ .) +
  ggtitle("Scatterplot: Expenditure per capita vs Spices.And.Condiments.Bought.With.Cash") + 
  theme(axis.text.x=element_text(size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("Spices.And.Condiments.Bought.With.Cash") + 
  stat_smooth(se=FALSE)
ggsave("out/scatterplot-expenditurecapita_spice.png", scatterplot.expenditurecapita.spice, width=8, height=6,units="in", dpi=300)


scatterplot.expenditurecapita.Rent.Occupancy <-ggplot(hve, aes(x=case.size.vaf, y=Rent.Occupancy)) +
  geom_point(size=3) + 
  facet_grid(dataset ~ .) +
  ggtitle("Scatterplot: Expenditure per capita vs Rent.Occupancy") + 
  theme(axis.text.x=element_text(size=14, vjust=0.5), 
        axis.text.y=element_text(size=14), 
        axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15), 
        plot.title=element_text(size=17)) + 
  scale_y_continuous( name="Expenditure.Per.Capita") +
  scale_x_continuous("Rent.Occupancy") + 
  stat_smooth(se=FALSE)
ggsave("out/scatterplot-expenditurecapita_Rent_Occupancy.png", scatterplot.expenditurecapita.Rent.Occupancy, width=8, height=6,units="in", dpi=300)


#summary(hve$Expenditure.Per.Capita)
# Histogram overlaid with Expenditure.Per.Capita
histo.expenditurecapita <- ggplot(hve, aes(x=hve$Expenditure.Per.Capita)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(1, 28, 68 , 100, 1000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(Expenditure.Per.Capita, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(dataset ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for Expenditure per capita between dataset")
ggsave("out/histogram-expenditurecapita.png", histo.expenditurecapita, width=8, height=6,units="in", dpi=300)


## Doing the same but facetting on Gov_NAME  -- Governorates
boxplot.expenditurecapita.gov <- ggplot(hve, aes(x=Gov_NAME, y=Expenditure.Per.Capita, fill=Gov_NAME)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ggtitle("Boxplot: Comparison of expenditure per capita")
ggsave("out/boxplot-expenditurecapitagov.png", boxplot.expenditurecapita.gov, width=8, height=6,units="in", dpi=300)
#summary(hve$Expenditure.Per.Capita)
# Histogram overlaid with Expenditure.Per.Capita
histo.expenditurecapita.gov <- ggplot(hve, aes(x=hve$Expenditure.Per.Capita)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(1, 28, 68 , 100, 1000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(Expenditure.Per.Capita, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(Gov_NAME ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for Expenditure per capita per Governorate")
ggsave("out/histogram-expenditurecapitagov.png", histo.expenditurecapita.gov, width=8, height=6,units="in", dpi=300)


#### Bar graph to show repartition by class for expenditure per capita
bar.Expenditure.Per.Capita.class <- ggplot(data=hve, 
                     aes(x=Expenditure.Per.Capita.class , y=case.size.vaf)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  coord_flip()+
  xlab("Class") + 
  ylab("# of Ind") +
  ggtitle("Expenditure.Per.Capita.class")
ggsave("out/barExpenditurePerCapitaclass.png", bar.Expenditure.Per.Capita.class, width=8, height=6,units="in", dpi=300)


###########################################
## Log Expenditure per capita variable

#summary(hve$Expenditure.Per.Capita)
# Histogram overlaid with Expenditure.Per.Capita
histo.ln.expenditurecapita <- ggplot(hve, aes(x=hve$ln.Expenditure.Per.Capita)) + 
  geom_histogram(aes(y =..density..), 
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(ln.Expenditure.Per.Capita, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(dataset ~ .) +
  labs(x="log Expenditure per capita")  +
  ggtitle("Histogramm for ln.Expenditure per capita between dataset")
ggsave("out/histogram-lnexpenditurecapita.png", histo.ln.expenditurecapita, width=8, height=6,units="in", dpi=300)


## Doing the same but facetting on Gov_NAME  -- Governorates
histo.ln.expenditurecapita.gov <- ggplot(hve, aes(x=hve$ln.Expenditure.Per.Capita)) + 
  geom_histogram(aes(y =..density..),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(ln.Expenditure.Per.Capita, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(Gov_NAME ~ .) +
  labs(x="log Expenditure per capita")  +
  ggtitle("Histogramm for ln.Expenditure per capita per Governorate")
ggsave("out/histogram-expenditurecapitagovln.png", histo.ln.expenditurecapita.gov, width=8, height=6,units="in", dpi=300)







#################################################
# Histogram for predicted expenditure per capita

testfit.vw5.v3 <- ggplot(hve, aes(x=predictedwellfare.vw5.v3, y=Expenditure.Per.Capita)) +
  geom_point(size=3) + 
  ggtitle("Expenditure per capita based by expenditure per capita based on v3") +
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        plot.title=element_text(size=17)) +
  scale_y_continuous(name="Expenditure.Per.Capita") +
  scale_x_continuous("predictedwellfare.vw5.v3") +
  geom_abline(intercept=0, slope=1, colour="yellow") +
  stat_smooth(se=FALSE)
ggsave("out/testfit_vw5_v3.png", testfit.vw5.v3, width=8, height=6,units="in", dpi=300)


testfit.vw5.v4 <- ggplot(hve, aes(x=predictedwellfare.vw5.v4, y=Expenditure.Per.Capita)) +
  geom_point(size=3) + 
  ggtitle("Expenditure per capita based by expenditure per capita based on v4") +
  theme(axis.text.x=element_text(size=14, vjust=0.5),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        plot.title=element_text(size=17)) +
  scale_y_continuous(name="Expenditure.Per.Capita") +
  scale_x_continuous("predictedwellfare.vw5.v4") +
  geom_abline(intercept=0, slope=1, colour="yellow") +
  stat_smooth(se=FALSE)
ggsave("out/testfit_vw5_v4.png", testfit.vw5.v4, width=8, height=6,units="in", dpi=300)



############ Histogramme for the models

summary(hve$predictedwellfare.vw5.v3)
histo.predictedwellfare.vw5.v3 <- ggplot(hve, aes(x=hve$predictedwellfare.vw5.v3)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(-170, 28, 68 , 100, 4000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(predictedwellfare.vw5.v3, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(dataset ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for Predicted expenditure per capita based on v3")
ggsave("out/histogram-predictedwellfarevw5v3.png", histo.predictedwellfare.vw5.v3, width=8, height=6,units="in", dpi=300)

summary(hve$predictedwellfare.vw5.v4)
histo.predictedwellfare.vw5.v4 <- ggplot(hve, aes(x=hve$predictedwellfare.vw5.v4)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=c(-105, 28, 68 , 100, 1000), 
                 #xlim(0, 250),
                 #binwidth=.5, 
                 colour="dark blue", fill="light blue", alpha = .2) +
  geom_density(col =2, alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(predictedwellfare.vw5.v4, na.rm=T)), color="red", linetype="dashed", size=1) + 
  facet_grid(dataset ~ .) +
  labs(x="Expenditure per capita", y="Count of cases")  +
  ggtitle("Histogramm for Predicted expenditure per capita based on v4")
ggsave("out/histogram-predictedwellfarevw5v4.png", histo.predictedwellfare.vw5.v4, width=8, height=6,units="in", dpi=300)


##################
#hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue", main = "Histogram of Welfare Model -vw5- estimated on V4 dataset", xlab = "Expected welfare Score ")
#hist(hve$predictedwellfare.vw5.v3, breaks=c(-105, 28, 68 , 100, 1000), border = "dark blue", col = "light blue", main = "Histogram of Welfare Model -vw5- estimated on V3 dataset", xlab = "Expected welfare Score ")


#### Bar graph to show repartition by class for expenditure per capita
bar.predictedwellfare.vw5.v4.class <- ggplot(data=hve, 
                                             aes(x=predictedwellfare.vw5.v4.class , y=case.size.vaf)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  #coord_flip()+
  xlab("Class") + 
  ylab("# of Ind") +
  ggtitle("predictedwellfare.vw5.v4.class")
ggsave("out/barpredictedwellfarevw5v4class.png", bar.predictedwellfare.vw5.v4.class, width=8, height=6,units="in", dpi=300)



bar.predictedwellfare.vw5.v3.class <- ggplot(data=hve, 
                                             aes(x=predictedwellfare.vw5.v3.class , y=case.size.vaf)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  #coord_flip()+
  xlab("Class") + 
  ylab("# of Ind") +
  ggtitle("predictedwellfare.vw5.v3.class")
ggsave("out/barpredictedwellfarevw5v3class.png", bar.predictedwellfare.vw5.v3.class, width=8, height=6,units="in", dpi=300)




###############################################################
# Reorgnise the matrix in order to compute pivot tables
# Tuto: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/

require("reshape2")


####### Let's graph Assets

# extract on asset per family size
hve.assets <- melt(hve, id=c("Household.information.Family.Size"),
                          measure=c("Type.of.Housing.Assets...Floor.mattress",
                                    "Type.of.Housing.Assets...Sofa.set",
                                    "Type.of.Housing.Assets...Kitchen.utilities",
                                    "Type.of.Housing.Assets...Computer",
                                    "Type.of.Housing.Assets...Blankets",
                                    "Type.of.Housing.Assets...Stove",
                                    "Type.of.Housing.Assets...Washing.machine",
                                  #  "Type.of.Housing.Assets...Table-chairs",
                                    "Type.of.Housing.Assets...Cabinets",
                                    "Type.of.Housing.Assets...Fridge",
                                    "Type.of.Housing.Assets...Television",
                                    "Type.of.Housing.Assets...Water.heater",
                                    "Type.of.Housing.Assets...Freezer"#,
                                  #  "Type.of.Housing.Assets...Other..specify"
                                  ))


## Reorder factor level according to a value


hve.assets <- hve.assets[order(- hve.assets$value), ]

#hve.assets$variable <- as.character(hve.assets$variable)

#str(hve.assets)

#hve.assets$variable <- factor(hve.assets$variable , levels = hve.assets[order(hve.assets$value), 1])

levels(hve.assets$variable)

#
hve.assets2 <- dcast(hve.assets, Household.information.Family.Size ~ variable)



# Bar graph based on reordered variable per value
asset.plot <- ggplot(data=hve.assets, aes(x=reorder(variable, value) , y=value, fill=variable)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  coord_flip()+
  xlab("Asset") + 
  ylab("Household") +
  ggtitle("Household assets")

# Save this!
ggsave("out/assetplot.png", asset.plot, width=8, height=6,units="in", dpi=300)

## simpler version
#assetplotfamily <- ggplot(data=hve.assets, aes(x=Household.information.Family.Size , y=value, fill=variable))+
#  geom_bar(stat="identity")+
#  labs(x = "", y = "")

# Save this!
#ggsave("out/assetplotfamily.png", assetplotfamily, width=8, height=2,units="in", dpi=300, bg = "transparent")
