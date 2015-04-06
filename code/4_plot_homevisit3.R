
##########################################################################
#### Import data & Rename variable
#source("code/2_Welfare-model.R")

require(ggplot2)


#######################################################################
######### A few plot to visualise the results

## Expenditure per capita variable

boxplot.expenditurecapita <- ggplot(hve, aes(x=dataset, y=Expenditure.Per.Capita, fill=dataset)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ggtitle("Boxplot: Comparison of expenditure per capita")
ggsave("out/boxplot-expenditurecapita.png", boxplot.expenditurecapita, width=8, height=6,units="in", dpi=300)

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





###############################################################
### Graph example with gplot2
##############################################################

### Plotting classe

v3.class.plot <- ggplot(data=hve, aes(x= predictedwellfare.vw5.v3.class , y=Household.information.Family.Size)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
 # geom_text(aes(label= predictedwellfare.vw5.v3.class), vjust=0) +
  guides(fill=FALSE) + 
  coord_flip()+
  xlab("Class") + 
  ylab("# of Ind in Household") +
  ggtitle("Vulnerability Classification")
# Save this!
ggsave("out/v3-class-plot.png", v3.class.plot, width=8, height=6,units="in", dpi=300)


v4.class.plot <- ggplot(data=hve, aes(x= predictedwellfare.vw5.v4.class , y=Household.information.Family.Size)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label= predictedwellfare.vw5.v3.class), vjust=0) +
  guides(fill=FALSE) + 
  coord_flip()+
  xlab("Class") + 
  ylab("# of Ind in Household") +
  ggtitle("Vulnerability Classification")
# Save this!
ggsave("out/v4-class-plot.png", v4.class.plot, width=8, height=6,units="in", dpi=300)


###############################################################
# Reorgnise the matrix in order to compute pivot tables
# Tuto: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/

require("reshape2")


####### Let's graph Assets

# extract on asset per family size
homevisit3.assets <- melt(homevisit3, id=c("Household.information.Family.Size"),
                          measure=c("Type.of.Housing.Assets...Floor.mattress",
                                    "Type.of.Housing.Assets...Sofa.set",
                                    "Type.of.Housing.Assets...Kitchen.utilities",
                                    "Type.of.Housing.Assets...Computer",
                                    "Type.of.Housing.Assets...Blankets",
                                    "Type.of.Housing.Assets...Stove",
                                    "Type.of.Housing.Assets...Washing.machine",
                                    "Type.of.Housing.Assets...Table-chairs",
                                    "Type.of.Housing.Assets...Cabinets",
                                    "Type.of.Housing.Assets...Fridge",
                                    "Type.of.Housing.Assets...Television",
                                    "Type.of.Housing.Assets...Water.heater",
                                    "Type.of.Housing.Assets...Freezer",
                                    "Type.of.Housing.Assets...Other..specify"))


## Reorder factor level according to a value


homevisit3.assets <- homevisit3.assets[order(-homevisit3.assets$value), ]

#homevisit3.assets$variable <- as.character(homevisit3.assets$variable)

#str(homevisit3.assets)

#homevisit3.assets$variable <- factor(homevisit3.assets$variable , levels = homevisit3.assets[order(homevisit3.assets$value), 1])

levels(homevisit3.assets$variable)

#
homevisit3.assets2 <- dcast(homevisit3.assets, Household.information.Family.Size ~ variable)



# Bar graph based on reordered variable per value
asset.plot <- ggplot(data=homevisit3.assets, aes(x=reorder(variable, value) , y=value, fill=variable)) + 
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
assetplotfamily <- ggplot(data=homevisit3.assets, aes(x=Household.information.Family.Size , y=value, fill=variable))+
  geom_bar(stat="identity")+
  labs(x = "", y = "")

# Save this!
ggsave("out/assetplotfamily.png", assetplotfamily, width=8, height=2,units="in", dpi=300, bg = "transparent")
