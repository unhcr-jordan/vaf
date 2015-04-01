
##########################################################################
#### Import data & Rename variable
source("code/2_Welfare-model.R")




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
