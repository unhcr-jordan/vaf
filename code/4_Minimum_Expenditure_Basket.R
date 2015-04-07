##########################################################################
### Minimum expenditure Basket exploration

##########################################################################
#### Import data & Rename variable
source("code/1_composite_variable_homevisit.R")

require(ggplot2)

## Filter data by date (01/09/2014 - 28/02/2015)
df_meb = subset(hve, as.Date(hve$HV_Date)>='2014-09-01' & 
                  as.Date(hve$HV_Date)<='2015-02-28')

## using only complete records
df_meb<-df_meb[complete.cases(df_meb$Exp_Rent_portion),]
df_meb<-df_meb[complete.cases(df_meb$Exp_Utilities),]
df_meb<-df_meb[complete.cases(df_meb$Exp_Food_ex_WFP),]
df_meb<-df_meb[complete.cases(df_meb$Exp_WASH),]
df_meb<-df_meb[complete.cases(df_meb$Exp_MED),]
df_meb<-df_meb[complete.cases(df_meb$Exp_EDU),]
df_meb<-df_meb[complete.cases(df_meb$Exp_transport),]
df_meb<-df_meb[complete.cases(df_meb$Exp_infant_needs),]
df_meb<-df_meb[complete.cases(df_meb$Exp_basic_needs),]
df_meb<-df_meb[complete.cases(df_meb$Exp_debt),]
df_meb<-df_meb[complete.cases(df_meb$Exp_other),]
df_meb<-df_meb[complete.cases(df_meb$Exp_total),]

#summary(df_meb)


##########################################################################
## visualize MEB data
## Boxplot and violin plot giving distribution of expenditure type by case size
## TO DO STILL: Use FOR loop to plot all variations of plots 

### Per Family Size
plot1_rent <- ggplot(df_meb, aes( x = case.size.vaf, y = Exp_Rent_portion)) +
                      geom_violin() + 
                      facet_wrap(~ case.size.vaf, nrow=1)
ggsave("out/meb/plot1_rent.png", plot1_rent, width=8, height=6,units="in", dpi=300)


### Per Family Size
rm(boxplot.rent.familysize)
boxplot.rent.familysize <- ggplot(df_meb, aes( x = case.size.vaf, y = Exp_Rent_portion)) +
                      geom_boxplot() +
                      facet_wrap(~ case.size.vaf, nrow=1)+
                      ggtitle("Boxplot: Comparison of Rent per Case size")
ggsave("out/meb/boxplot_rent_familysize.png", boxplot.rent.familysize, width=8, height=6,units="in", dpi=300)
rm(boxplot.rent.familysize)

### Per Gov
boxplot.expenditurerent.gov <- ggplot(hve, aes(x=Gov_NAME, y=Exp_Rent_portion, fill=Gov_NAME)) +
                                      geom_boxplot() +
                                      guides(fill=FALSE) +
                                      ggtitle("Boxplot: Comparison of Rent per Governorate")
ggsave("out/meb/boxplot_expenditurerent_gov.png", boxplot.expenditurerent.gov, width=8, height=6,units="in", dpi=300)

## A quick map plotting average rent 
## Calling map script to get background
source("4_map_homevisit3.R")

## now starting the map
rm(map.average.rent)
map.average.rent <- googleeroad
map.average.rent <- map.average.rent +
  stat_summary_hex(aes(x= long,  y= lat, z = Exp_Rent_portion), 
                   data=hve ,
                   fun = mean,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Average Rent") +
  ggtitle("Home Visit Analysis ")
ggsave("out/meb/map-average_rent.png", map.average.rent, width=8, height=6,units="in", dpi=300)
rm(map.average.rent)

## Zooming in the north 
rm(map.average.rent.n)
map.average.rent.n <- northeroad
map.average.rent.n <- map.average.rent.n +
  stat_summary_hex(aes(x= long,  y= lat, z = Exp_Rent_portion), 
                   data=hve ,
                   fun = mean,
                   bins=50,
                   alpha = 9/10) +
  theme_bw() + 
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444") +
  labs(x = "Longitude", y = "Latitude", fill = "Average Rent") +
  ggtitle("Home Visit Analysis ")
ggsave("out/meb/map-average_rent_north.png", map.average.rent.n, width=8, height=6,units="in", dpi=300)
rm(map.average.rent.n)
