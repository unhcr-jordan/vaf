##########################################################################
#### Import data & Rename variable
#source("code/2_Welfare-model.R")

require(ggplot2)

### Extract mapable data 

mapextract <- hve[ , c(
  "lat" ,                                                                                                                                                                                                                                                     
  "long" ,
  "adm1_name" ,                                                                                                                                                                                                                                              
  "adm1_nam"    ,                                                                                                                                                                                                                                            
  "adm1_code"    ,                                                                                                                                                                                                                                           
  "adm1_pro"      ,                                                                                                                                                                                                                                          
  "adm2_name"     ,                                                                                                                                                                                                                                          
  "adm2_nam"       ,                                                                                                                                                                                                                                         
  "adm2_code"     ,                                                                                                                                                                                                                                          
  "adm2_pro"       ,                                                                                                                                                                                                                                         
  "adm3_name"     ,                                                                                                                                                                                                                                          
  "adm3_nam"     ,                                                                                                                                                                                                                                           
  "adm3_code"    ,                                                                                                                                                                                                                                           
  "adm3_pro"     ,                                                                                                                                                                                                                                           
  "Pov_2008"    ,                                                                                                                                                                                                                                            
  "Pov_2006"    ,                                                                                                                                                                                                                                            
  "Pov_2002"     ,                                                                                                                                                                                                                                           
  "Pov_2010"   ,
  "case.size.vaf" ,
  "Coping.Strategies.Basic.Food.Needs" ,
  "Expenditure.Per.Capita" ,                                                                                                                                                                                                                                 
  "Expenditure.Per.Capita.class"  ,
  "predictedwellfare.vw5.v3" ,                                                                                                                                                                                                                               
  "predictedwellfare.vw5.v3a"  ,                                                                                                                                                                                                                             
  "predictedwellfare.vw5.v3.class"  ,                                                                                                                                                                                                                        
  "predictedwellfare.vw5.v4"   ,                                                                                                                                                                                                                             
  "predictedwellfare.vw5.v4b" ,                                                                                                                                                                                                                              
  "predictedwellfare.vw5.v4.class"   
)]

write.csv(mapextract, file="out/mapextract.csv")


######### Aggregate scoring at district level
hve.subdistrict.size <- aggregate(cbind(case.size.vaf) ~   adm3_pro , data = hve, FUN = sum, na.rm = TRUE)

hve.subdistrict.vaf.mean <- aggregate(cbind(Expenditure.Per.Capita, predictedwellfare.vw5.v3, predictedwellfare.vw5.v4 ) ~   adm3_pro , data = hve, FUN = mean, na.rm = TRUE)
hve.subdistrict.vaf.mean <- rename(hve.subdistrict.vaf.mean, c("Expenditure.Per.Capita"="Expenditure.Per.Capita.mean", "predictedwellfare.vw5.v3"="predictedwellfare.vw5.v3.mean", "predictedwellfare.vw5.v4"="predictedwellfare.vw5.v4.mean"))

hve.subdistrict.vaf.sd <- aggregate(cbind(Expenditure.Per.Capita, predictedwellfare.vw5.v3, predictedwellfare.vw5.v4 ) ~   adm3_pro , data = hve, FUN = sd , na.rm = TRUE)
hve.subdistrict.vaf.sd <- rename(hve.subdistrict.vaf.sd, c("Expenditure.Per.Capita"="Expenditure.Per.Capita.sd", "predictedwellfare.vw5.v3"="predictedwellfare.vw5.v3.sd", "predictedwellfare.vw5.v4"="predictedwellfare.vw5.v4.sd"))

hve.subdistrict.vaf.median <- aggregate(cbind(Expenditure.Per.Capita, predictedwellfare.vw5.v3, predictedwellfare.vw5.v4 ) ~   adm3_pro , data = hve, FUN = median , na.rm = TRUE)
hve.subdistrict.vaf.median <- rename(hve.subdistrict.vaf.median, c("Expenditure.Per.Capita"="Expenditure.Per.Capita.median", "predictedwellfare.vw5.v3"="predictedwellfare.vw5.v3.median", "predictedwellfare.vw5.v4"="predictedwellfare.vw5.v4.median"))


## Count individdual per score class & calcultate poverty level
hve.subdistrict.expclass <- melt(hve, id=c("adm3_pro","case.size.vaf"),
                   measure=c(  "Expenditure.Per.Capita.class" # ,                                                                                                                                                                                                                       
                              # "predictedwellfare.vw5.v3.class"  ,                                                                                                                                                                                                                        
                              # "predictedwellfare.vw5.v4.class" 
                               ))
hve.subdistrict.expclass <- dcast(hve.subdistrict.expclass, adm3_pro ~ value + variable, value.var = "case.size.vaf", fun.aggregate = sum)
#names(hve.subdistrict.expclass)
hve.subdistrict.expclass$poorexp <- (hve.subdistrict.expclass$High_Expenditure.Per.Capita.class +
                                       hve.subdistrict.expclass$Severe_Expenditure.Per.Capita.class) /
                                    psum (hve.subdistrict.expclass$High_Expenditure.Per.Capita.class, 
                                          hve.subdistrict.expclass$Low_Expenditure.Per.Capita.class, 
                                          hve.subdistrict.expclass$Moderate_Expenditure.Per.Capita.class, 
                                          hve.subdistrict.expclass$Severe_Expenditure.Per.Capita.class ,
                                          na.rm = TRUE)                                                                                                                      [4] "Moderate_Expenditure.Per.Capita.class" "Severe_Expenditure.Per.Capita.class")

## using V3 model
hve.subdistrict.expv3class <- melt(hve, id=c("adm3_pro","case.size.vaf"),
                                 measure=c( # "Expenditure.Per.Capita.class" # ,                                                                                                                                                                                                                       
                                              "predictedwellfare.vw5.v3.class" #,                                                                                                                                                                                                                        
                                             # "predictedwellfare.vw5.v4.class" 
                                 ))
hve.subdistrict.expv3class <- dcast(hve.subdistrict.expv3class, adm3_pro ~ value + variable, value.var = "case.size.vaf", fun.aggregate = sum)
hve.subdistrict.expv3class$poorv3exp <- (hve.subdistrict.expv3class$High_predictedwellfare.vw5.v3.class +
                                       hve.subdistrict.expv3class$Severe_predictedwellfare.vw5.v3.class) /
  psum (hve.subdistrict.expv3class$High_predictedwellfare.vw5.v3.class, 
        hve.subdistrict.expv3class$Low_predictedwellfare.vw5.v3.class, 
        hve.subdistrict.expv3class$Moderate_predictedwellfare.vw5.v3.class, 
        hve.subdistrict.expv3class$Severe_predictedwellfare.vw5.v3.class, na.rm = TRUE) 


## using V4 model
hve.subdistrict.expv4class <- melt(hve, id=c("adm3_pro","case.size.vaf"),
                                 measure=c( # "Expenditure.Per.Capita.class" # ,                                                                                                                                                                                                                       
                                             # "predictedwellfare.vw5.v3.class"  ,                                                                                                                                                                                                                        
                                              "predictedwellfare.vw5.v4.class" 
                                 ))
hve.subdistrict.expv4class <- dcast(hve.subdistrict.expv4class, adm3_pro ~ value + variable, value.var = "case.size.vaf", fun.aggregate = sum)
hve.subdistrict.expv4class$poorv3exp <- (hve.subdistrict.expv4class$High_predictedwellfare.vw5.v4.class +
                                       hve.subdistrict.expv4class$Severe_predictedwellfare.vw5.v4.class) /
  psum (hve.subdistrict.expv4class$High_predictedwellfare.vw5.v4.class, 
        hve.subdistrict.expv4class$Low_predictedwellfare.vw5.v4.class, 
        hve.subdistrict.expv4class$Moderate_predictedwellfare.vw5.v4.class, 
        hve.subdistrict.expv4class$Severe_predictedwellfare.vw5.v4.class, na.rm = TRUE) 

## now merging all those calculations 
subdistrict <- cbind( hve.subdistrict.size,
                      hve.subdistrict.vaf.mean,
                      hve.subdistrict.vaf.sd,
                      hve.subdistrict.vaf.median,
                      hve.subdistrict.expclass, hve.subdistrict.expv4class, hve.subdistrict.expv3class)

rm(hve.subdistrict.size)
rm(hve.subdistrict.vaf.mean)
rm(hve.subdistrict.vaf.sd)
rm(hve.subdistrict.vaf.median)
rm(hve.subdistrict.expclass)
rm(hve.subdistrict.expv4class)
rm(hve.subdistrict.expv3class)

#names(subdistrict)

write.csv(subdistrict, file="out/mapsubdistrict.csv")

## Merge this back with the subdistrict spatial data frame
rm(jor_adm3)
jor_adm3 <- readOGR("geo/subdistrict.geojson", "OGRGeoJSON")
jor_adm3@data$id = rownames(jor_adm3@data)
rm(jor_adm3_f)
jor_adm3_f <- fortify(jor_adm3, region="id")
jor_adm3_f <-join(jor_adm3_f, jor_adm3@data, by="id")

rm(maplevel3)
maplevel3 <-  ggplot(jor_adm3_f, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_f, aes(x = long, y = lat, group = group), alpha = 0.5) +
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Governorates of Jordan") +
  theme_bw()
ggsave("out/maplevel3.png", maplevel3, width=8, height=6,units="in", dpi=300)

## Now merging it with the subdistrict info we created.
jor_adm3_sub <-join(jor_adm3_f, subdistrict, by="adm3_pro")
# names(jor_adm3_sub)

#### First set of map on expenditure per capita levels
###
rm(maplevel3_Expenditure.Per.Capita.mean)
maplevel3_Expenditure.Per.Capita.mean <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Expenditure.Per.Capita.mean), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(-170, 28, 68 , 100),
                      labels=c("Severe", "High", "Moderate", "Low")) +
 # add labels at centroids
# geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Expenditure Per Capita / Mean") +
  theme_bw()
ggsave("out/maplevel3_Expenditure_Per_Capita_mean.png", maplevel3_Expenditure.Per.Capita.mean, width=8, height=6,units="in", dpi=300)

#####
rm(maplevel3_Expenditure.Per.Capita.median)
maplevel3_Expenditure.Per.Capita.median <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Expenditure.Per.Capita.median), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(-170, 28, 68 , 100),
                      labels=c("Severe", "High", "Moderate", "Low")) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Expenditure Per Capita / Median") +
  theme_bw()
ggsave("out/maplevel3_Expenditure_Per_Capita_median.png", maplevel3_Expenditure.Per.Capita.median, width=8, height=6,units="in", dpi=300)


##
rm(maplevel3_Expenditure.Per.Capita.sd)
maplevel3_Expenditure.Per.Capita.sd <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Expenditure.Per.Capita.sd), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(-170, 28, 68 , 100),
                      labels=c("Severe", "High", "Moderate", "Low")) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Expenditure Per Capita / Standard Deviation") +
  theme_bw()
ggsave("out/maplevel3_Expenditure_Per_Capita_sd.png", maplevel3_Expenditure.Per.Capita.sd, width=8, height=6,units="in", dpi=300)




################################################################
#### plotting all maps sin one layout
png(filename="out/map_expenditure.png",
    width = 844, height = 546, units = "px", pointsize = 12,
    bg = "white", res = NA, restoreConsole = TRUE)
multiplot( maplevel3_Expenditure.Per.Capita.mean ,
           maplevel3_Expenditure.Per.Capita.median , 
           maplevel3_Expenditure.Per.Capita.sd , cols=2)

dev.off()


########## Set of map on poverty level
## See reference here: http://books.openedition.org/ifpo/5036?lang=fr
#str(jor_adm3_sub$Pov_2002)

jor_adm3_sub$pPov_2002 <- as.factor(jor_adm3_sub$Pov_2002)

rm(maplevel3_Pov_2002)
maplevel3_Pov_2002 <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Pov_2002), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(0.25,  1), labels=c("Poverty Pockets <25%", "Other Sub districts")) +
  #scale_fill_manual( values=cbPalette, breaks=c(0.25,  1), labels=c("Poverty Pockets <25%", "Other Sub districts")) +
  #scale_fill_manual(breaks = c(0.25, 0.5,  1), 
  #                  values = c("grey50", "grey80", "black"), 
  #                  labels=c("Poverty Pockets <25%", "middle", "Other Sub districts"),
  #                  drop = FALSE) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Poverty level  - Pov_2002") +
  theme_bw()
ggsave("out/maplevel3_Pov_2002.png", maplevel3_Pov_2002, width=8, height=6,units="in", dpi=300)

rm(maplevel3_Pov_2006)
maplevel3_Pov_2006 <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Pov_2006), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") +
  scale_fill_gradient(breaks=c(0.25,  1), labels=c("Poverty Pockets <25%", "Other Sub districts")) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Poverty level  - Pov_2006") +
  theme_bw()
ggsave("out/maplevel3_Pov_2006.png", maplevel3_Pov_2006, width=8, height=6,units="in", dpi=300)

rm(maplevel3_Pov_2008)
maplevel3_Pov_2008 <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Pov_2008), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(0.25,  1), labels=c("Poverty Pockets <25%", "Other Sub districts")) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Poverty level  - Pov_2008") +
  theme_bw()
ggsave("out/maplevel3_Pov_2008.png", maplevel3_Pov_2008, width=8, height=6,units="in", dpi=300)

rm(maplevel3_Pov_2010)
maplevel3_Pov_2010 <-  ggplot(jor_adm3_sub, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm3_sub, aes(x = long, y = lat, group = group, fill= Pov_2010), alpha = 0.5) +
  #scale_fill_brewer(palette="PuRd", name=" JOD") + 
  scale_fill_gradient(breaks=c(0.25,  1), labels=c("Poverty Pockets <25%", "Other Sub districts")) +
  # add labels at centroids
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ 
  geom_path(data = jor_adm3_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Poverty level  - Pov_2010") +
  theme_bw()
ggsave("out/maplevel3_Pov_2010.png", maplevel3_Pov_2010, width=8, height=6,units="in", dpi=300)



################################################################
#### plotting all maps sin one layout
png(filename="out/map_poverty.png",
    width = 844, height = 546, units = "px", pointsize = 12,
    bg = "white", res = NA, restoreConsole = TRUE)
map_poverty <- multiplot(maplevel3_Pov_2002,
          maplevel3_Pov_2006,
          maplevel3_Pov_2008,
          maplevel3_Pov_2010, cols=2)

dev.off()


############################################################
########################################
### Simple map background with governorate

jor_adm1 <- readOGR("geo/admin1.geojson", "OGRGeoJSON")
#plot(jor_adm1)
# Fortify them
jor_adm1@data$id = rownames(jor_adm1@data)
rm(jor_adm1_f)
jor_adm1_f <- fortify(jor_adm1, region="id")
jor_adm1_f <-join(jor_adm1_f, jor_adm1@data, by="id")

rm(maplevel1)
maplevel1 <-  ggplot(jor_adm1_f, aes(long, lat)) + 
  coord_equal() +
  geom_polygon(data = jor_adm1_f, aes(x = long, y = lat, group = group), alpha = 0.5) +
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  geom_path(data = jor_adm1_f, aes(x = long, y = lat, group = group), color="white")+
  ggtitle("Governorates of Jordan") +
  theme_bw()
ggsave("out/maplevel1.png", maplevel1, width=8, height=6,units="in", dpi=300)
