#####
## plotting prediction for welfare on full progres database


#source("code/analytic_code/2_Welfare-Model_ProGres_only.R")
progres.case <- read.csv("out/progres-only/progrescase-with-prediction.csv")

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


########################################################################################
######################################################################################
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


########################################################################################
######################################################################################
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
  ggtitle("Expenditure.Per.Capita.class.h3 for all Refugees in progres")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres.png", bar.Expenditure.Per.Capita.class, width=8, height=6,units="in", dpi=300)

bar.Expenditure.Per.Capita.class <- bar.Expenditure.Per.Capita.class +
  geom_text(aes(x=progres.case$predictedwellfare.class,
                y=progres.case$Num_Inds + 0.3 * sign(Num_Inds),
                label=format(progres.case$Num_Inds, digits=2),
                hjust=ifelse(progres.case$Num_Inds > 0,0,1)), 
            size=3,
            color=rgb(100,100,100, maxColorValue=255))  
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres.png", bar.Expenditure.Per.Capita.class, width=8, height=6,units="in", dpi=300)
#rm(bar.Expenditure.Per.Capita.class)

#rm(bar.Expenditure.Per.Capita.class.h3)
bar.Expenditure.Per.Capita.class.h3 <- ggplot(data=progres.case, 
                                              aes(x=predictedwellfare.class.h3 , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("Expenditure.Per.Capita.class.h3 for all Refugees in progres")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progresh3.png", bar.Expenditure.Per.Capita.class.h3, width=8, height=6,units="in", dpi=300)
#rm(bar.Expenditure.Per.Capita.class.h3)

#rm(bar.Expenditure.Per.Capita.class.h4)
bar.Expenditure.Per.Capita.class.h4 <- ggplot(data=progres.case, 
                                              aes(x=predictedwellfare.class.h4 , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("Expenditure.Per.Capita.class.h4 for all Refugees in progres")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progresh4.png", bar.Expenditure.Per.Capita.class.h4, width=8, height=6,units="in", dpi=300)
#rm(bar.Expenditure.Per.Capita.class)

#rm(bar.Expenditure.Per.Capita.class.aug)
bar.Expenditure.Per.Capita.class.aug <- ggplot(data=progres.case, 
                                               aes(x=predictedwellfare.class.aug , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("Expenditure.Per.Capita.class.aug for all Refugees in progres")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progresaug.png", bar.Expenditure.Per.Capita.class.aug, width=8, height=6,units="in", dpi=300)
#rm(bar.Expenditure.Per.Capita.class)



###############################################
#############################################################################################

#### Bar graph to show repartition by class for expenditure per capita
rm(bar.Expenditure.Per.Capita.class.hve)
bar.Expenditure.Per.Capita.class.hve <- ggplot(data=progres.case, 
                                               aes(x=predictedwellfare.class , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  facet_grid(visited ~ .) +
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("# Ind for all registered refugees per predicted wellfare class - Comparison visited or not")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres-visited.png", bar.Expenditure.Per.Capita.class.hve, width=8, height=6,units="in", dpi=300)
rm(bar.Expenditure.Per.Capita.class.hve)

names(progres.case)
rm(bar.Expenditure.Per.Capita.class.hve.cash)
bar.Expenditure.Per.Capita.class.hve.cash <- ggplot(data=progres.case, 
                                               aes(x=predictedwellfare.class , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  facet_wrap(visited ~ status) +
  # coord_flip()+
  xlab("Class: Severe<28JOD; High:28-68JOD; Moderate:68-100JOD; Low>100JOD") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  ggtitle("# Ind for all registered refugees per predicted wellfare class - Comparison - visited/cash or not")
ggsave("out/progres-only/barExpenditurePerCapitaclass-progres-visitedcash.png", bar.Expenditure.Per.Capita.class.hve.cash, width=8, height=6,units="in", dpi=300)
rm(bar.Expenditure.Per.Capita.class.hve.cash)
