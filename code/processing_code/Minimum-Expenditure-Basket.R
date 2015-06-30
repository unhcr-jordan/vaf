require(ggplot2)

##The first part of this script is taken from homevisit3.R
## Extracted from https://www.unhcrmenadagdata.org/RaisJordan
## The export has two line of header so we need to skip the first one
homevisit <- read.csv("data/Home_visit_version3.csv", skip=1)

homevisit3 <- homevisit

## Label are not easily legible
## Labels have been manually reviewed
label <- read.csv("data/homevisit_label.csv")

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(homevisit3) <- label[, 3]

## str(homevisit3)
rm(label)

## extracting coordinates from dataset
homevisit3$geo <- as.character(homevisit3$Household.information.address..)
options(digits = 15)
homevisit3$lat <- as.numeric(substr(homevisit3$geo , 1,13))
homevisit3$long <- as.numeric(substr( homevisit3$geo, 15,27))


## Eliminate record without coordinates
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["lat"])), ]
homevisit3 <-homevisit3[!rowSums(is.na(homevisit3["long"])), ]

# str(homevisit3)

##################################################
## Section Looking into MEB related information ##
##################################################

## extracting MEB related columns

df_meb<-homevisit3[,c(
  'Date.of.Visit.',
  'Household.information.UNHCR.File.Number',
  'Household.information.Family.Size',
  'Monthly.Expenditure.-JD-.The.portion.Rent.that.you.are.paying',
  'Monthly.Expenditure.-JD-.Utilities.-electricity..gas..etc.-',
  'Monthly.Expenditure.-JD-.Food.-excluding.WFP.vouchers-',
  'Monthly.Expenditure.-JD-.Water.-network..tanker..bottled..dislodging.waste.water..etc.-',
  'Monthly.Expenditure.-JD-.Treatment.-medical..pharmaceuticals-',
  'Monthly.Expenditure.-JD-.Education',
  'Monthly.Expenditure.-JD-.Transportation',
  'Monthly.Expenditure.-JD-.Infant.needs.-diapers-milk-',
  'Monthly.Expenditure.-JD-.Basic.HH.items.-hygiene.and.NFIs-',
  'Monthly.Expenditure.-JD-.Debt.repayment.-monthly-',
  'Monthly.Expenditure.-JD-.Other..please.specify',
  'Financial.Situation.Total.Expenditure',
  'lat',
  'long')]

## Rename Headers
new_headers<-c(
  'HV_Date',
  'UNHCR_ID',
  'FS',
  'Exp_Rent_portion',
  'Exp_Utilities',
  'Exp_Food_ex_WFP',
  'Exp_WASH',
  'Exp_MED',
  'Exp_EDU',
  'Exp_transport',
  'Exp_infant_needs',
  'Exp_basic_needs',
  'Exp_debt',
  'Exp_other',
  'Exp_total',
  'lat',
  'lon')

names(df_meb)<-new_headers

##Convert to numerical values
df_meb$FS<-as.numeric(as.character(df_meb$FS))
df_meb$Exp_Rent_portion<-as.numeric(as.character(df_meb$Exp_Rent_portion))
df_meb$Exp_Utilities<-as.numeric(as.character(df_meb$Exp_Utilities))
df_meb$Exp_Food_ex_WFP<-as.numeric(as.character(df_meb$Exp_Food_ex_WFP))
df_meb$Exp_WASH<-as.numeric(as.character(df_meb$Exp_WASH))
df_meb$Exp_MED<-as.numeric(as.character(df_meb$Exp_MED))
df_meb$Exp_EDU<-as.numeric(as.character(df_meb$Exp_EDU))
df_meb$Exp_transport<-as.numeric(as.character(df_meb$Exp_transport))
df_meb$Exp_infant_needs<-as.numeric(as.character(df_meb$Exp_infant_needs))
df_meb$Exp_basic_needs<-as.numeric(as.character(df_meb$Exp_basic_needs))
df_meb$Exp_debt<-as.numeric(as.character(df_meb$Exp_debt))
df_meb$Exp_other<-as.numeric(as.character(df_meb$Exp_other))
df_meb$Exp_total<-as.numeric(as.character(df_meb$Exp_total))

####################################################
##!!!!! FS value is not clean and has extreme values
##!!!!! (data entry error are capped and FS=0 and 
##!!!!! FS=NA need to be removed
####################################################

## capping FS at 13
fs_cap<-df_meb[which.max(df_meb[,3]),3]

repeat {
  row<-which.max(df_meb[,3])
  df_meb<-df_meb[-row,]
  fs_cap<-df_meb[which.max(df_meb[,3]),3]
  if (fs_cap<=14) break
}


## Change zero values to NA for later elimination
## First line is to eliminate existing NAs in order to allow for subsequent FOR loop
df_meb<-df_meb[complete.cases(df_meb$FS),]

for (i in 1:nrow(df_meb)) {
  if (df_meb[i,3]==0) df_meb[i,3]<-NA
}
     

## removing NAs
df_meb<-df_meb[complete.cases(df_meb$FS),]
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


## Free up memory
##rm(homevisit3)
##rm(homevisit)
##rm(new_headers)


## Change date format
df_meb$HV_Date<-as.Date(as.character(df_meb$HV_Date), "%d-%m-%Y")

## Filter data by date (01/09/2014 - 28/02/2015)
df_meb = subset(df_meb, as.Date(df_meb$HV_Date)>='2014-09-01' & 
                        as.Date(df_meb$HV_Date)<='2015-02-28')

summary(df_meb)

## visualize MEB data
## Boxplot and violin plot giving distribution of expenditure type by case size
## TO DO STILL: Use FOR loop to plot all variations of plots 

plot1_rent<-ggplot(df_meb, aes(x=FS, y=Exp_Rent_portion)) + geom_violin() + facet_wrap(~FS, nrow=1)
plot2_rent<-ggplot(df_meb, aes(x=FS, y=Exp_Rent_portion)) + geom_boxplot() + facet_wrap(~FS, nrow=1)

## for (i in 1 to length(new_headers)) {
## if new_headers[i] CONTAINS "EXP" {
## USE new_headers[i] to build ggplot expression and save to vector (possibly with ggsave)
##}
##}
## 
