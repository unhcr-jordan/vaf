##########################################################################
#### Import data & Rename variable
source("code/processing_code/1_composite_variable_homevisit.R")

########################################################
## Anonymisation


drop <- c(
  "Date.of.Visit.",
  "Volunteer..Name.",
  "Volunteer..Phone.Number.",
  "Household.information.Name.of.Principal.Applicant",
  "Household.information.Governorate.",
  "Household.information.District.",
  "Specify.identification.documentations.Relationship",
  "Household.information.Telephone.s..",
  "Household.information.Alternative.phone.s..",
  "Payment.Who.","Payment.Other..specify..",
  "Water.Others.",
  "Financial.Situation.Remittances..where..country..",
  "Financial.Situation.Remittances..whom..relationship..",
  "Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify",
  "Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..",
  "Financial.Situation.specify.other.income.",
  "Financial.Situation.If.there.is.a.gap.between.the.monthly.expenditure.and.the.monthly.income..how.do.you.manage.to.fill.this.gap.",
  "Currently..how.many.of.your.children.do.the.following.Name..",
  "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Name",
  "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Relationship..",
  "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Family.Size",
  "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Age",
  "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Notes",
  "Specify.identification.documentations.UNHCR.File.Number",
  "Type.of.Housing.IF.other.please.specify..",
  "Payment.Who.",
  "Payment.Other..specify..",
  "Water.Others.",
  "Financial.Situation.Remittances..where..country..",
  "Financial.Situation.Remittances..whom..relationship..",
  "Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify",
  "Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..","Financial.Situation.specify.other.income.",
  "Currently..how.many.of.your.children.do.the.following.Name..","Children.Not.Attending.School.If.yes..why..",
  "Number.of.family.members.who.are.going.to.universities.in.Jordan...Name.of.the.universities..",
  "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Name.",
  "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Relationship.",
  "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.IF.other.please.specify..",
  "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Medical.reports.checked..",
  "Access.to.Health.Services.IF.other.please.specify..","Effect.on.Daily.Activities.Work.If.yes..please.specify..",
  "If.no..specify.family.members.that.separated.and.their.location..IF.other.please.specify..",
  "Documentation...Protection.If.not..what.is.the.reason.","Documentation.IF.other.please.specify..",
  "Working.Family.Member.If.not..what.is.the.reason..","Working.Family.Member.What.kind.of.job..Other..specify",
  "Working.Family.Member.IF.other.please.specify..","Documentation.If.yes..the.date.","Specific.Needs.More.explination..",
  "Categorization.of.Vulnerabilities.Notes.on.the.general.previous.situation.of.the.family.CCO",
  "Categorization.of.Vulnerabilities.Notes.on.the.general.situation.of.the.family.from.the.moment.of.their.arrival.to.Jordan.until.now",
  "Name.of.Principal.Applicant",
  "General.remarkes.unreachable",
  "start",
  "end",
  "lat",
  "long",
  "CoO_L1",
  "ProcessingGroupNumber",
  "Household.information.address..",
  "Household.information.UNHCR.File.Number",
  "Household.information.Telephone..s..",
  "Household.information.Alternative.phone..s..",
  "Volunteer..Home.Visit...Urgent" ,                                                                                                                                                                                                                         
  "Volunteer..Home.Visit...Normal"  ,                                                                                                                                                                                                                        
  "Volunteer..Home.Visit...In.Field",                                                                                                                                                                                                                        
  "Volunteer..IRD.List.Number.." ,                                                                                                                                                                                                                           
  "Volunteer..Case.Status...Reachable" ,                                                                                                                                                                                                                     
  "Volunteer..Case.Status...Unreachable",                                                                                                                                                                                                                    
  "Volunteer..Case.Status...Refused.Visit",                                                                                                                                                                                                                  
  "Volunteer..Case.Status...person.is.dead" ,                                                                                                                                                                                                                
  "Volunteer..Case.Status...out.of.county",                                                                                                                                                                                                                  
  "Volunteer..Case.Status...merging.with.another..file.number",                                                                                                                                                                                              
  "Volunteer..Case.Status...out.of.IRD.restriction" ,                                                                                                                                                                                                        
  "Household.information.address.."       ,                                                                                                                                                                                                                  
  "Household.information.UNHCR.File.Number" 
)

hve4 <-hve3[,!(names(hve3) %in% drop)]


##########################################################################################################################
##########################################################################################################################
#Sex PA (Replace "M" with 1 and "F" with 0): 
hve$Sex..PA.<- as.character(hve$Sex..PA.)
hve$Sex..PA.[hve$Sex..PA.=="M"] <- 1
hve$Sex..PA.[hve$Sex..PA.=="F"] <- 0


##################################
#Dependency Ratio (Must create duplicate; Replace 0 with 1 for INF):
View(hve$MembersAtWorkingAge)
hve$MembersAtWorkingAge2 <- as.numeric(hve$MembersAtWorkingAge)
hve$MembersAtWorkingAge2[hve$MembersAtWorkingAge2 == "0"] <- "1"
hve$MembersAtWorkingAge2 <- as.numeric (hve$MembersAtWorkingAge2)
hve$Dependency.Ratio <- (hve$Dependents / hve$MembersAtWorkingAge2) 


#Education PA (Duplicate Variable and Replace Values):
#Dummy.Uni.PA <- (hve2$EducationLevelCode.PA.)
#hve <- cbind(hve2,Dummy.Uni.PA)

hve$Dummy.Uni.PA <- ""

hve$Dummy.Uni.PA <- as.character(hve$Dummy.Uni.PA)
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="14"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="13"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="12"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="11"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="10"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="9"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="8"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="7"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="6"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="5"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="4"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="3"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="2"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="1"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="Technical or vocational"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="No Education"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="Informal education"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="KG"] <- 0
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="University Level"] <- 1
hve$Dummy.Uni.PA[hve$Dummy.Uni.PA=="Post university level"] <- 1
#Must revert to numeric ("-" cells replaced by coercion with NA):
hve$Dummy.Uni.PA <- as.numeric(hve$Dummy.Uni.PA)
#Must replace NA cells with 0:
hve$Dummy.Uni.PA[is.na(hve$Dummy.Uni.PA)] <- 0
#Education PA (Quasi-continuous)
hve$EducationLevelCode.PA. <- as.character(hve$EducationLevelCode.PA.)
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="Technical or vocational"] <- 10
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="No Education"] <- 0
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="Informal education"] <- 3
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="KG"] <- 0
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="University Level"] <- 17
hve$EducationLevelCode.PA.[hve$EducationLevelCode.PA.=="Post university level"] <- 19
hve$EducationLevelCode.PA. <- as.numeric(hve$EducationLevelCode.PA.)
hve$EducationLevelCode.PA.[is.na(hve$EducationLevelCode.PA.)] <- 0
#EducationLevelCode.PA. <- hve$EducationLevelCode.PA.

### 

drop2 <- c()
homevisit3.ano <- homevisit3[,!(names(homevisit3) %in% drop2)]
View(homevisit3.ano)


## sdcMicro: Statistical Disclosure Control methods for anonymization of microdata and risk estimation
install.packages("sdcMicro")
install.packages("sdcMicroGUI") 

require(sdcMicroGUI); sdcGUI()



### http://stackoverflow.com/questions/10454973/how-to-create-example-data-set-from-private-data-replacing-variable-names-and-l
## A function to anonymise columns in 'colIDs' 
##    colIDs can be either column names or integer indices
anonymiseColumns <- function(df, colIDs) {
  id <- if(is.character(colIDs)) match(colIDs, names(df)) else colIDs
  for(id in colIDs) {
    prefix <- sample(LETTERS, 1)
    suffix <- as.character(as.numeric(as.factor(df[[id]])))
    df[[id]] <- paste(prefix, suffix, sep="")
  }
  names(df)[id] <- paste("V", id, sep="")
  df
}

## A data.frame containing sensitive information in column 1 & 2
## Anonymise it - df2 <- anonymiseColumns(df, c(1,3))

anonymise <- function(df, colString = "Variable", rowString = "Sample") {
  foo <- function(x) {
    if(is.factor(x)) {
      levels(x) <- sample(LETTERS, length(levels(x)))
    }
    x
  }
  ## replace the variable names
  colnames(df) <- paste(colString, seq_len(ncol(df)), sep = "")
  ## fudge any factor levels
  df <- data.frame(lapply(df, foo))
  ## replace rownames
  rownames(df) <- paste(rowString, seq_len(nrow(df)), sep = "")
  ## return
  df
}
