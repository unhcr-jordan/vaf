##########################################################################
#WELFARE MODEL 5000 SCRIPT
##########################################################################

## Coding style guide
#  http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

#Dataframes:
#hve: original dataset with zeros
#hve2: clean dataset (eliminate 56 columns with arabic text, incorrect family size, incorrect number of rooms)
#hve3: clean dataset with duplicate variables


##########################################################################
#DATA CLEANING:
##########################################################################

#Import Dataset and Change Name:
hve <- read.csv("data/Home_visit_version3.csv", skip=1)

## Volker


#Replace NA with 0 in data.frame:
hve[is.na(hve)]<-0
#View(hve)
attach(hve)


#Eliminate Columns with Arabic Text (56 Columns):
drop<-c("X.Date.of.Visit.","Volunteer..Name.","Volunteer..Phone.Number.","Household.information.Name.of.Principal.Applicant","Household.information.Governorate.","Household.information.District.","X1.2.Specify.identification.documentations.Relationship","Household.information.Telephone.s..","Household.information.Alternative.phone.s..","Type.of.Housing.IF.other.please.specify..", "Payment.Who.","Payment.Other..specify..", "Water.Others.","Financial.Situation.Remittances..where..country..","Financial.Situation.Remittances..whom..relationship..","Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify","Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..","Financial.Situation.specify.other.income.","Financial.Situation.If.there.is.a.gap.between.the.monthly.expenditure.and.the.monthly.income..how.do.you.manage.to.fill.this.gap.", "Currently..how.many.of.your.children.do.the.following.Name..","Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Name","Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Relationship..","Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Family.Size","Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Age","Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Notes","X1.2.Specify.identification.documentations.UNHCR.File.Number","Type.of.Housing.IF.other.please.specify..","Payment.Who.","Payment.Other..specify..","Water.Others.","Financial.Situation.Remittances..where..country..","Financial.Situation.Remittances..whom..relationship..","Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify","Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..","Financial.Situation.specify.other.income.","Currently..how.many.of.your.children.do.the.following.Name..","Children.Not.Attending.School.If.yes..why..","Number.of.family.members.who.are.going.to.universities.in.Jordan...Name.of.the.universities..","Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Name._____________","Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Relationship.___________","Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.IF.other.please.specify..","Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Medical.reports.checked..","Access.to.Health.Services.IF.other.please.specify..","Effect.on.Daily.Activities.Work.If.yes..please.specify..","If.no..specify.family.members.that.separated.and.their.location..IF.other.please.specify..","Documentation...Protection.If.not..what.is.the.reason.","Documentation.IF.other.please.specify..","Working.Family.Member.If.not..what.is.the.reason..","Working.Family.Member.What.kind.of.job..Other..specify","Working.Family.Member.IF.other.please.specify..","Documentation.If.yes..the.date.","Specific.Needs.More.explination..","Categorization.of.Vulnerabilities.Notes.on.the.general.previous.situation.of.the.family.CCO","Categorization.of.Vulnerabilities.Notes.on.the.general.situation.of.the.family.from.the.moment.of.their.arrival.to.Jordan.until.now","X.Name.of.Principal.Applicant","X.General.remarkes.unreachable")
hve2<-hve[,!(names(hve) %in% drop)]
#View(hve2)


#Eliminate Rows with Family Size is = 0 and Family Size is >= 20 (Total of 10 records present of family size of 0, 500 and 5000):
hve2$Household.information.Family.Size <- as.numeric(hve2$Household.information.Family.Size)
hve2 <- hve2 [!(hve2$Household.information.Family.Size==0),]
hve2 <- hve2 [!(hve2$Household.information.Family.Size >= 20),]
#View(hve2)


#Eliminate Rows with Number of Rooms is = 0 and Number of Rooms is >= 10 (Total of 24 records present with 0, 14, 16 and 60 rooms):
hve2$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.<- as.numeric(hve2$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.)
hve2 <- hve2 [!(hve2$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.== 0),]
hve2 <- hve2 [!(hve2$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities. >= 10),]





##########################################################################
#VARIABLE GENERATION (including COMPOSITE INDICES):
##########################################################################

#Education PA (Duplicate Variable and Replace Values):
Dummy.Uni.PA <- (hve2$EducationLevelCode.PA.)
hve3 <- cbind(hve2,Dummy.Uni.PA)
hve3$Dummy.Uni.PA <- as.character(hve3$Dummy.Uni.PA)
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="14"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="13"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="12"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="11"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="10"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="9"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="8"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="7"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="6"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="5"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="4"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="3"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="2"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="1"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="Technical or vocational"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="No Education"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="Informal education"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="KG"] <- 0
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="University Level"] <- 1
hve3$Dummy.Uni.PA[hve3$Dummy.Uni.PA=="Post university level"] <- 1
#Must revert to numeric ("-" cells replaced by coercion with NA):
hve3$Dummy.Uni.PA <- as.numeric(hve3$Dummy.Uni.PA)
#Must replace NA cells with 0:
hve3$Dummy.Uni.PA[is.na(hve3$Dummy.Uni.PA)] <- 0
Dummy.Uni.PA <- hve3$Dummy.Uni.PA


#Education PA (Quasi-continuous)
hve3$EducationLevelCode.PA. <- as.character(hve3$EducationLevelCode.PA.)
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="Technical or vocational"] <- 10
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="No Education"] <- 0
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="Informal education"] <- 3
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="KG"] <- 0
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="University Level"] <- 17
hve3$EducationLevelCode.PA.[hve3$EducationLevelCode.PA.=="Post university level"] <- 19
hve3$EducationLevelCode.PA. <- as.numeric(hve3$EducationLevelCode.PA.)
hve3$EducationLevelCode.PA.[is.na(hve3$EducationLevelCode.PA.)] <- 0
EducationLevelCode.PA. <- hve3$EducationLevelCode.PA.
hve3 <- cbind(hve2, EducationLevelCode.PA.)


#Expenditure Per Capita:
Expenditure.Per.Capita <- (hve3$Financial.Situation.Total.Expenditure/hve3$Household.information.Family.Size)
hve3 <- cbind(hve3,Expenditure.Per.Capita)
#Log Expenditure Per Capita:
ln.Expenditure.Per.Capita <-log(Expenditure.Per.Capita)
ln.Expenditure.Per.Capita[ln.Expenditure.Per.Capita == "-Inf"] <- "0"
ln.Expenditure.Per.Capita <- as.numeric(ln.Expenditure.Per.Capita)
hve3 <- cbind(hve3,ln.Expenditure.Per.Capita)


#Income Per Capita:
Income.Per.Capita <- (hve3$Financial.Situation.Total.income../hve3$Household.information.Family.Size)
hve3 <- cbind(hve3,Income.Per.Capita)
#Income Per Capita Squared:
Income.Per.Capita.Squared <- (hve3$Financial.Situation.Total.income../hve3$Household.information.Family.Size)^2
hve3 <- cbind(hve3,Income.Per.Capita.Squared)


#Debt To Expenditure (Must Create Duplicate, Replace 0 with 1):
Total.Expenditure2 <- (hve3$Financial.Situation.Total.Expenditure)
hve3 <- cbind(hve3, Total.Expenditure2)
Total.Expenditure2[Total.Expenditure2 == "0"] <- "1"
Total.Expenditure2 <- as.numeric(Total.Expenditure2)
Debt.To.Expenditure <- (hve3$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc../Total.Expenditure2)
hve3 <- cbind(hve3,Debt.To.Expenditure)


#Debt To Income (Must Create Duplicate, Replace 0 with 1 for INF):
Total.Income2 <- (hve3$Financial.Situation.Total.income..)
hve3 <- cbind(hve3, Total.Income2)
Total.Income2[Total.Income2 == "0"] <- "1"
Total.Income2 <- as.numeric(Total.Income2)
Debt.To.Income <- (hve3$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc../Total.Income2)
hve3 <- cbind(hve3,Debt.To.Income)


#Family Size Squared:
Family.Size.Squared <- (hve3$Household.information.Family.Size)^2
hve3 <- cbind(hve3,Family.Size.Squared)


#Family Size All File Numbers Squared:
Family.Size.All.File.Numbers.Squared <- (hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)^2
hve3 <- cbind(hve3,Family.Size.All.File.Numbers.Squared)


#Coping Strategies to Meet Basic Food Needs:
Coping.Strategies.Basic.Food.Needs <- (hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Spent.savings.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Spent.savings.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Bought.food.on.credit.or.borrowed.money.to.purchase.food.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Bought.food.on.credit.or.borrowed.money.to.purchase.food.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Reduced.essential.non.food.expenditure.such.as.education.health.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Reduced.essential.non.food.expenditure.such.as.education.health.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sent.adult.family.members.to.beg.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sent.adult.family.members.to.beg.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sent.children..under.18..family.members.to.beg.Yes+hve3$n.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs..Sent.children..under.18..family.members.to.beg.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore)
hve3 <- cbind(hve3,Coping.Strategies.Basic.Food.Needs)
#Coping Strategies to Meet Basic Food Needs Squared:
Coping.Strategies.Basic.Food.Needs.Squared <- (Coping.Strategies.Basic.Food.Needs)^2
hve3 <- cbind(hve3,Coping.Strategies.Basic.Food.Needs.Squared)


#Coping Strategies Used In Last Six Months:
Coping.Strategies.Used.Last.Six.Months <- (hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Sharing.costs.with.host.family..Jordanian...Syrian.+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Support.from.family.members..irregular.remittances.+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Support.from.host.community..Jordanian..+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Humanitarian.assistance..CBOs..personal.donations..etc..+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Selling.properties..jewelry..car..etc..+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Selling.food.vouchers+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Selling.household.assets+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Borrowing.money+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Buying.against.credit+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Dropping.children.out.from.school+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Child.labor...16.years.+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Begging+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Savings+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Irregular.work+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Have.not.paid.the.rent.for.the.past.months)
hve3 <- cbind(hve3,Coping.Strategies.Used.Last.Six.Months)
#Coping Strategies Used In Last Six Months Squared:
Coping.Strategies.Used.Last.Six.Months.Squared <- (Coping.Strategies.Used.Last.Six.Months)^2
hve3 <- cbind(hve3,Coping.Strategies.Used.Last.Six.Months.Squared)


#House Assets:
House.Assets <- (hve3$Type.of.Housing.Access.to.kitchen..Yes+hve3$Type.of.Housing.Access.to.sanitary.facilities..Yes+hve3$Type.of.Housing.Ventilation..Yes+hve3$Type.of.Housing.Access.to.electricity..Yes)
House.Assets.To.Family.Size <- (House.Assets/hve3$Household.information.Family.Size)
hve3 <- cbind(hve3,House.Assets)


#House Poor Conditions Observed:
House.Poor.Conditions <- (hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Damp.walls+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Leaking.roofs+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Hygienic.concerns+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Privacy.concern+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Rodents+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Poor.insulation..winter...summer.+hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..broken.windows)
#NO: hpcfs<-(House.Poor.Conditions/hve3$Household.information.Family.Size) #NO
hve3 <- cbind(hve3,House.Poor.Conditions)


#Chronic Diseases in Family:
Chronic.Diseases <- (hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Hypertension+hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Diabetes+hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Cardiovascular+hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Critical.medical.condition..life.threatening.+hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Other..specify)
#NO: cdfs<-(Chronic.Diseases/hve3$Household.information.Family.Size) #ok
hve3 <- cbind(hve3,Chronic.Diseases)


#Vaccinations Not Received:
Vaccinations.Not.Received <- (hve3$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.measles..Yes+hve3$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.polio...child.who.never.had.a.polio.dose..Yes+hve3$Vaccination.If.you.have.children.under.2.years..have.they.received.routine.vaccines..EPI....Note..As.this.is.sometimes.difficult.for.household.and.staff.to.distinguish.from.above..could.use.a.proxy...question.such.as..do.you.have.children.under.2.who.have.a.vaccination.card...Yes)
hve3 <- cbind(hve3,Vaccinations.Not.Received)


#Disabilities in Family:
Disability.In.Family <- (hve3$Age...Disability.details.Age...0.17.years.old+hve3$Age...Disability.details.Age...18.60.years.old+hve3$Age...Disability.details.Age...61.years.and.above)
hve3 <- cbind(hve3,Disability.In.Family)


#House Luxury Assets:
House.Luxury.Assets <- (hve3$Type.of.Housing.Assets...Floor.mattress+hve3$Type.of.Housing.Assets...Sofa.set+hve3$Type.of.Housing.Assets...Kitchen.utilities+hve3$Type.of.Housing.Assets...Computer+hve3$Type.of.Housing.Assets...Blankets+hve3$Type.of.Housing.Assets...Stove+hve3$Type.of.Housing.Assets...Washing.machine+hve3$Type.of.Housing.Assets...Table.chairs+hve3$Type.of.Housing.Assets...Cabinets+hve3$Type.of.Housing.Assets...Fridge+hve3$Type.of.Housing.Assets...Television+hve3$Type.of.Housing.Assets...Water.heater+hve3$Type.of.Housing.Assets...Freezer+hve3$Type.of.Housing.Assets...Other..specify)
#NO: lafs<-(House.Luxury.Assets/hve3$Household.information.Family.Size) 
hve3 <- cbind(hve3,House.Luxury.Assets)


#House Crowding (People per Room):
House.Crowding<-(hve2$Household.information.Family.Size/hve2$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.)
hve3 <- cbind(hve3,House.Crowding)
#House Crowding Squared (People per Room):
House.Crowding.Squared<-(House.Crowding)^2
hve3 <- cbind(hve3,House.Crowding.Squared)


#Alternative, but not as good:
House.CrowdingX6X<-(hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file../hve3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.)
House.CrowdingX6Xsquared<-(House.CrowdingX6X)^2
hve3 <- cbind(hve3,House.CrowdingX6X)
hve3 <- cbind(hve3,House.CrowdingX6Xsquared)


#House Crowding Version 2 (Area per Person):
House.Crowding.v2 <- (hve2$Type.of.Housing.Total.area.excluding.the.kitchen...WASH.facilities..Sq..meter../hve2$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)
#ISSUE: Reported value of ZERO exists for both variables in data
House.Crowding.v2[House.Crowding.v2 == "Inf"] <- "0"
House.Crowding.v2[House.Crowding.v2 == "NaN"] <- "0"
House.Crowding.v2 <- as.numeric(House.Crowding.v2)
#House Crowding Version 2 (Area per Person) Squared:
House.Crowding.v2.Squared <- (House.Crowding.v2)^2
hve3 <- cbind(hve3,House.Crowding.v2)
hve3 <- cbind(hve3,House.Crowding.v2.Squared)


#Dependency Ratio (Must create duplicate; Replace 0 with 1 for INF):
X.MembersAtWorkingAge2 <- (hve3$X.MembersAtWorkingAge)
hve3 <- cbind(hve3, X.MembersAtWorkingAge2)
X.MembersAtWorkingAge2[X.MembersAtWorkingAge2 == "0"] <- "1"
X.MembersAtWorkingAge2 <- as.numeric(X.MembersAtWorkingAge2)
Dependency.Ratio <- (hve3$X.Dependents/X.MembersAtWorkingAge2) 
hve3 <- cbind(hve3,Dependency.Ratio)


#Savings Per Family Member:
Saving.Per.Family.Member <- (hve3$Poverty...Coping.Strategies.IF.Saving.how.much../hve3$Household.information.Family.Size)
hve3 <- cbind(hve3,Saving.Per.Family.Member)


#Disability Age Related:
Age.Related.Disability <- (hve3$Age...Disability.Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.impairments.dishabilles..Yes+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Pregnant.females.with.complications..UNHCR.+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Visual.hearing.impairment+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.physical.disability+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Mental.disability+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Intellectual.disability+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Temporary.injured+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Chronically.ill.or.serious.medical.condition+hve3$Age...Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.people.in.need.of.support.to.do.daily.activities)
Age.Related.Disability.Per.Capita <- (Age.Related.Disability/hve3$Household.information.Family.Size)
hve3 <- cbind(hve3,Age.Related.Disability)


#Sex PA (Replace "M" with 1 and "F" with 0): 
hve3$Sex..PA.<- as.character(hve3$Sex..PA.)
hve3$Sex..PA.[hve3$Sex..PA.=="M"] <- 1
hve3$Sex..PA.[hve3$Sex..PA.=="F"] <- 0
Sex..PA. <- (hve3$Sex..PA.)
hve3 <- cbind(hve3,Sex..PA.)


drop2 <- c("CoO_L1","ProcessingGroupNumber","Volunteer..IRD.List.Number..","Household.information.address..","Household.information.UNHCR.File.Number")
hve4 <- hve3[,!(names(hve3) %in% drop2)]
View(hve4)





##########################################################################
#PACKAGES TO ATTACH FOR ANALYSIS:
##########################################################################
#install.packages("car")
#install.packages("tseries")
#install.packages("lme4")
#install.packages("lmtest")

library(car)
library(tseries)
library(lme4)
library(lmtest)



##########################################################################
#REGRESSION MODELS:
##########################################################################
#Preliminary Welfare Model:
reg <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared)
#Preliminary Welfare Model 2:
reg2 <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared+hve3$Payment.Type.of.occupancy..For.rent+hve3$Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash)
#Bonferroni outliers:  
outlierTest(reg2)
#Leverage & Hat-values outliers:
K<-length(coef(reg2))
N<-nrow(reg2$model)
avghat<-K/N
unname(which(hatvalues(reg2)>2*avghat))
#Cooks Distance outliers:
unname(which(cooks.distance(reg2)>4/reg2$df.residual))
#Matching Outliers:
outlier <- hve3[-c(4247,2444,4167,823,616,2716,4210,733),]
#THE WELFARE MODEL: 7 Variables = 55.87  
reg <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+outlier$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+outlier$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared, data=outlier)
summary(reg)
#THE WELFARE MODEL: 9 Variables = 57.13
reg2 <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+outlier$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+outlier$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared+Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash+Payment.Type.of.occupancy..For.rent, data=outlier)
summary(reg2)


##########################################################################
#PROBIT MODEL:
##########################################################################
#Poor Variable (if expenditure per capita less than or equal to 50 then 1, otherwise 0):
#Probit: 7 Variables:
Poor<-ifelse(Expenditure.Per.Capita<=50, 1, 0)
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared)
probit<-glm(Y~X, family=binomial (link="probit"), data=outlier)
summary(probit)
table(true = Y, pred = round(fitted(probit)))  
#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash, Payment.Type.of.occupancy..For.rent)

probit2<-glm(Y~X, family=binomial (link="probit"), data=outlier)
summary(probit2)
table(true = Y, pred = round(fitted(probit2)))


##########################################################################
#LOGIT MODEL:
##########################################################################
#Logit: 7 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, data=outlier)
logit<-glm(Poor~House.Crowding+House.Crowding.Squared+Coping.Strategies.Basic.Food.Needs+Coping.Strategies.Basic.Food.Needs.Squared+hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian.+Saving.Per.Family.Member+Debt.To.Expenditure+Income.Per.Capita+Income.Per.Capita.Squared+hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..+Family.Size.All.File.Numbers.Squared, family=binomial (link="logit"), data=outlier)
summary(logit)
table(true = Y, pred = round(fitted(logit)))  
#Probit: 9 Variables:
Y <- cbind(Poor)
X <- cbind(House.Crowding, House.Crowding.Squared, Coping.Strategies.Basic.Food.Needs, Coping.Strategies.Basic.Food.Needs.Squared, hve3$Poverty...Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months...Select.all.that.apply...Living.together.with.host.family..Jordanian...Syrian., Saving.Per.Family.Member, Debt.To.Expenditure, Income.Per.Capita, Income.Per.Capita.Squared, hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.., Family.Size.All.File.Numbers.Squared, Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash, Payment.Type.of.occupancy..For.rent, data=outlier)
logit2<-glm(Y~X, family=binomial (link="logit"), data=outlier)
summary(logit2)
table(true = Y, pred = round(fitted(logit2)))

##########################################################################











