##########################################################################
#### Import data & Rename variable
source("code/0_import_clean_rename_homevisit3.R")


##########################################################################
# Creation of composite Variable
##########################################################################

## Coding style guide
#  http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


##########################################################################
#DATA CLEANING:
##########################################################################

hve3 <- homevisit3


##########################################################################
#VARIABLE GENERATION (including COMPOSITE INDICES):
##########################################################################



#Expenditure Per Capita:
hve3$Expenditure.Per.Capita <- (hve3$Financial.Situation.Total.Expenditure / 
                                  hve3$Household.information.Family.Size)
#Log Expenditure Per Capita:
hve3$ln.Expenditure.Per.Capita <-log(hve3$Expenditure.Per.Capita)
hve3$ln.Expenditure.Per.Capita[hve3$ln.Expenditure.Per.Capita == "-Inf"] <- "0"
hve3$ln.Expenditure.Per.Capita <- as.numeric(hve3$ln.Expenditure.Per.Capita)



#Income Per Capita:
hve3$Income.Per.Capita <- ( as.numeric(hve3$Financial.Situation.Total.income..) / as.numeric(hve3$Household.information.Family.Size) ) 
hve3$Income.Per.Capita.Squared <- ( as.numeric(hve3$Financial.Situation.Total.income..) / as.numeric(hve3$Household.information.Family.Size) ) ^2

#Debt To Expenditure (Must Create Duplicate, Replace 0 with 1):
hve3$Total.Expenditure2 <- as.numeric(hve3$Financial.Situation.Total.Expenditure)
hve3$Total.Expenditure2[hve3$Total.Expenditure2 == "0"] <- "1"
hve3$Total.Expenditure2 <- as.numeric(hve3$Total.Expenditure2)

#str(hve3$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc)
hve3$Debt.To.Expenditure <- ( as.numeric (hve3$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                               hve3$Total.Expenditure2)



#Debt To Income (Must Create Duplicate, Replace 0 with 1 for INF):
hve3$Total.Income2 <- as.numeric(hve3$Financial.Situation.Total.income..)
hve3$Total.Income2[hve3$Total.Income2 == "0"] <- "1"
hve3$Total.Income2 <- as.numeric(hve3$Total.Income2)

hve3$Debt.To.Income <- ( as.numeric (hve3$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                          hve3$Total.Income2)

#Family Size Squared:
hve3$Family.Size <- as.numeric(hve3$Household.information.Family.Size)
hve3$Family.Size.Squared <- as.numeric(hve3$Household.information.Family.Size)^2
#View(hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)
hve3$Family.Size.All.File.Numbers.Squared <- (hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)^2



#Coping Strategies to Meet Basic Food Needs:
hve3$Coping.Strategies.Basic.Food.Needs <- (  hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Spent.savings.Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Spent.savings.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore + 
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Bought.food.on.credit.or.borrowed.money.to.purchase.food.Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Bought.food.on.credit.or.borrowed.money.to.purchase.food.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Reduced.essential.non.food.expenditure.such.as.education..health.Yes + 
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Reduced.essential.non.food.expenditure.such.as.education..health.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore + 
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.adult.family.members.to.beg.Yes + 
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.adult.family.members.to.beg.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore + 
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.children..under.18..family.members.to.beg.Yes +
                                              hve3$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.children..under.18..family.members.to.beg.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore
                                              )
hve3$Coping.Strategies.Basic.Food.Needs.Squared <- (hve3$Coping.Strategies.Basic.Food.Needs)^2

#Coping Strategies Used In Last Six Months:
hve3$Coping.Strategies.Used.Last.Six.Months <- (  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian + 
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Sharing.costs.with.host.family..Jordanian.and.Syrian + 
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Support.from.family.members..irregular.remittances + 
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Support.from.host.community..Jordanian +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Humanitarian.assistance..CBOs..personal.donations..etc +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.properties..jewelry..car..etc +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.food.vouchers +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.household.assets +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Borrowing.money +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Buying.against.credit + 
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Dropping.children.out.from.school +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Child.labor....16.years +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Begging +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Savings +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Irregular.work +
                                                  hve3$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Have.not.paid.the.rent.for.the.past.months
                                                  )
hve3$Coping.Strategies.Used.Last.Six.Months.Squared <- (hve3$Coping.Strategies.Used.Last.Six.Months)^2



#House Assets:
hve3$House.Assets <- (hve3$Type.of.Housing.Access.to.kitchen..Yes +
                      hve3$Type.of.Housing.Access.to.sanitary.facilities..Yes +
                      hve3$Type.of.Housing.Ventilation..Yes +
                      hve3$Type.of.Housing.Access.to.electricity..Yes
                      )
hve3$House.Assets.To.Family.Size <- (hve3$House.Assets / hve3$Household.information.Family.Size)


#House Poor Conditions Observed:
hve3$House.Poor.Conditions <- (  hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Damp.walls + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Leaking.roofs + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Hygienic.concerns + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Privacy.concern + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Rodents + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Poor.insulation..winter.and.summer. + 
                                 hve3$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..broken.windows
                               )
#NO: hpcfs<-(House.Poor.Conditions/hve3$Household.information.Family.Size) #NO

#Chronic Diseases in Family:
hve3$Chronic.Diseases <- (  hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Hypertension +
                            hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Diabetes +
                            hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Cardiovascular +
                            hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Critical.medical.condition..life..threatening.. +
                            hve3$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Other..specify
                            )
#NO: cdfs<-(Chronic.Diseases/hve3$Household.information.Family.Size) #ok


#Vaccinations Not Received:
#View(hve3$Vaccination.If.you.have.children.under.2.years..have.they.received.routine.vaccines..EPI..proxy...question.such.as..do.you.have.children.under.2.who.have.a.vaccination.card....Yes)
hve3$Vaccinations.Not.Received <- (  hve3$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.measles...Yes +
                                     hve3$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.polio..child.who.never.had.a.polio.dose..Yes +
                                     hve3$Vaccination.If.you.have.children.under.2.years..have.they.received.routine.vaccines..EPI..proxy...question.such.as..do.you.have.children.under.2.who.have.a.vaccination.card....Yes
                                     )

#Disabilities in Family:
hve3$Disability.In.Family <- (  hve3$Age.and.Disability.details.Age...0..17.years.old +
                                hve3$Age.and.Disability.details.Age...18..60.years.old +
                                hve3$Age.and.Disability.details.Age...61.years.and.above
                                )

#House Luxury Assets:
#names(hve3)
hve3$House.Luxury.Assets <- (  hve3$Type.of.Housing.Assets...Floor.mattress +
                               hve3$Type.of.Housing.Assets...Sofa.set +
                               hve3$Type.of.Housing.Assets...Kitchen.utilities + 
                               hve3$Type.of.Housing.Assets...Computer +
                               hve3$Type.of.Housing.Assets...Blankets +
                               hve3$Type.of.Housing.Assets...Stove +
                               hve3$Type.of.Housing.Assets...Washing.machine +
                               hve3$Type.of.Housing.Assets...Table.chairs +
                               hve3$Type.of.Housing.Assets...Cabinets +
                               hve3$Type.of.Housing.Assets...Fridge +
                               hve3$Type.of.Housing.Assets...Television +
                               hve3$Type.of.Housing.Assets...Water.heater +
                               hve3$Type.of.Housing.Assets...Freezer +
                               hve3$Type.of.Housing.Assets...Other.specify
                               )
#NO: lafs<-(hve3$House.Luxury.Assets/hve3$Household.information.Family.Size) 


#House Crowding (People per Room):
hve3$House.Crowding <- ( as.numeric(hve3$Household.information.Family.Size) /
                         as.numeric(hve3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.))
hve3$House.Crowding.Squared<-(hve3$House.Crowding)^2

#Alternative, but not as good:
hve3$House.Crowding <- ( hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. /
                           hve3$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)
hve3$House.Crowding.squared <- ( hve3$House.Crowding )^2

#House Crowding Version 2 (Area per Person):
hve3$House.Crowding.v2 <- ( hve3$Type.of.Housing.Total.area.excluding.the.kitchen.and.WASH.facilities..Sq..meter.. /
                            hve3$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)
#ISSUE: Reported value of ZERO exists for both variables in data
hve3$House.Crowding.v2[hve3$House.Crowding.v2 == "Inf"] <- "0"
hve3$House.Crowding.v2[hve3$House.Crowding.v2 == "NaN"] <- "0"
hve3$House.Crowding.v2 <- as.numeric(hve3$House.Crowding.v2)
hve3$House.Crowding.v2.Squared <- (hve3$House.Crowding.v2)^2


#Savings Per Family Member:
hve3$Saving.Per.Family.Member <- (hve3$Poverty.and.Coping.Strategies.IF.Saving.how.much. /
                                    hve3$Household.information.Family.Size)

#Disability Age Related:
hve3$Age.Related.Disability <- (  hve3$Age.and.Disability.Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases..impairments..dishabilles...Yes +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Pregnant.females.with.complications..UNHCR.. +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Visual..hearing.impairment +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.physical.disability +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Mental.disability +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Intellectual.disability +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Temporary.injured +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Chronically.ill.or.serious.medical.condition +
                                  hve3$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.people.in.need.of.support.to.do.daily.activities
                                  )
hve3$Age.Related.Disability.Per.Capita <- (Age.Related.Disability/hve3$Household.information.Family.Size)



# Debt to Expenditure
hve3$Debt.To.Expenditure <- (as.numeric(hve3$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                               hve3$Financial.Situation.Total.Expenditure)


# Debt Per Capita:
hve3$Debt.Per.Capita <- (as.numeric(hve3$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                           hve3$Household.information.Family.Size)

# Family Size
hve3$Family.Size.Squared <- (as.numeric(hve3$Household.information.Family.Size))^2

# Rent
hve3$Rent.Occupancy <- ifelse(hve3$Payment.Type.of.occupancy..For.rent == "1", 1, 0) 

# Spices & Condiments
hve3$Spices.And.Condiments.Bought.With.Cash <- ifelse(hve3$Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash == "1", 1, 0)




##########################################################################################################################
##########################################################################################################################
#Sex PA (Replace "M" with 1 and "F" with 0): 
hve3$Sex..PA.<- as.character(hve3$Sex..PA.)
hve3$Sex..PA.[hve3$Sex..PA.=="M"] <- 1
hve3$Sex..PA.[hve3$Sex..PA.=="F"] <- 0


##################################
#Dependency Ratio (Must create duplicate; Replace 0 with 1 for INF):
View(hve3$MembersAtWorkingAge)
hve3$MembersAtWorkingAge2 <- as.numeric(hve3$MembersAtWorkingAge)
hve3$MembersAtWorkingAge2[hve3$MembersAtWorkingAge2 == "0"] <- "1"
hve3$MembersAtWorkingAge2 <- as.numeric (hve3$MembersAtWorkingAge2)
hve3$Dependency.Ratio <- (hve3$Dependents / hve3$MembersAtWorkingAge2) 


#Education PA (Duplicate Variable and Replace Values):
#Dummy.Uni.PA <- (hve2$EducationLevelCode.PA.)
#hve3 <- cbind(hve2,Dummy.Uni.PA)

hve3$Dummy.Uni.PA <- ""

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
#EducationLevelCode.PA. <- hve3$EducationLevelCode.PA.

