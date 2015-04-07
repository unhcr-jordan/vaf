##########################################################################
#### Import data & Rename variable
#source("code/0_import_clean_rename_homevisit3.R")


##########################################################################
# Creation of composite Variable
##########################################################################

## Coding style guide
#  http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


##########################################################################
#DATA CLEANING:
##########################################################################
rm(hve)
hve <- homevisit


##########################################################################
#VARIABLE GENERATION (including COMPOSITE INDICES):
##########################################################################



#Expenditure Per Capita:
hve$Expenditure.Per.Capita <- (hve$Financial.Situation.Total.Expenditure /
                                 hve$Household.information.Family.Size)

## Class Severe <28 ; High <68; Moderate < 100;  Low > 100;
hve$Expenditure.Per.Capita.class <- as.factor(findCols(classIntervals(hve$Expenditure.Per.Capita, n = 4, style = "fixed", fixedBreaks = c(-105, 28, 68 , 100, 1000))))
hve$Expenditure.Per.Capita.class <- revalue(hve$Expenditure.Per.Capita.class, c(`1` = "Severe", `2` = "High", `3` = "Moderate", `4` = "Low"))
hve$Expenditure.Per.Capita.class  <- factor(hve$Expenditure.Per.Capita.class, levels = c("Severe", "High", "Moderate", "Low"))


## Additionnal data cleaning
hve <- hve [!(hve$Expenditure.Per.Capita == 0),]
hve <- hve [!(hve$Expenditure.Per.Capita > 1000),]

#Log Expenditure Per Capita:
hve$ln.Expenditure.Per.Capita <-log(hve$Expenditure.Per.Capita)
hve$ln.Expenditure.Per.Capita[hve$ln.Expenditure.Per.Capita == "-Inf"] <- "0"
hve$ln.Expenditure.Per.Capita <- as.numeric(hve$ln.Expenditure.Per.Capita)

#Income Per Capita:
hve$Income.Per.Capita <- ( as.numeric(hve$Financial.Situation.Total.income..) / as.numeric(hve$Household.information.Family.Size) )
hve$Income.Per.Capita.Squared <- ( as.numeric(hve$Financial.Situation.Total.income..) / as.numeric(hve$Household.information.Family.Size) ) ^2

#Debt To Expenditure (Must Create Duplicate, Replace 0 with 1):
hve$Total.Expenditure2 <- as.numeric(hve$Financial.Situation.Total.Expenditure)
hve$Total.Expenditure2[hve$Total.Expenditure2 == "0"] <- "1"
hve$Total.Expenditure2 <- as.numeric(hve$Total.Expenditure2)

#str(hve$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc)
hve$Debt.To.Expenditure <- ( as.numeric (hve$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                               hve$Total.Expenditure2)



#Debt To Income (Must Create Duplicate, Replace 0 with 1 for INF):
hve$Total.Income2 <- as.numeric(hve$Financial.Situation.Total.income..)
hve$Total.Income2[hve$Total.Income2 == "0"] <- "1"
hve$Total.Income2 <- as.numeric(hve$Total.Income2)

hve$Debt.To.Income <- ( as.numeric (hve$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                          hve$Total.Income2)

#Family Size Squared:
hve$Family.Size <- as.numeric(hve$Household.information.Family.Size)
hve$Family.Size.Squared <- as.numeric(hve$Household.information.Family.Size) ^ 2

#summary(hve$Family.Size)
## Renamed to Home visit Case size squarred

#summary(hve$Household.information.Family.Size)
hve$case.size.vaf <- as.numeric(hve$Household.information.Family.Size)
hve$case.size.vaf.sq <- as.numeric(hve$case.size.vaf) ^ 2
#summary(hve$case.size.vaf)

#View(hve$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)
hve$Family.Size.All.File.Numbers.Squared <- (hve$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)^2



#Coping Strategies to Meet Basic Food Needs:
hve$Coping.Strategies.Basic.Food.Needs <- (   hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Spent.savings.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Spent.savings.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Bought.food.on.credit.or.borrowed.money.to.purchase.food.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Bought.food.on.credit.or.borrowed.money.to.purchase.food.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Reduced.essential.non.food.expenditure.such.as.education..health.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Reduced.essential.non.food.expenditure.such.as.education..health.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.household.goods..jewelry..phone..furniture..electro.domestics..etc..No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sell.productive.assets.or.means.of.transport..sewing.machine..car..wheel.barrow..bicycle..motorbike..etc..No..exhausted.this.strategy +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Since.arriving.in.Jordan..have.you.accepted.high.risk..illegal..socially.degrading.or.exploitive.temporary.jobs.No..exhausted.this.strategy +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.adult.family.members.to.beg.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.adult.family.members.to.beg.No..exhausted.this.strategy +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.children..under.18..family.members.to.beg.Yes +
                                                hve$In.the.past.30.days..has.your.family.applied.any.of.the.below.strategies.to.meet.basic.food.needs...Sent.children..under.18..family.members.to.beg.No..because.I.have.exhausted.this.strategy.already.and.cannot.do.it.anymore
)
hve$Coping.Strategies.Basic.Food.Needs.Squared <- (hve$Coping.Strategies.Basic.Food.Needs)^2

#Coping Strategies Used In Last Six Months:
hve$Coping.Strategies.Used.Last.Six.Months <- (  hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Living.together.with.host.family..Jordanian.and.Syrian +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Sharing.costs.with.host.family..Jordanian.and.Syrian +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Support.from.family.members..irregular.remittances +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Support.from.host.community..Jordanian +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Humanitarian.assistance..CBOs..personal.donations..etc +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.properties..jewelry..car..etc +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.food.vouchers +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Selling.household.assets +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Borrowing.money +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Buying.against.credit +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Dropping.children.out.from.school +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Child.labor....16.years +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Begging +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Savings +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Irregular.work +
                                                   hve$Poverty.and.Coping.Strategies.What.are.the.coping.strategies.that.you.used.in.the.last.six.months..Select.all.that.apply.....Have.not.paid.the.rent.for.the.past.months
)
hve$Coping.Strategies.Used.Last.Six.Months.Squared <- (hve$Coping.Strategies.Used.Last.Six.Months)^2

#House Assets:
hve$House.Assets <- (hve$Type.of.Housing.Access.to.kitchen..Yes +
                       hve$Type.of.Housing.Access.to.sanitary.facilities..Yes +
                       hve$Type.of.Housing.Ventilation..Yes +
                       hve$Type.of.Housing.Access.to.electricity..Yes
)
hve$House.Assets.To.Family.Size <- (hve$House.Assets / hve$Household.information.Family.Size)


#House Poor Conditions Observed:
hve$House.Poor.Conditions <- (  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Damp.walls +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Leaking.roofs +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Hygienic.concerns +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Privacy.concern +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Rodents +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..Poor.insulation..winter.and.summer. +
                                  hve$Type.of.Housing.Please.specify.if.any.of.the.following.is.observed..broken.windows
)
#NO: hpcfs<-(House.Poor.Conditions/hve$Household.information.Family.Size) #NO

#Chronic Diseases in Family:

#summary(hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Critical.medical.condition..life..threatening..)

hve$Chronic.Diseases <- (   hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Hypertension +
                              hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Diabetes +
                              hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Cardiovascular +
                              #  hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Critical.medical.condition..life..threatening.. +
                              hve$Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.If.yes..specify...Other..specify
)
#NO: cdfs<-(Chronic.Diseases/hve$Household.information.Family.Size) #ok


#Vaccinations Not Received:
#View(hve$Vaccination.If.you.have.children.under.2.years..have.they.received.routine.vaccines..EPI..proxy...question.such.as..do.you.have.children.under.2.who.have.a.vaccination.card....Yes)
hve$Vaccinations.Not.Received <- (  hve$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.measles...Yes +
                                      hve$Vaccination.Do.you.have.a.child.under.5.years.who.was.not.immunized.for.polio..child.who.never.had.a.polio.dose..Yes +
                                      hve$Vaccination.If.you.have.children.under.2.years..have.they.received.routine.vaccines..EPI..proxy...question.such.as..do.you.have.children.under.2.who.have.a.vaccination.card....Yes
)

#Disabilities in Family:
hve$Disability.In.Family <- (  hve$Age.and.Disability.details.Age...0..17.years.old +
                                 hve$Age.and.Disability.details.Age...18..60.years.old +
                                 hve$Age.and.Disability.details.Age...61.years.and.above
)

#House Luxury Assets:
#names(hve)
hve$House.Luxury.Assets <- (   hve$Type.of.Housing.Assets...Floor.mattress +
                                 hve$Type.of.Housing.Assets...Sofa.set +
                                 hve$Type.of.Housing.Assets...Kitchen.utilities +
                                 hve$Type.of.Housing.Assets...Computer +
                                 hve$Type.of.Housing.Assets...Blankets +
                                 hve$Type.of.Housing.Assets...Stove +
                                 hve$Type.of.Housing.Assets...Washing.machine +
                                 hve$Type.of.Housing.Assets...Table.chairs +
                                 hve$Type.of.Housing.Assets...Cabinets +
                                 hve$Type.of.Housing.Assets...Fridge +
                                 hve$Type.of.Housing.Assets...Television +
                                 hve$Type.of.Housing.Assets...Water.heater +
                                 hve$Type.of.Housing.Assets...Freezer +
                                 hve$Type.of.Housing.Assets...Other.specify
)
#NO: lafs<-(hve$House.Luxury.Assets/hve$Household.information.Family.Size)


#House Crowding (People per Room):
hve$House.Crowding <- ( as.numeric(hve$Household.information.Family.Size) /
                          as.numeric(hve$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.))
hve$House.Crowding.Squared<-(hve$House.Crowding)^2


#Alternative, but not as good:
hve$household.House.Crowding <- ( hve$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file.. /
                                    hve$Type.of.Housing.Number.of.rooms.excluding.the.kitchen.and.WASH.facilities.)
hve$household.House.Crowding.squared <- ( hve$House.Crowding )^2

#House Crowding Version 2 (Area per Person):
hve$household.House.Crowding <- ( hve$Type.of.Housing.Total.area.excluding.the.kitchen.and.WASH.facilities..Sq..meter.. /
                                    hve$Type.of.Housing.Number.of.family.members.in.the.house..both.in.the.same.file.number.or.in.another.file..)
#ISSUE: Reported value of ZERO exists for both variables in data
hve$household.House.Crowding[hve$household.House.Crowding == "Inf"] <- "0"
hve$household.House.Crowding[hve$household.House.Crowding == "NaN"] <- "0"
hve$household.House.Crowding <- as.numeric(hve$household.House.Crowding)
hve$household.House.Crowding.Squared <- (hve$household.House.Crowding)^2


#Savings Per Family Member:
hve$Saving.Per.Family.Member <- (hve$Poverty.and.Coping.Strategies.IF.Saving.how.much. /
                                   hve$Household.information.Family.Size)

#Disability Age Related:
hve$Age.Related.Disability <- (  hve$Age.and.Disability.Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases..impairments..dishabilles...Yes +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Pregnant.females.with.complications..UNHCR.. +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Visual..hearing.impairment +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.physical.disability +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Mental.disability +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Intellectual.disability +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Temporary.injured +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Chronically.ill.or.serious.medical.condition +
                                   hve$Age.and.Disability.details.How.many.of.the.following.are.part.of.your.family..in.the.same.file....Other.people.in.need.of.support.to.do.daily.activities
)
hve$Age.Related.Disability.Per.Capita <- ( hve$Age.Related.Disability / as.numeric(hve$Household.information.Family.Size))



# Debt to Expenditure
hve$Debt.To.Expenditure <- (as.numeric(hve$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                              hve$Financial.Situation.Total.Expenditure)


# Debt Per Capita:
hve$Debt.Per.Capita <- (as.numeric(hve$Poverty.and.Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now..This.should.include.not.paying.the.rent..etc) /
                          hve$Household.information.Family.Size)



# Rent occupancy:
hve$Rent.Occupancy <- ifelse(hve$Payment.Type.of.occupancy..For.rent == "1", 1, 0)

# Rent occupancy sub groups
hve$Rent.Occupancy.grp1 <- ifelse(hve$Payment.Type.of.occupancy..For.rent == 1, 1, 0)
hve$Rent.Occupancy.grp1[is.na(hve$Rent.Occupancy.grp1)] <- 0

hve$Rent.Occupancy.grp2 <- ifelse(hve$Payment.Type.of.occupancy..Shelter.provided.through.humanitarian.assistance..donation == 1, 1, 0)
hve$Rent.Occupancy.grp2[is.na(hve$Rent.Occupancy.grp2)] <- 0

hve$Rent.Occupancy.grp3 <- ifelse(hve$Payment.Type.of.occupancy..Hosted..for.free.. == 1, 1, 0)
hve$Rent.Occupancy.grp3[is.na(hve$Rent.Occupancy.grp3)] <- 0

hve$Rent.Occupancy.grp4 <- ifelse(hve$Payment.Type.of.occupancy..Shelter.provided.in.return.for.work..in.a.farm..as.a.guard..etc.. == 1, 1, 0)
hve$Rent.Occupancy.grp4[is.na(hve$Rent.Occupancy.grp4)] <- 0

hve$Rent.Occupancy.grp5 <- ifelse(hve$Payment.Type.of.occupancy..Squatter..illegal.occupation.of.someone.elses.house..land.. == 1, 1, 0)
hve$Rent.Occupancy.grp5[is.na(hve$Rent.Occupancy.grp5)] <- 0


# Spices & Condiments
hve$Spices.And.Condiments.Bought.With.Cash <- ifelse(hve$Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0..7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices.and.condiment.bought.with.cash == "1", 1, 0)



# hve$house.assets <- (  hve$Type.of.Housing.Assets...Floor.mattress,
#                                  hve$Type.of.Housing.Assets...Sofa.set,
#                                  hve$Type.of.Housing.Assets...Kitchen.utilities,
#                                  hve$Type.of.Housing.Assets...Computer,
#                                  hve$Type.of.Housing.Assets...Blankets,
#                                  hve$Type.of.Housing.Assets...Stove,
#                                  hve$Type.of.Housing.Assets...Washing.machine,
#                                  hve$Type.of.Housing.Assets...Table-chairs,
#                                  hve$Type.of.Housing.Assets...Cabinets,
#                                  hve$Type.of.Housing.Assets...Fridge,
#                                  hve$Type.of.Housing.Assets...Television,
#                                  hve$Type.of.Housing.Assets...Water.heater,
#                                  hve$Type.of.Housing.Assets...Freezer,
#                                  hve$Type.of.Housing.Assets...Other..specify)


### Additional variables --

#  House Assets: Kitchen Access + Sanitary Facilities Access + Ventilation + Electricity Access

#   House Luxury Assets: Floor Mattress + Sofas/Beds + Cabinets + Kitchen utilities + Freezer + Fridge + Television + Water heater + Others + Oven

#   Type of Housing (Poor Conditions): Damp Walls + Leaking Roof + Hygienic Concerns + Privacy Concerns + Rodents + Poor Insulation + Broken Windows

#   Total Amount of NFIs & NCIs Received in last 6 Months: Mattresses + Blankets/Pillows + Detergents/Hygiene Items + Kitchen Equipment + Furniture + Stove + Gas Cylinder + Fan + Cloth/Shoes + Electrical Equipment

#   Total Amount of NFIs & NCIs that are Regular Family Needs: Mattresses + Blankets/Pillows + Detergents/Hygiene Items + Kitchen Equipment + Furniture + Stove + Gas Cylinder + Fan + Cloth/Shoes + Electrical Equipment

#   No Water Coping Strategies (Total Amount of ‘What Did You Do’ in the Past Month When You Didn’t Have Water): Buy from Own Pocket + Borrow From Family or Borrow Money to Buy + Shop Credit + Stay Without + Other

#   Coping Strategies Index (Total Number of Coping Strategies Taken in the Last 6 Months): Living together with host family (Jordanian & Syrian) + Sharing costs with host family (Jordanian & Syrian) + Support from family members (irregular remittances) + Support from host community (Jordanian & Syrian) + Humanitarian assistance (CBOs. personal donations. etc.) + Selling properties (jewelry. car. etc.) + Selling food vouchers + Selling household assets + Borrowing money + Buying against credit + Dropping children out from school + Child labor + Begging + Savings + Irregular Work + Have not paid the rent for the past months

#   WFP Food Consumption Score: Times Consumed Each Product in the Last Week multiplied by its Weight
#   PRODUCTS: Cereals (Weight: 2), White Tubers & Roots (Weight: 2), Vegetables & Leaves (Weight: 1), Fruits (Weight: 1), Meat (Weight: 4), Eggs (Weight: 4), Fish & Seafood (Weight: 4), Pulses, Nuts & Seeds (Weight: 3), Milk & Dairy Products (Weight: 4), Oils & Fats (Weight: 0.5), Sweets (Weight: 0.5), Spices & Condiment (Weight: 0)

#   WFP Coping Strategy Index: 5 Coping Strategies Employed in the Last Week Multiplied by its Weight

#   COPING STRATEGIES: Rely on less preferred and less expensive food (i.e. cheaper lower quality food) (Weight: 1), Borrow food or relied on help from relative(s) or friend(s) (Severity Weight: 2), Reduce number of meals eaten a day (Severity Weight 2), Limit portion size at mealtime (different from above: i.e. less food per meal (Severity Weight: 1), Restrict consumption by adults in order for small children to eat (Severity Weight: 2)
#   Coping Strategies Used to Meet Basic Food Needs (In the Past 30 Days): Spent savings + Bought food on credit or borrowed money to purchase food + Reduced essential non-food expenditure such as education/health + Sell household goods (jewelry, phone, furniture, electro domestics, etc.) + Sell productive assets or means of transport (sewing machine, car, wheel barrow, bicycle, motorbike, etc.) + Since arriving in Jordan have you accepted high risk, illegal, socially degrading or exploitive temporary jobs + Sent adult family members to beg + Sent children (under 18) family members to beg


#   Reasons Children Do Not Attend School: Do not know + Not interested in school + Child marriage/engagement + Child labour + No resources + Distance to school + Issues at School + Safety fears + Other + Do not know if school registration is possible + Were not going to school in Syria + Waiting for return in Syria to register children in school + financial constraints + Expired asylum-seeker certificate + Big gap between last grade in home country and one supposed to attend in Jordan + Don’t have an MOI card + Psychological distress + Disability + Not comfortable with teachers/teaching methods/curriculum + Prefer teachers from same nationality + Difficult dialect + Moving from one house to another + Arrival in Jordan in the middle of academic year

#   Chronic Diseases in Family: Hypertension + Diabetes + Cardiovascular + Critical Medical Conditions + Other

#   Vaccinations NOT received: Measles + Polio + Routine Vaccines (EPI)

#   Age & Disability Amongst Family Members in Same File Number: Chronic diseases/impairments/dishabilles + Pregnant females with complications + Visual/Hearing Impairment + Other physical disability + Mental disability + Intellectual disability + Temporary Injured + Chronically ill or serious medical condition + People in need of support to do daily activities



## Recoding shelter variable - 
hve$shelter <- ""
hve$shelter <- with(hve,
                       ifelse( ( hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Permanent.Shelter..structurally.durable.sound.building.with.permanent.materials..cement.. == 1),
                              paste0("Permanent"), hve$shelter ))
hve$shelter <- with(hve,
                    ifelse( ( hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Transitional.Shelter..caravan..mud.hut..tin.or.wood.structure..scrap.material.. == 1),
                    paste0("Transitional"), hve$shelter ))
hve$shelter <- with(hve,
                    ifelse( ( hve$Type.of.Housing.Type.of.Housing..Based.on.the.volunteers.observations..Temporary..emergency.shelter..tent.. == 1),
                    paste0("Temporary"), hve$shelter ))
View(hve$shelter)

## Recoding latrine variable - 
hve$latrine <- ""
hve$latrine <- with(hve,
                    ifelse( ( hve$Wastewater.What.kind.of.latrine..toilet.facility.does.your.household.use...Improved.latrine.with.cement.slab...flush.latrine == 1),
                            paste0("Improved"), hve$latriner ))
hve$latrine <- with(hve,
                    ifelse( ( hve$Wastewater.What.kind.of.latrine..toilet.facility.does.your.household.use...Traditional.pit.latrine..without.slab..open.pit == 1),
                            paste0("Traditional"), hve$latrine ))
hve$latrine <- with(hve,
                    ifelse( ( hve$Wastewater.What.kind.of.latrine..toilet.facility.does.your.household.use...Open.air == 1),
                            paste0("OpenAir"), hve$latrine ))
View(hve$latrine)





write.csv(hve, file="out/hve.csv")

#names(hve)
