# VAF Baseline Survey
# VAF version 4
# Author: Marco Santacroce

# Random selection
# Proportional regional allocation

# Import data:
vafbaseline <- read.csv("C:/Users/SANTACRO/Desktop/Baseline Surveys/vafbaseline.csv")

# Replace Null with 0 in data.frame:
vafbaseline[is.na(vafbaseline)] <- 0

# Clean:
vafbaseline <- vafbaseline [!(vafbaseline$Volunteer..Case.Status...Available==0),]

vafbaseline <- vafbaseline [!(vafbaseline$Household.information.Family.Size==0),]
vafbaseline <- vafbaseline [!(vafbaseline$Household.information.Family.Size >= 20),]

vafbaseline <- vafbaseline [!(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.== 0),]
vafbaseline <- vafbaseline [!(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities. >= 10),]

vafbaseline <- vafbaseline [!(vafbaseline$Financial.Situation.Total.Expenditure==0),]

# Create Regional Dummies:
north <- ifelse((vafbaseline$Household.information.Governorate..Irbid == "1" | vafbaseline$Household.information.Governorate..Ajloun == "1" | vafbaseline$Household.information.Governorate..Jerash == "1"), 1, 0)
center <- ifelse((vafbaseline$Household.information.Governorate..Amman == "1" | vafbaseline$Household.information.Governorate..Balqa == "1" | vafbaseline$Household.information.Governorate..Madaba == "1" | vafbaseline$Household.information.Governorate..Zarqa == "1"), 1, 0)
east <- ifelse((vafbaseline$Household.information.Governorate..Mafraq == "1"), 1, 0)
south <- ifelse((vafbaseline$Household.information.Governorate..Karak == "1" | vafbaseline$Household.information.Governorate..Tafileh == "1" | vafbaseline$Household.information.Governorate..Ma.an == "1" | vafbaseline$Household.information.Governorate..Aqaba == "1"), 1, 0)

# Bind regional dummies:
vafbaseline <- cbind(vafbaseline, north)
vafbaseline <- cbind(vafbaseline, center)
vafbaseline <- cbind(vafbaseline, east)
vafbaseline <- cbind(vafbaseline, south)

# Expenditure
vafbaseline$Financial.Situation.Total.Expenditure <- as.numeric(vafbaseline$Financial.Situation.Total.Expenditure) # can't generate quotient variable if factor (and other)
vafbaseline$Household.information.Family.Size <- as.numeric(vafbaseline$Household.information.Family.Size)
Expenditure.Per.Capita <- (vafbaseline$Financial.Situation.Total.Expenditure/vafbaseline$Household.information.Family.Size)
vafbaseline <- cbind(vafbaseline, Expenditure.Per.Capita) # necessary for subset regression

# Income
vafbaseline$Financial.Situation.Total.income.. <- as.numeric(vafbaseline$Financial.Situation.Total.income..)
Income.Per.Capita <- (vafbaseline$Financial.Situation.Total.income../vafbaseline$Household.information.Family.Size)
Income.Per.Capita.Squared <- (vafbaseline$Financial.Situation.Total.income../vafbaseline$Household.information.Family.Size)^2
vafbaseline <- cbind(vafbaseline, Income.Per.Capita) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, Income.Per.Capita.Squared) # necessary for subset regression

# House Crowding
vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities. <- as.numeric(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.)
House.Crowding <- (vafbaseline$Household.information.Family.Size/vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.facilities.)
House.Crowding.Squared <- (House.Crowding)^2
vafbaseline <- cbind(vafbaseline, House.Crowding) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, House.Crowding.Squared) # necessary for subset regression

# Debt to Expenditure
vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc.. <- as.numeric(vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc..)
Debt.To.Expenditure <- (vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc../vafbaseline$Financial.Situation.Total.Expenditure)
vafbaseline <- cbind(vafbaseline, Debt.To.Expenditure) # necessary for subset regression

# Debt Per Capita:
Debt.Per.Capita <- (vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up.to.now...This.should.include.not.paying.the.rent..etc../vafbaseline$Household.information.Family.Size)
vafbaseline <- cbind(vafbaseline, Debt.Per.Capita) # necessary for subset regression

# Family Size
Family.Size <- (vafbaseline$Household.information.Family.Size)
Family.Size.Squared <- (vafbaseline$Household.information.Family.Size)^2
vafbaseline <- cbind(vafbaseline, Family.Size) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, Family.Size.Squared) # necessary for subset regression

# Rent
Rent.Occupancy <- ifelse(vafbaseline$Payment.Type.of.occupancy..For.rent == "1", 1, 0) 
vafbaseline <- cbind(vafbaseline, Rent.Occupancy) # necessary for subset regression

# Spices & Condiments
Spices.And.Condiments.Bought.With.Cash <- ifelse(vafbaseline$Over.the.last.7.days..how.many.days.did.you.consume.the.following.foods..0.7..What.was.the.main.source.of.the.food.in.the.past.7.days..Spices...condiment.bought.with.cash == "1", 1, 0)
vafbaseline <- cbind(vafbaseline, Spices.And.Condiments.Bought.With.Cash) # necessary for subset regression

# Regressions Full:
reg.full <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.To.Expenditure+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy)
summary(reg.full) 

reg.full.2 <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy)
summary(reg.full.2) 

# Regressions Regional:
reg.north <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy, subset(vafbaseline, north == "1"))
summary(reg.north) 

reg.center <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy, subset(vafbaseline, center == "1"))
summary(reg.center) 

reg.east <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy, subset(vafbaseline, east == "1"))
summary(reg.east) 

reg.south <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy, subset(vafbaseline, south == "1"))
summary(reg.south) 

# Correlations:
cor(Expenditure.Per.Capita,House.Crowding) # -0.433744
cor(Expenditure.Per.Capita,Income.Per.Capita) # 0.5958559
cor(Expenditure.Per.Capita,Debt.To.Expenditure) # -0.08116488
cor(Expenditure.Per.Capita,Debt.Per.Capita) # 0.3733538
cor(Expenditure.Per.Capita,Family.Size) # -0.5351606
cor(Expenditure.Per.Capita,Spices.And.Condiments.Bought.With.Cash) # 0.1835674
cor(Expenditure.Per.Capita,Rent.Occupancy) # 0.1205072

# Correlations Squared:
cor(Expenditure.Per.Capita,House.Crowding) # 0.1881
cor(Expenditure.Per.Capita,Income.Per.Capita) # 0.3550443
cor(Expenditure.Per.Capita,Debt.To.Expenditure) # 0.006587738
cor(Expenditure.Per.Capita,Debt.Per.Capita) # 0.1393931
cor(Expenditure.Per.Capita,Family.Size) # 0.2863969
cor(Expenditure.Per.Capita,Spices.And.Condiments.Bought.With.Cash) # 0.03369699
cor(Expenditure.Per.Capita,Rent.Occupancy) # 0.01452199

# Gender Breakdown:

# Analysis 1: Male:
vafbaseline <- read.csv("C:/Users/SANTACRO/Desktop/Baseline Surveys/Male Stratum.csv")
# as dataframe same name, repeat coding above, following Access Query to match Gender with cases, headers name shortened

# Analysis 2: Female:
vafbaseline <- read.csv("C:/Users/SANTACRO/Desktop/Baseline Surveys/Female Stratum.csv")
# as dataframe same name, repeat coding above, following Access Query to match Gender with cases, headers name shortened

# Replace Null with 0 in data.frame:
vafbaseline[is.na(vafbaseline)] <- 0
attach(vafbaseline)

# Clean:
vafbaseline <- vafbaseline [!(vafbaseline$Volunteer..Case.Status...Available==0),]

vafbaseline <- vafbaseline [!(vafbaseline$Household.information.Family.Size==0),]
vafbaseline <- vafbaseline [!(vafbaseline$Household.information.Family.Size >= 20),]

vafbaseline <- vafbaseline [!(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.fac== 0),]
vafbaseline <- vafbaseline [!(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.fac >= 10),]

vafbaseline <- vafbaseline [!(vafbaseline$Financial.Situation.Total.Expenditure==0),]

# Expenditure
vafbaseline$Financial.Situation.Total.Expenditure <- as.numeric(vafbaseline$Financial.Situation.Total.Expenditure) # can't generate quotient variable if factor (and other)
vafbaseline$Household.information.Family.Size <- as.numeric(vafbaseline$Household.information.Family.Size)
Expenditure.Per.Capita <- (vafbaseline$Financial.Situation.Total.Expenditure/vafbaseline$Household.information.Family.Size)
vafbaseline <- cbind(vafbaseline, Expenditure.Per.Capita) # necessary for subset regression

# Income
vafbaseline$Financial.Situation.Total.income.. <- as.numeric(vafbaseline$Financial.Situation.Total.income..)
Income.Per.Capita <- (vafbaseline$Financial.Situation.Total.income../vafbaseline$Household.information.Family.Size)
Income.Per.Capita.Squared <- (vafbaseline$Financial.Situation.Total.income../vafbaseline$Household.information.Family.Size)^2
vafbaseline <- cbind(vafbaseline, Income.Per.Capita) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, Income.Per.Capita.Squared) # necessary for subset regression

# House Crowding
vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.fac <- as.numeric(vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.fac)
House.Crowding <- (vafbaseline$Household.information.Family.Size/vafbaseline$Type.of.Housing.Number.of.rooms.excluding.the.kitchen...WASH.fac)
House.Crowding.Squared <- (House.Crowding)^2
vafbaseline <- cbind(vafbaseline, House.Crowding) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, House.Crowding.Squared) # necessary for subset regression

# Debt to Expenditure
vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up <- as.numeric(vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up)
Debt.To.Expenditure <- (vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up/vafbaseline$Financial.Situation.Total.Expenditure)
vafbaseline <- cbind(vafbaseline, Debt.To.Expenditure) # necessary for subset regression

# Debt Per Capita:
Debt.Per.Capita <- (vafbaseline$Poverty...Coping.Strategies.What.is.your.total.amount.of.debt.up/vafbaseline$Household.information.Family.Size)
vafbaseline <- cbind(vafbaseline, Debt.Per.Capita) # necessary for subset regression

# Family Size
Family.Size <- (vafbaseline$Household.information.Family.Size)
Family.Size.Squared <- (vafbaseline$Household.information.Family.Size)^2
vafbaseline <- cbind(vafbaseline, Family.Size) # necessary for subset regression
vafbaseline <- cbind(vafbaseline, Family.Size.Squared) # necessary for subset regression

# Rent
Rent.Occupancy <- ifelse(vafbaseline$Payment.Type.of.occupancy..For.rent == "1", 1, 0) 
vafbaseline <- cbind(vafbaseline, Rent.Occupancy) # necessary for subset regression

# Spices & Condiments
Spices.And.Condiments.Bought.With.Cash <- ifelse(vafbaseline$Over.the.last.7.days..how.many.days.did.you.consume.the.followin == "1", 1, 0)
vafbaseline <- cbind(vafbaseline, Spices.And.Condiments.Bought.With.Cash) # necessary for subset regression

# Regressions Full:
reg.full <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.To.Expenditure+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy)
summary(reg.full)

reg.full.2 <- lm(Expenditure.Per.Capita~House.Crowding+House.Crowding.Squared+Income.Per.Capita+Income.Per.Capita.Squared+Debt.Per.Capita+Family.Size+Family.Size.Squared+Spices.And.Condiments.Bought.With.Cash+Rent.Occupancy)
summary(reg.full.2) 


