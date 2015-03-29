################################################################
#### Importe data, Rename variables & Clean rows
source("code/import_clean_rename_homevisit3.R")


#Eliminate Columns with Arabic Text (56 Columns):
drop<-c("Date.of.Visit.","Volunteer..Name.","Volunteer..Phone.Number.","Household.information.Name.of.Principal.Applicant",
        "Household.information.Governorate.","Household.information.District.","Specify.identification.documentations.Relationship",
        "Household.information.Telephone.s..","Household.information.Alternative.phone.s..","Type.of.Housing.IF.other.please.specify..",
        "Payment.Who.","Payment.Other..specify..", "Water.Others.","Financial.Situation.Remittances..where..country..",
        "Financial.Situation.Remittances..whom..relationship..",
        "Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify",
        "Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..","Financial.Situation.specify.other.income.",
        "Financial.Situation.If.there.is.a.gap.between.the.monthly.expenditure.and.the.monthly.income..how.do.you.manage.to.fill.this.gap.",
        "Currently..how.many.of.your.children.do.the.following.Name..",
        "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Name",
        "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Relationship..",
        "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Family.Size",
        "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Age",
        "Information.about.family.members.who.are.living.in.the.same.house.and.NOT.registered.with.UNHCR.Notes",
        "Specify.identification.documentations.UNHCR.File.Number","Type.of.Housing.IF.other.please.specify..",
        "Payment.Who.","Payment.Other..specify..","Water.Others.","Financial.Situation.Remittances..where..country..",
        "Financial.Situation.Remittances..whom..relationship..",
        "Financial.Situation.Income.from.other.organizations.or.charitable.donations...monthly.and.continuously..not.from.UNHCR...From.whom..Other.specify",
        "Financial.Situation.wirte.the.name.of.the.organization.that.gives.you.the.donation..","Financial.Situation.specify.other.income.",
        "Currently..how.many.of.your.children.do.the.following.Name..","Children.Not.Attending.School.If.yes..why..",
        "Number.of.family.members.who.are.going.to.universities.in.Jordan...Name.of.the.universities..",
        "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Name._____________",
        "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Relationship.___________",
        "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.IF.other.please.specify..",
        "Are.your.family.members..in.the.same.file..suffering.from.chronic.diseases.Medical.reports.checked..",
        "Access.to.Health.Services.IF.other.please.specify..","Effect.on.Daily.Activities.Work.If.yes..please.specify..",
        "If.no..specify.family.members.that.separated.and.their.location..IF.other.please.specify..",
        "Documentation...Protection.If.not..what.is.the.reason.","Documentation.IF.other.please.specify..",
        "Working.Family.Member.If.not..what.is.the.reason..","Working.Family.Member.What.kind.of.job..Other..specify",
        "Working.Family.Member.IF.other.please.specify..","Documentation.If.yes..the.date.","Specific.Needs.More.explination..",
        "Categorization.of.Vulnerabilities.Notes.on.the.general.previous.situation.of.the.family.CCO",
        "Categorization.of.Vulnerabilities.Notes.on.the.general.situation.of.the.family.from.the.moment.of.their.arrival.to.Jordan.until.now",
        "Name.of.Principal.Applicant","X.General.remarkes.unreachable"
)

homevisit_noarabic <-homevisit[,!(names(homevisit) %in% drop)]
#View(hve2)