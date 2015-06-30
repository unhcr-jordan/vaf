
#####


## Checking duplicate unhcr file number in home visit
homevisit.uniquecase <- unique(homevisit$Household.information.UNHCR.File.Number) ##8353

progress.vaf <- as.data.frame(homevisit$Household.information.UNHCR.File.Number)

# Show the repeated progress case entries
progress.vaf.dup <- as.data.frame(progress.vaf[duplicated(progress.vaf),])
progress.vaf.dup <- rename(progress.vaf.dup, c("progress.vaf[duplicated(progress.vaf), ]" ="caseid"))
progress.vaf.dup <- progress.vaf.dup[order(caseid),] 
# Show unique repeat entries 
progress.vaf.dup.un <-unique(homevisit[duplicated(homevisit$progress.vaf),])