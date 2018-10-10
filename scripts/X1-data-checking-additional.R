#' This file includes additional data integrity checks that were preforemd
#' throughout the analysis and report writing
#' This file does not in any way change the data, that is all done in 01 and 02
#' but outcomes from this file explain some of the changes to the 
#' data done in 02. 

load("data/outputs/clean.Rdata")
library(dplyr)

# is the number of unique hhs the same as the number of respondents

length(unique(ds.mmbr$hh.id))
sum(ds.mmbr$n4 == 1)

# No, it's not, there are two respondents missing. 

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  summarise(respondent = first(n4)) %>% 
  filter(respondent != "Respondent")

# ok, first of all there are ten householdsd where they just didn't list the respondent as the first one:

ds.mmbr[ds.mmbr$hh.id == 28,]
ds.hohh[ds.hohh$hh.id == 28,]
# This is an error, fixed now in 02-data-clean

ds.mmbr[ds.mmbr$hh.id == 34,]
# this one just ahs the order wrong, although it is a bit suspicions that the husband is 29 years younger?
ds.hohh[ds.hohh$hh.id == 34,]
# actually based on this it looks like she is a widow, so that guy is not her spouse, but more likely son, 
# since her daugter in law is in teh house... Fixed in 02-data-clean

ds.mmbr[ds.mmbr$hh.id == 117,]
ds.hohh[ds.hohh$hh.id == 117,]
# OK, just swapped
ds.mmbr[ds.mmbr$hh.id == 118,]
ds.hohh[ds.hohh$hh.id == 118,]
# mistake, 370 is actually the respondent, fixed in 02-data-clean

ds.mmbr[ds.mmbr$hh.id == 121,]
ds.hohh[ds.hohh$hh.id == 121,]
ds.mmbr[ds.mmbr$hh.id == 121,]
ds.hohh[ds.hohh$hh.id == 121,]
ds.mmbr[ds.mmbr$hh.id == 238,]
ds.hohh[ds.hohh$hh.id == 238,]
ds.mmbr[ds.mmbr$hh.id == 353,]
ds.hohh[ds.hohh$hh.id == 353,]
ds.mmbr[ds.mmbr$hh.id == 390,]
ds.hohh[ds.hohh$hh.id == 390,]
ds.mmbr[ds.mmbr$hh.id == 393,]
ds.hohh[ds.hohh$hh.id == 393,]
ds.mmbr[ds.mmbr$hh.id == 398,]
ds.hohh[ds.hohh$hh.id == 398,]
# OK, just swapped


# What are the numbers of respondents, other people in the same 
# household, and other members not living there anymore. For total and for women over 50. 

nrow(ds.mmbr)
nrow(ds.mmbr[ds.mmbr$n1 == 1,])
nrow(ds.mmbr[ds.mmbr$n1 == 1 & ds.mmbr$n4 == 1,])
nrow(ds.mmbr[ds.mmbr$n1 == 1 & ds.mmbr$n4 != 1,])
nrow(ds.mmbr[ds.mmbr$n1 == 2 & ds.mmbr$n4 != 1,])
nrow(ds.mmbr[ds.mmbr$n1 == 2 & ds.mmbr$n4 == 1,])

ds.hohh[ds.hohh$a1 == "Female"& ds.hohh$a2 <= 1966, ] -> ds.hohh.w.50
ds.mmbr[ds.mmbr$hh.id %in% ds.hohh.w.50$hh.id , ] -> ds.mmbr.w.50

nrow(ds.mmbr.w.50)
nrow(ds.mmbr.w.50[ds.mmbr.w.50$n1 == 1,])
nrow(ds.mmbr.w.50[ds.mmbr.w.50$n1 == 1 & ds.mmbr.w.50$n4 == 1,])
nrow(ds.mmbr.w.50[ds.mmbr.w.50$n1 == 1 & ds.mmbr.w.50$n4 != 1,])
nrow(ds.mmbr.w.50[ds.mmbr.w.50$n1 == 2 & ds.mmbr.w.50$n4 != 1,])
nrow(ds.mmbr.w.50[ds.mmbr.w.50$n1 == 2 & ds.mmbr.w.50$n4 == 1,])

