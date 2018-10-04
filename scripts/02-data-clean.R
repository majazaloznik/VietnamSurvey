###############################################################################
## 0. Preliminaries  ##########################################################
###############################################################################
## 1. CONSOLIDATE head of hh  TABLES###########################################
## 1.1. unique ID - hh.id - in ds1 and ds5#####################################
## 1.2. chech ids are good and merge both #####################################
###############################################################################
## 2. CONSOLIDATE household member TABLES######################################
## 2.1. type of hh member## in ds2 and ds3#####################################
## 2.2. rename columns in ds2 and 3 so they match##############################
## 2.3. merge  ds2 and 3 and polish up items ##################################
## 2.4. add household id and member id ########################################
###############################################################################
## 3. add hh ids to the plots #################################################
###############################################################################

## 0. Preliminaries  ##########################################################
###############################################################################
library(memisc)
library(tidyverse)
load("data/outputs/tmp.clean.import.RData")
source("scripts/00-functions.R")

## 1. CONSOLIDATE head of hh  TABLES###########################################
###############################################################################
## 1.1. unique ID - hh.id - in ds1 and ds5#####################################
ds.1$hh.id <- as.item(1:nrow(ds.1))
measurement(ds.1$hh.id) <- "interval"
description(ds.1$hh.id) <- "Household ID - unique"
annotation(ds.1$hh.id)["flag"] <- "Deriv."
annotation(ds.1$hh.id)["origin"] <- "nrows"
ds.1 <- FunSwap(ds=ds.1, New = "hh.id", After =  "hhcode")

ds.5$hh.id <- as.item(1:nrow(ds.5))
measurement(ds.5$hh.id) <- "interval"
description(ds.5$hh.id) <- "ID - unique"
annotation(ds.5$hh.id)["flag"] <- "Deriv."
annotation(ds.5$hh.id)["origin"] <- "nrows"
ds.5 <- FunSwap(ds=ds.5, New = "hh.id", After =  "hhcode")

## 1.2. chech ids are good and merge both #####################################
all.equal(ds.1[1:9], ds.5[1:9])

ds.hohh <- cbind(ds.1,  ds.5[10:122])
colnames(ds.hohh) <- c(colnames(ds.1),  colnames(ds.5[10:122]))

# some descriptions have elipsis need to be removed
description(ds.hohh$g19) <- substr(description(ds.hohh$g19), 1, 52)
description(ds.hohh$g20) <- substr(description(ds.hohh$g20), 1, 47)

# some descriptions are clipped
description(ds.hohh$d1) <- "When you are no longer able to farm rice, what will you do with your rice fields?"
description(ds.hohh$d21) <- "It is the duty of a parent to do his/her best for his/her children even at the expense of their own well-being"
description(ds.hohh$d22) <- "It is the duty of grandparents to be there for their grandchildren in times of difficulty (e.g. illness, divorce)"
description(ds.hohh$d23) <- "It is the duty of grandparents to contribute to the economic security of their family (i.e., their children and their grandchildren)"
description(ds.hohh$e231) <- "Is it done for free?"
description(ds.hohh$e232) <- "Is it done for free?"
description(ds.hohh$e233) <- "Is it done for free?"
description(ds.hohh$e234) <- "Is it done for free?"
description(ds.hohh$e235) <- "Is it done for free?"
description(ds.hohh$e236) <- "Is it done for free?"
description(ds.hohh$d31) <- "	Who should be primarily responsible for	Financial support for older persons in need?"
description(ds.hohh$d32) <- "	Who should be primarily responsible for	Practical help in the home for older persons in need? (e.g. cleaning, shopping, cooking)"
description(ds.hohh$d33) <- "	Who should be primarily responsible for Personal care for older persons in need? (e.g. nursing, bathing, dressing)their children and their grandchildren)?"
description(ds.hohh$d34) <- "	Who should be primarily responsible for Help in farming for older person in need?"
description(ds.hohh$g13) <-  "Does the Co-op/Commune/Plant Protection Agency influence the way in hich you farm?"
description(ds.hohh$h1a) <- "When choosing the sort of rice seed to plant, the three most important things you consider are:"
description(ds.hohh$h1b) <- "When choosing the sort of rice seed to plant, the three most  important things you consider are:"
description(ds.hohh$h1c) <- "When choosing the sort of rice seed to plant, the three most  important things you consider are:"
description(ds.hohh$h2a) <- "The three most important sources of my knowledge of rice seed are "
description(ds.hohh$h2b) <- "The three most important sources of my knowledge of rice seed are "
description(ds.hohh$h2c) <- "The three most important sources of my knowledge of rice seed are "
description(ds.hohh$h3a) <- "When choosing the type of fertilizer to use on my crops, , the three most important things you consider are"
description(ds.hohh$h3b) <- "When choosing the type of fertilizer to use on my crops, , the three most important things you consider are"
description(ds.hohh$h3c) <- "When choosing the type of fertilizer to use on my crops, , the three most important things you consider are"
description(ds.hohh$h4a) <- "The three most important sources of my knowledge of the type of fertilizer to use are"
description(ds.hohh$h4b) <- "The three most important sources of my knowledge of the type of fertilizer to use are"
description(ds.hohh$h4c) <- "The three most important sources of my knowledge of the type of fertilizer to use are"
description(ds.hohh$h5a) <- "When determining when is the best time to use fertilizer, the three most important things you consider are "
description(ds.hohh$h5b) <- "When determining when is the best time to use fertilizer, the three most important things you consider are "
description(ds.hohh$h5c) <- "When determining when is the best time to use fertilizer, the three most important things you consider are "
description(ds.hohh$h6a) <- "The three most important sources of my knowledge of when is the best time to use fertilizer comes are"
description(ds.hohh$h6b) <- "The three most important sources of my knowledge of when is the best time to use fertilizer comes are"
description(ds.hohh$h6c) <- "The three most important sources of my knowledge of when is the best time to use fertilizer comes are"
description(ds.hohh$h7a) <- "When choosing the pesticides to use, the three most important things you consider are"
description(ds.hohh$h7b) <- "When choosing the pesticides to use, the three most important things you consider are"
description(ds.hohh$h7c) <- "When choosing the pesticides to use, the three most important things you consider are"
description(ds.hohh$h8a) <- "The three most important sources of my knowledge of the sort of pesticides to choose are"
description(ds.hohh$h8b) <- "The three most important sources of my knowledge of the sort of pesticides to choose are"
description(ds.hohh$h8c) <- "The three most important sources of my knowledge of the sort of pesticides to choose are"
description(ds.hohh$h9a) <- "When determining when is the best time to use pesticides, the three most important things you consider are"
description(ds.hohh$h9b) <- "When determining when is the best time to use pesticides, the three most important things you consider are"
description(ds.hohh$h9c) <- "When determining when is the best time to use pesticides, the three most important things you consider are"
description(ds.hohh$h10a) <- "The three most important sources of my knowledge of when is the best time to use pesticides are"
description(ds.hohh$h10b) <- "The three most important sources of my knowledge of when is the best time to use pesticides are"
description(ds.hohh$h10c) <- "The three most important sources of my knowledge of when is the best time to use pesticides are"
description(ds.hohh$i1) <- "Do you grow any other rice varieties than the ones recommended by the co-operative?"

description(ds.hohh$i51) <- "Do you think in your commune experimenting with new rice varieties can lead to Higher yields?"
description(ds.hohh$i52) <- "Do you think in your commune experimenting with new rice varieties can lead to Greater use of chemicals?"
description(ds.hohh$i53) <- "Do you think in your commune experimenting with new rice varieties can lead to Fewer pests?"
description(ds.hohh$i54) <- "Do you think in your commune experimenting with new rice varieties can lead to Better rice quality/taste?"

description(ds.hohh$i61) <- "When you personally decide about experimenting with new rice varieties, how important is the effect on Higher yields?"
description(ds.hohh$i62) <- "When you personally decide about experimenting with new rice varieties, how important is the effect on Greater use of chemicals?"
description(ds.hohh$i63) <- "When you personally decide about experimenting with new rice varieties, how important is the effect on Fewer pests?"
description(ds.hohh$i64) <- "When you personally decide about experimenting with new rice varieties, how important is the effect on Better rice quality/taste?"

description(ds.hohh$i71) <- "Do you think in your commune experimenting with new rice varieties is limited by: Insufficient knowledge/information"
description(ds.hohh$i72) <- "Do you think in your commune experimenting with new rice varieties is limited by: Limited access to seeds"
description(ds.hohh$i73) <- "Do you think in your commune experimenting with new rice varieties is limited by: Farmers being afraid of failure"
description(ds.hohh$i74) <- "Do you think in your commune experimenting with new rice varieties is limited by: Field characteristics"

description(ds.hohh$i81) <- "How important is Insufficient knowledge-information, when you decide about experimenting with new rice varieties"
description(ds.hohh$i82) <- "How important is Limited access to seeds, when you decide about experimenting with new rice varieties"
description(ds.hohh$i83) <- "How important is Farmers being afraid of failure, when you decide about experimenting with new rice varieties"
description(ds.hohh$i84) <- "How important are Field characteristics, when you decide about experimenting with new rice varieties"

description(ds.hohh$i91) <- "How much is experimenting with new rice varieties appreciated by your neighbouring farmers"
description(ds.hohh$i92) <- "How much is experimenting with new rice varieties appreciated by your household members"

description(ds.hohh$i101) <- "When you decide about experimenting with new rice varieties, how much do you personally care about the opinion of Neighbouring farmers"
description(ds.hohh$i101) <- "When you decide about experimenting with new rice varieties, how much do you personally care about the opinion of Household members"

description(ds.hohh$k51) <- "When you personally decide whether or not to spray preventatively, how important is its effect on: Pest damage"
description(ds.hohh$k52) <- "When you personally decide whether or not to spray preventatively, how important is its effect on: Crop yield"
description(ds.hohh$k53) <- "When you personally decide whether or not to spray preventatively, how important is its effect on: Levels of chemical pollution"
description(ds.hohh$k54) <- "When you personally decide whether or not to spray preventatively, how important is its effect on: NUmber of 'good animals'"
  
description(ds.hohh$k61) <- "Do you think in your commune correct pesticide application is limited by	Insufficient knowledge/Information of farmers"
description(ds.hohh$k62) <- "Do you think in your commune correct pesticide application is limited by How frequently farmers check their fields"
description(ds.hohh$k63) <- "Do you think in your commune correct pesticide application is limited by	How accurate the public announcements are"
description(ds.hohh$k64) <- "Do you think in your commune correct pesticide application is limited by How afraid farmers are of pest damage"

description(ds.hohh$k71) <-  "How important is Insufficient knowledge-Information of farmers for you personally when you decide about spraying with pesticides"
description(ds.hohh$k72) <-  "How important is How frequently farmers check their fields for you personally when you decide about spraying with pesticides"
description(ds.hohh$k73) <-  "How important is How accurate the public announcements are for you personally when you decide about spraying with pesticides"
description(ds.hohh$k74) <-  "How important is How afraid farmers are of pest damage for you personally when you decide about spraying with pesticides"

description(ds.hohh$k8) <- "How much is correct pesticide application important for the neighbouring farmers"
description(ds.hohh$k9) <- "When you decide about pesticide spraying, how much do you personally care about the opinion of the neightbouring farmers?"

# missing labels
labels(ds.hohh$e221) <- c(Respondent       =  1,
                     Spouse             =  2,
                     Son =  3, 
                     Daughter      =  4,
                     "Son/daughter in law " = 5,
                     "Adopted child" = 6,
                     Parent = 7,
                     "Parent in law" = 8,
                     Grandparent = 9,
                     "(Great) Grandchildren"= 10,
                     Siblings = 11,
                     "Other relatives" = 12,
                     "House servant" = 13,
                     "other" = 96)

labels(ds.hohh$e222) <- labels(ds.hohh$e221)
labels(ds.hohh$e223) <- labels(ds.hohh$e221)
labels(ds.hohh$e224) <- labels(ds.hohh$e221)
labels(ds.hohh$e225) <- labels(ds.hohh$e221)
labels(ds.hohh$e226) <- labels(ds.hohh$e221)

measurement(ds.hohh$h1396) <- "nominal"
labels(ds.hohh$h1396)<- labels(ds.hohh$h135)
measurement(ds.hohh$i1) <- "nominal"
labels(ds.hohh$i1)<- labels(ds.hohh$h135)
measurement(ds.hohh$i2) <- "nominal"
labels(ds.hohh$i2)<- labels(ds.hohh$i1)
measurement(ds.hohh$i3) <- "nominal"
labels(ds.hohh$i3)<- labels(ds.hohh$i1)
measurement(ds.hohh$k1) <- "nominal"
labels(ds.hohh$k1)<- labels(ds.hohh$i1)
measurement(ds.hohh$k31) <- "nominal"
labels(ds.hohh$k31)<- labels(ds.hohh$i1)

measurement(ds.hohh$k32) <- "nominal"
labels(ds.hohh$k32)<- labels(ds.hohh$i1)
measurement(ds.hohh$k33) <- "nominal"
labels(ds.hohh$k33)<- labels(ds.hohh$i1)
measurement(ds.hohh$k34) <- "nominal"
labels(ds.hohh$k34)<- labels(ds.hohh$i1)
measurement(ds.hohh$k396) <- "nominal"
labels(ds.hohh$k396)<- labels(ds.hohh$i1)

# duplicate labels
labels(ds.hohh$d21)[2] <- "Disagree"
labels(ds.hohh$d22)[2] <- "Disagree"
labels(ds.hohh$d23)[2] <- "Disagree"
rm(ds.1, ds.5)


## 2. CONSOLIDATE household member TABLES######################################
###############################################################################

## 2.1. type of hh member## in ds2 and ds3#####################################
ds.2$n1 <- as.item(rep(1, nrow(ds.2)))

measurement(ds.2$n1) <- "nominal"
description(ds.2$n1) <- "Type of hh member"
annotation(ds.2$n1)["flag"] <- "Deriv."
annotation(ds.2$n1)["origin"] <- "based on which table (section) was the source"
labels(ds.2$n1) <- c(
  "In same household"        =  1,
  "Not living in same household"=  2)

ds.3$n1 <- as.item(rep(2, nrow(ds.3)))

measurement(ds.3$n1) <- "nominal"
description(ds.3$n1) <- "Type of hh member"
annotation(ds.3$n1)["flag"] <- "Deriv."
annotation(ds.3$n1)["origin"] <- "based on which table (section) was the source"
labels(ds.3$n1) <- c(
  "In same household"        =  1,
  "Not living in same household"=  2)

## 2.2. rename columns in ds2 and 3 so they match##############################
colnames(ds.3)[3] <- "mcode"
colnames(ds.3)[4] <- "n2"
colnames(ds.2)[4] <- "n2"
colnames(ds.3)[5] <- "n3"
colnames(ds.2)[5] <- "n3"
colnames(ds.3)[6] <- "n4"
colnames(ds.2)[6] <- "n4"
colnames(ds.3)[7] <- "n5"
colnames(ds.2)[7] <- "n6"
ds.3$n6 <- as.item(rep(100, nrow(ds.3)))
ds.2$n5 <- as.item(rep(0, nrow(ds.2)))

## 2.3. merge  ds2 and 3 and polish up items ##################################
ds.mmbr <- rbind(ds.2, ds.3)

description(ds.mmbr$n2) <- "name of household member"
annotation(ds.mmbr$n2)["flag"] <- "Deriv."
annotation(ds.mmbr$n2)["origin"] <- "merged from b1 and c1"

annotation(ds.mmbr$n3)["flag"] <- "Deriv."
annotation(ds.mmbr$n3)["origin"] <- "merged from b2 and c2"

annotation(ds.mmbr$n4)["flag"] <- "Deriv."
annotation(ds.mmbr$n4)["origin"] <- "merged from b3 and c3"
labels(ds.mmbr$n4) <- labels(ds.mmbr$n4) + c("Missing due to input error" = 100)

measurement(ds.mmbr$n5) <- "nominal"
description(ds.mmbr$n5) <- "Where are they living now?"
labels(ds.mmbr$n5)<- labels(ds.3$n5)
labels(ds.mmbr$n5) <- labels(ds.mmbr$n5) + c("Same Household" = 0)
annotation(ds.mmbr$n5)["flag"] <- "Deriv."
annotation(ds.mmbr$n5)["origin"] <- "merged from c4 and membership in Section B"

measurement(ds.mmbr$n6) <- "interval"
missing.values(ds.mmbr$n6) <- c(100)
annotation(ds.mmbr$n6)["flag"] <- "Deriv."
annotation(ds.mmbr$n6)["origin"] <- "from b4 but missing for section C"

ds.mmbr <- FunSwap(ds=ds.mmbr, New = "n1", After =  "mcode")
ds.mmbr <- FunSwap(ds=ds.mmbr, New = "n5", After =  "n4")
rm(ds.2, ds.3)
## 2.4. add household id and member id ########################################
ds.mmbr$hh.id <- as.item(left_join(as.data.frame(ds.mmbr), 
                                   as.data.frame(ds.hohh[c(3,5,6)]))$hh.id)

measurement(ds.mmbr$hh.id) <- "interval"
description(ds.mmbr$hh.id) <- "Household ID - unique"
annotation(ds.mmbr$hh.id)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.id)["origin"] <- "matched from Household file"

# order them correctly 
ds.mmbr<- ds.mmbr[order(ds.mmbr$n1),]
ds.mmbr<- ds.mmbr[order(ds.mmbr$hh.id),]

ds.mmbr$hh.member.id <- as.item(1:nrow(ds.mmbr))
description(ds.mmbr$hh.member.id) <- "Household Memeber ID - unique"
measurement(ds.mmbr$hh.member.id) <- "interval"
annotation(ds.mmbr$hh.member.id)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.member.id)["origin"] <- "nrows"

ds.mmbr <- FunSwap(ds=ds.mmbr, New = "hh.id", After =  "hhcode")
ds.mmbr <- FunSwap(ds=ds.mmbr, New = "hh.member.id", After =  "mcode")

# add missing description
description(ds.mmbr$mcode) <- "Member code within hh (and type of member)"
annotation(ds.mmbr$mcode) <- annotation(ds.mmbr$mcode)[c(3,1,2)]
 # add back province 
ds.mmbr$province <- memisc::recode(ds.mmbr$commune,   
                                   "Thai Binh" = 1 <- c(111, 112, 121, 122),
                                   "Vinh Phuc" = 2 <- c(211, 212, 221, 222))


## 3. add hh ids to the plots #################################################
###############################################################################
## 3.4. add household id and member id ########################################
ds.plot <- ds.4
ds.plot$hh.id <- as.item(left_join(as.data.frame(ds.plot), 
                                   as.data.frame(ds.hohh[c(3,5,6)]))$hh.id)
description(ds.plot$hh.id) <- "Household ID - unique"
measurement(ds.plot$hh.id) <- "interval"
annotation(ds.plot$hh.id)["flag"] <- "Deriv."
annotation(ds.plot$hh.id)["origin"] <- "nrows"
ds.plot <- FunSwap(ds=ds.plot, New = "hh.id", After =  "hhcode")
description(ds.plot$plot) <- "Plot code within hh"
annotation(ds.plot$plot) <- annotation(ds.plot$plot)[c(3,1,2)]
# add back province 
ds.plot$province <- memisc::recode(ds.plot$commune,   
                                   "Thai Binh" = 1 <- c(111, 112, 121, 122),
                                   "Vinh Phuc" = 2 <- c(211, 212, 221, 222))

rm(ds.4)
save(ds.hohh, ds.mmbr, ds.plot, file = "data/outputs/clean.Rdata")
