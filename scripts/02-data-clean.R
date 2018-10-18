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
## 2.5 more variables - 10.10. ################################################
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

# new variables:
# married or not married 

ds.hohh$n7 <- memisc::recode(ds.hohh$a3,   
                             "Married" = 1 <- 2,
                             "Not married" = 2 <- c(1,3,4,5))
description(ds.hohh$n7) <- "Married or not married"
measurement(ds.hohh$n7) <- "nominal"
annotation(ds.hohh$n7)["flag"] <- "Deriv."
annotation(ds.hohh$n7)["origin"] <- "recode from a3"

ds.hohh <- FunSwap(ds=ds.hohh, New = "n7", After =  "a3")

rm(ds.1, ds.5)


## 2. CONSOLIDATE household member TABLES######################################
###############################################################################

## 2.1. type of hh member## in ds2 and ds3#####################################
# changed the measurement to interval here, because i use sum at some point, 
# and the conversion to numeric only works this way, otherwise it turns them 
# into factors. 
ds.2$n1 <- as.item(rep(1, nrow(ds.2)))

measurement(ds.2$n1) <- "interval"
description(ds.2$n1) <- "Type of hh member"
annotation(ds.2$n1)["flag"] <- "Deriv."
annotation(ds.2$n1)["origin"] <- "based on which table (section) was the source"
labels(ds.2$n1) <- c(
  "In same household"        =  1,
  "Not living in same household"=  0)

ds.3$n1 <- as.item(rep(0, nrow(ds.3)))

measurement(ds.3$n1) <- "interval"
description(ds.3$n1) <- "Type of hh member"
annotation(ds.3$n1)["flag"] <- "Deriv."
annotation(ds.3$n1)["origin"] <- "based on which table (section) was the source"
labels(ds.3$n1) <- c(
  "In same household"        =  1,
  "Not living in same household"=  0)

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

## additional errors fixed 5.10. (see x1-data-checking-additional for reasons)
ds.mmbr$n4[ds.mmbr$hh.member.id == 99] <- 1 # "Respondent"
ds.mmbr$n4[ds.mmbr$hh.member.id == 115] <- 3 #"Son"
ds.mmbr$n4[ds.mmbr$hh.member.id == 370] <- 1 # "Respondent"

# add married respondent variable
ds.mmbr$n8 <- ds.hohh$n7[match(ds.mmbr$hh.id, ds.hohh$hh.id)]  


description(ds.mmbr$n8) <- "Respondent married or not married"
measurement(ds.mmbr$n8) <- "nominal"
annotation(ds.mmbr$n8)["flag"] <- "Deriv."
annotation(ds.mmbr$n8)["origin"] <- "recode from a3"

ds.mmbr <- FunSwap(ds=ds.mmbr, New = "n8", After =  "province")

## 2.5 more variables - 10.10. ################################################

# household size
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.size = n()) %>% 
  pull(hh.size) %>% 
  as.item() -> ds.mmbr$hh.size

description(ds.mmbr$hh.size) <- "Household size"
measurement(ds.mmbr$hh.size) <- "interval"
annotation(ds.mmbr$hh.size)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.size)["origin"] <- "recode from hh.id"

# Household size net - only people living in household 
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.size.net = sum(n1))%>% 
  pull(hh.size.net)  %>% 
  as.item() -> ds.mmbr$hh.size.net

description(ds.mmbr$hh.size.net) <- "Household size net of migrants"
measurement(ds.mmbr$hh.size.net) <- "interval"
annotation(ds.mmbr$hh.size.net)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.size.net)["origin"] <- "recode from hh.id and n1"

# gen - generation relative to respondent 
ds.mmbr$gen <- memisc::recode(ds.mmbr$n4, 
                              0  <- 1:2,
                              1 <- 3:6, 
                              -1 <- 7:8,
                              -2 <- 9,
                              2 <- 10,
                              0 <- 11,
                              otherwise = NA)
description(ds.mmbr$gen) <- "Generation relative to respondent"
measurement(ds.mmbr$gen) <- "interval"
annotation(ds.mmbr$gen)["flag"] <- "Deriv."
annotation(ds.mmbr$gen)["origin"] <- "recode from n4"

# n.gen - number of generations in hosuehold
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(n.gen = FunNumberGens(gen)) %>% 
  pull(n.gen) %>% 
  as.item() -> ds.mmbr$n.gen

description(ds.mmbr$n.gen) <- "Number of different generations in household"
measurement(ds.mmbr$n.gen) <- "interval"
annotation(ds.mmbr$n.gen)["flag"] <- "Deriv."
annotation(ds.mmbr$n.gen)["origin"] <- "recode from gen and hh.id"

# n.gen.net - number of generations in hosuehold net of migrants
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(n.gen.net = FunNumberGens(gen[n1 == 1])) %>% 
   pull(n.gen.net) %>% 
  as.item() -> ds.mmbr$n.gen.net 

description(ds.mmbr$n.gen.net) <- "Number of different generations in household net of migrants"
measurement(ds.mmbr$n.gen.net) <- "interval"
annotation(ds.mmbr$n.gen.net)["flag"] <- "Deriv."
annotation(ds.mmbr$n.gen.net)["origin"] <- "recode from gen and hh.id and n1"

# skipped.gen - whether or not a generation is skipped in the household

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(x = FunSkippedGen(gen)) %>% 
  pull(x) %>% 
  as.item() -> ds.mmbr$skipped.gen

description(ds.mmbr$skipped.gen) <- "Whether or not there is a skipped generation in the household"
measurement(ds.mmbr$skipped.gen) <- "nominal"
annotation(ds.mmbr$skipped.gen)["flag"] <- "Deriv."
annotation(ds.mmbr$skipped.gen)["origin"] <- "recode from gen and hh.id and gen"
labels(ds.mmbr$skipped.gen) <- c(
  "Skipped generation"        =  1,
  "No skipped gen"=  0)

# skipped.gen.net - whether or not a generation is skipped in the household net of migrants

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(x = FunSkippedGen(gen[n1 == 1])) %>% 
  pull(x) %>% 
  as.item() -> ds.mmbr$skipped.gen.net

description(ds.mmbr$skipped.gen.net) <- "Whether or not there is a skipped generation in the household net of mirgants"
measurement(ds.mmbr$skipped.gen.net) <- "nominal"
annotation(ds.mmbr$skipped.gen.net)["flag"] <- "Deriv."
annotation(ds.mmbr$skipped.gen.net)["origin"] <- "recode from gen and hh.id and gen and n1"
labels(ds.mmbr$skipped.gen.net) <- c(
  "Skipped generation"        =  1,
  "No skipped generation"=  0)



# is the respondent lone

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(single = ifelse(any(2 %in% as.numeric(n4)), 0, 1)) %>% 
  pull(single) %>%
  as.item() ->  ds.mmbr$lone.resp

description(ds.mmbr$lone.resp) <- "Lone respondent without spouse"
measurement(ds.mmbr$lone.resp) <- "nominal"
annotation(ds.mmbr$lone.resp)["flag"] <- "Deriv."
annotation(ds.mmbr$lone.resp)["origin"] <- "recode from and hh.id and n4"
labels(ds.mmbr$lone.resp) <- c(
  "lone"        =  1,
  "not lone")

# is the respondent lone - net

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(single = ifelse(any(2 %in% as.numeric(n4[n1 == 1])), 0, 1)) %>% 
  pull(single) %>%
  as.item() ->  ds.mmbr$lone.resp.net

description(ds.mmbr$lone.resp.net) <- "Lone respondent without spouse net"
measurement(ds.mmbr$lone.resp.net) <- "nominal"
annotation(ds.mmbr$lone.resp.net)["flag"] <- "Deriv."
annotation(ds.mmbr$lone.resp.net)["origin"] <- "recode from and hh.id and n4"
labels(ds.mmbr$lone.resp.net) <- c(
  "lone"        =  1,
  "not lone"    =   0)

# un based typology
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.type.fam = 
           factor(ifelse(hh.size.net == 1, 1,
                         ifelse(all(unique(as.numeric(n4[n1 == 1])) %in% c(1,2,3,4)), 2, 
                                ifelse(13 %in% as.numeric(n4[n1 == 1]), 4, 3))), levels = 1:4,
                  labels = c("single person", "nuclear",  "composite", "extended"))) %>% 
  pull(hh.type.fam) %>% 
  as.item() ->  ds.mmbr$hh.type.fam

description(ds.mmbr$hh.type.fam) <- "Family based household typology"
measurement(ds.mmbr$hh.type.fam) <- "nominal"
annotation(ds.mmbr$hh.type.fam)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.type.fam)["origin"] <- "recode from and hh.id and hh.size and n4"
labels(ds.mmbr$hh.type.fam) <- c(
  "single person"        =  1,
  "nuclear"    =   2,
  "extended" = 3,
  "composite" =4
)

# un based typology - expanded with lone status

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.type.fam.lone = 
           ifelse(hh.type.fam == "single person", 1,
                  ifelse(hh.type.fam == "nuclear" & 
                           lone.resp.net == "lone", 2, 
                         ifelse(hh.type.fam == "nuclear" & 
                                  lone.resp.net == "not lone" , 3,
                                ifelse(hh.type.fam == "extended" & 
                                         lone.resp.net == "lone" , 4,
                                       ifelse(hh.type.fam == "extended" & 
                                                lone.resp.net == "not lone" , 5,
                                              ifelse(hh.type.fam == "composite" & 
                                                       lone.resp.net == "lone" , 6, 7))))))) %>% 
  pull(hh.type.fam.lone) %>% 
  as.item() ->  ds.mmbr$hh.type.fam.lone

description(ds.mmbr$hh.type.fam.lone) <- "Family based household typology - expanded"
measurement(ds.mmbr$hh.type.fam.lone) <- "nominal"
annotation(ds.mmbr$hh.type.fam.lone)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.type.fam.lone)["origin"] <- "recode from and hh.id and hh.size and n4"
labels(ds.mmbr$hh.type.fam.lone) <- c(
  "single person"        =  1,
  "nuclear-lone"    =   2,
  "nuclear"    =   3,
  "extended-lone" = 4,
  "extended" = 5,
  "composite-lone" = 6,
  "composite" = 7
)

# generation typology
ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.type.gen = ifelse(hh.size.net == 1, 1,
                                      ifelse(skipped.gen.net == "Skipped generation", 2, n.gen.net+2 )))%>% 
  pull(hh.type.gen) %>% 
  as.item() ->  ds.mmbr$hh.type.gen

description(ds.mmbr$hh.type.gen) <- "Generation based household typology"
measurement(ds.mmbr$hh.type.gen) <- "nominal"
annotation(ds.mmbr$hh.type.gen)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.type.gen)["origin"] <- "recode from gen and hh.id and hh.size and skipped.gen.net"
labels(ds.mmbr$hh.type.gen) <- c(
  "single person"        =  1,
  "skipped"    =   2,
  "one gen" =3,
  "two gen" = 4,
  "three gen" = 5,
  "four gen" = 6)

# generational typology - expanded with lone status

ds.mmbr %>% 
  as.data.frame() %>% 
  group_by(hh.id) %>% 
  mutate(hh.type.gen.lone = 
           ifelse(hh.type.gen == "single person", 1,
                  ifelse(hh.type.gen == "skipped" & 
                           lone.resp.net == "lone", 2, 
                         ifelse(hh.type.gen == "skipped" & 
                                  lone.resp.net == "not lone" , 3,
                                ifelse(hh.type.gen == "one gen" & 
                                         lone.resp.net == "lone" , 4,
                                       ifelse(hh.type.gen == "one gen" & 
                                                lone.resp.net == "not lone" , 5,
                                              ifelse(hh.type.gen == "two gen" & 
                                                       lone.resp.net == "lone" , 6, 
                                                     ifelse(hh.type.gen == "two gen" & 
                                                              lone.resp.net == "not lone" , 7, 
                                                            ifelse(hh.type.gen %in% c("three gen", "four gen") & 
                                                                     lone.resp.net == "lone" , 8,9))))))))) %>% 
  pull(hh.type.gen.lone) %>% 
  as.item() ->  ds.mmbr$hh.type.gen.lone

description(ds.mmbr$hh.type.gen.lone) <- "Generation based household typology - expanded"
measurement(ds.mmbr$hh.type.gen.lone) <- "nominal"
annotation(ds.mmbr$hh.type.gen.lone)["flag"] <- "Deriv."
annotation(ds.mmbr$hh.type.gen.lone)["origin"] <- "recode from hh.type.gen and lone.resp.net"
labels(ds.mmbr$hh.type.gen.lone) <- c(
  "single person"        =  1,
  "skipped - lone"    =   2,
  "skipped"    =   3,
  "one gen - lone" =4,
  "one gen" = 5,
  "two gen - lone" = 6,
  "two gen" = 7,
  "three+ gen - lone" = 8,
  "three+ gen" = 9)

## 2.6 re-join household typologies to household dataset

ds.hohh<- merge(ds.hohh, subset(ds.mmbr, n4 == 1, select = -c(n1, gen)))
 


## 3. add hh ids to the plots #################################################
###############################################################################
## 3.4. add household id and member id ########################################
ds.plot <- ds.4
colz <- c("commune", "hhcode", "hh.id")
colz <- which(names(ds.hohh) %in% colz)
ds.plot$hh.id <- as.item(left_join(as.data.frame(ds.plot), 
                                   as.data.frame(ds.hohh[colz]))$hh.id)

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
