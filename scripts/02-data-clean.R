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
rm(list = ls())
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
