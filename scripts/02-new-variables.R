###############################################################################
##
##  DATA TRAWL AND NEW VARS
##
###############################################################################
## 0. Preliminaries
## 1. New variables, A
## 2. New variables, B
###############################################################################

## 0. Preliminaries
###############################################################################
library(memisc)
library(tidyr)
library(dplyr)
load("data/working-copy.RData")
source("scripts/00-my-functions.R")

## 1.  New variables  A
###############################################################################

# remove province
FunRemove("province")

# label procode:
labels(ds$procode) <- c("Thai Binh" = 1,
                        "Vinh Phuc" = 2)

# remove province
FunRemove("distric")

# label procode:
labels(ds$distcode) <- c("Binh Xuyen" = 21,
                         "Kien Xuong" = 12,
                        "Tam Duong" = 22,
                        "Thai Thuy" = 11)

# remove province
FunRemove("commune")

# label procode:
labels(ds$commcode) <- c("Dao Tu" = 221,
                         "Dinh Phung" = 122,
                         "Hoang Dan" = 222,
                         "Phu Xuan" = 211,
                         "Tam Hop" = 212,
                         "Thanh Tan" = 121,
                         "Thuy Binh" = 111,
                         "Thuy Van" = 112)

# missing value in HHcode fixed, 99
ds$hhcode[which(is.na(as.vector(ds$hhcode)))] <- 99
missing.values(ds$hhcode) <- 99
annotation(ds$hhcode)["flag"] <- "Orig.*"

# measurement cphongvan
measurement(ds$cphongvan) <- "nominal"

# cphongvan needs to merge categoris 1 and 12. and measurement
ds$cphongvan <- recode(ds$cphongvan, 
                       1 <- c(1,12), 
                       otherwise = "copy")
annotation(ds$cphongvan)["flag"] <- "Orig.*"

# measurement cnhaptin
measurement(ds$cnhaptin) <- "nominal"

# a1 - sex
measurement(ds$a1) <- "nominal"
labels(ds$a1) <- c("Male" = 1,
                   "Female" = 2)
# n2 - age

ds$n2 <- 2016 - ds$a2
measurement(ds$n2) <- "interval"
description(ds$n2) <- "Age - approx"
FunSwap(ds, "n2", "a2")
annotation(ds$n2)["flag"] <- "Deriv."
annotation(ds$n2)["origin"] <- "a2"

# a3 marital status
measurement(ds$a3) <- "nominal"
labels(ds$a3) <- c("Never married" = 1,
                   "Married" = 2,
                   "Separated" = 3,
                   "Divorced" = 4,
                   "Widow/widower" = 5)

# a4 completed grade
labels(ds$a4) <- c("NA" = 101)
annotation(ds$a4)["flag"] <- "Orig.*"

# a5 general education
measurement(ds$a5) <- "nominal"
labels(ds$a5) <- c(
  "No qualification" = 0, 
  "Primary" = 1,
  "Lower secondary" = 2,
  "Higher secondary" = 3,
  "College" = 4,
  "University" = 5,
  "Others (Specify)" = 96)
  
# a6 vocational education
measurement(ds$a6) <- "nominal"
labels(ds$a6) <- c(
  "No qualification" = 0, 
  "Elementary vocational school" = 1,
  "Middle-level vocational school" = 2,
  "Professional school" = 3,
  "Vocational college" = 4,
  "Others (Specify)" = 96)


## 2.  New variables  B
###############################################################################









## 3.  New variables  C
###############################################################################



## 4.  New variables  D
###############################################################################

# d1 - no longer rice
measurement(ds$d1) <- "nominal"
labels(ds$d1) <- c(
  "I am still able to do and not think about this yet" = 0, 
  "My children will continue to farm them" = 1,
  "I will rent them out" = 2,
  "I will return them to the commune" = 3,
  "My neighbours will have them" = 4,
  "Other family members will farm them" = 5,
  "Others (Specify)" = 96)












save(ds, file = "data/updated-copy.RData")
rmarkdown::render("scripts/r00-survey-variable-list.Rmd", output_format = "pdf_document", output_dir = "reports")