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

ds$n2 <- as.item(2016 - ds$a2)
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
labels(ds$a4) <- c("0" = 101)
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
description(ds$d1) <- paste0(description(ds$d1), "elds?")


# d21 - Likert 1 
measurement(ds$d21) <- "nominal"
labels(ds$d21) <- c(
  "Stronlgy disagree" = 1,
  "Disagree" = 2, 
  "Neither agree nor disagree" = 3, 
  "Agree" = 4,
  "Stronlgy agree" =5)
description(ds$d21) <- "It is the duty of a parent to do his/her 
best for his/her children even at the expense of their own well-being"


# d22 - Likert 2 
 
measurement(ds$d22) <- "nominal"
labels(ds$d22) <- c(
  "Stronlgy disagree" = 1,
  "Disagree" = 2, 
  "Neither agree nor disagree" = 3, 
  "Agree" = 4,
  "Stronlgy agree" =5)
description(ds$d22) <- "It is the duty of grandparents to be there for their 
grandchildren in times of difficulty (e.g. illness, divorce)"


# d23 - Likert 3
measurement(ds$d23) <- "nominal"
labels(ds$d23) <- c(
  "Stronlgy disagree" = 1,
  "Disagree" = 2, 
  "Neither agree nor disagree" = 3, 
  "Agree" = 4,
  "Stronlgy agree" =5)
description(ds$d23) <- "It is the duty of grandparents to contribute to the economic security 
of their family (i.e., their children and their grandchildren)"


# d31 primary responsibility 1
measurement(ds$d31) <- "nominal"
labels(ds$d31) <- c(
  "Self" = 1,
  "Family" = 2, 
  "Community" = 3)

# d32 primary responsibility 2
measurement(ds$d32) <- "nominal"
labels(ds$d32) <- c(
  "Self" = 1,
  "Family" = 2, 
  "Community" = 3)

# d33 primary responsibility 3
measurement(ds$d33) <- "nominal"
labels(ds$d33) <- c(
  "Self" = 1,
  "Family" = 2, 
  "Community" = 3)

# d34 primary responsibility 4
measurement(ds$d34) <- "nominal"
labels(ds$d34) <- c(
  "Self" = 1,
  "Family" = 2, 
  "Community" = 3)

# e1 health 
measurement(ds$e1) <- "nominal"
labels(ds$e1) <- c(
  "Very good" = 1,
  "Good" = 2, 
  "Fair" = 3,
  "Poor" = 4,
  "Very poor" = 5)

## 2.1
# e211
measurement(ds$e211) <- "nominal"
labels(ds$e211) <- c(
  "Yes" = 1,
  "No" = 2) 

# e221
measurement(ds$e221) <- "nominal"
labels(ds$e221) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e231
measurement(ds$e231) <- "nominal"
ds$e231 <- recode(ds$e231,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e231) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 




## 2.2
# e212
measurement(ds$e212) <- "nominal"
labels(ds$e212) <- c(
  "Yes" = 1,
  "No" = 2) 

# e221
measurement(ds$e222) <- "nominal"
labels(ds$e222) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e232
measurement(ds$e232) <- "nominal"
ds$e232 <- recode(ds$e232,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e232) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 


## 2.3

# e213
measurement(ds$e213) <- "nominal"
labels(ds$e213) <- c(
  "Yes" = 1,
  "No" = 2) 

# e223
measurement(ds$e223) <- "nominal"
labels(ds$e223) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e233
measurement(ds$e233) <- "nominal"
ds$e233 <- recode(ds$e233,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e233) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 


## 2.4

# e214
measurement(ds$e214) <- "nominal"
labels(ds$e214) <- c(
  "Yes" = 1,
  "No" = 2) 

# e224
measurement(ds$e224) <- "nominal"
labels(ds$e224) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e234
measurement(ds$e234) <- "nominal"
ds$e234 <- recode(ds$e234,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e234) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 


## 2.5

# e215
measurement(ds$e215) <- "nominal"
labels(ds$e215) <- c(
  "Yes" = 1,
  "No" = 2) 

# e225
measurement(ds$e225) <- "nominal"
labels(ds$e225) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e235
measurement(ds$e235) <- "nominal"
ds$e235 <- recode(ds$e235,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e235) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 


## 2.6

# e216
measurement(ds$e216) <- "nominal"
labels(ds$e216) <- c(
  "Yes" = 1,
  "No" = 2) 

# e226
measurement(ds$e226) <- "nominal"
labels(ds$e226) <-c("Respondent" = 1,
                    "Spouse" = 2,
                    "Son" = 3,
                    "Daughter" = 4,
                    "Son/daughter in law" = 5,
                    "Adopted child" = 6,
                    "Parent" = 7,
                    "Parent in law" = 8,
                    "Grandparent" = 	9,
                    "(Great) Grandchildren" = 10,
                    "Siblings" = 11,
                    "Other relatives" = 12,
                    "House servant" = 13,
                    "Other (specify)" = 96, 
                    "NA" = 101)

# e236
measurement(ds$e236) <- "nominal"
ds$e236 <- recode(ds$e236,
                  0 <- 2,
                  1 <- 1,
                  101 <- 101)
labels(ds$e236) <- c(
  "Yes" = 1,
  "No" = 0, 
  "NA" = 101) 

# MODULE 2 = section G
###############################################################################

# g1 = OK 
# but 96, should it be NA?
missing.values(ds$g1) <- 96

# g2
measurement(ds$g2) <- "nominal"
labels(ds$g2) <- c(
  "Subsistence" = 1,
  "Sell" = 2, 
  "Both" = 3, 
  "NA" = 101) 



# MODULE 3 - section H
###############################################################################
# h1a
measurement(ds$h1a) <- "nominal"
labels(ds$h1a) <- c(
  "Weather" = 1,
  "Access to water"= 2,
  "My field type" = 3,
  "My soil type and quality" = 4,
  "Whether I have access to ploughing machinery/ labour" = 5,
  "Whether they are resistant certain pests/ diseases" = 6,
  "How they taste" = 7 ,
  "Whether they are a productive variety" = 8,
  "Other (specify)" = 96,  
  "NA" = 101)
description(ds$h1a)  <- paste(description(ds$h1a), "- first choice")
  
# h1b
measurement(ds$h1b) <- "nominal"
labels(ds$h1b) <- c(
  "Weather" = 1,
  "Access to water"= 2,
  "My field type" = 3,
  "My soil type and quality" = 4,
  "Whether I have access to ploughing machinery/ labour" = 5,
  "Whether they are resistant certain pests/ diseases" = 6,
  "How they taste" = 7 ,
  "Whether they are a productive variety" = 8,
  "Other (specify)" = 96,  
  "NA" = 101)
description(ds$h1b)  <- paste(description(ds$h1b), "- second choice")

# h1c
measurement(ds$h1c) <- "nominal"
labels(ds$h1c) <- c(
  "Weather" = 1,
  "Access to water"= 2,
  "My field type" = 3,
  "My soil type and quality" = 4,
  "Whether I have access to ploughing machinery/ labour" = 5,
  "Whether they are resistant certain pests/ diseases" = 6,
  "How they taste" = 7 ,
  "Whether they are a productive variety" = 8,
  "Other (specify)" = 96,  
  "NA" = 101)
description(ds$h1c)  <- paste(description(ds$h1c), "- third choice")


# MODULE 4 - section I
###############################################################################
# i1 
measurement(ds$i1) <- "nominal"
ds$i1 <- recode(ds$i1,
                  0 <- 2,
                  1 <- 1)
labels(ds$i1) <- c(
  "Yes" = 1,
  "No" = 0) 

save(ds, file = "data/updated-copy.RData")
# rmarkdown::render("scripts/r00-survey-variable-list.Rmd", output_format = "pdf_document", output_dir = "reports")
