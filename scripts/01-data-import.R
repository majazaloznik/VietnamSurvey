###############################################################################
##
##  DATA IMPORT, ANNONYMIZATION
##
###############################################################################
## 0. Preliminaries
## 1. Import copy .dta file # One OFF
## 2. Annonimize the data # One OFF
## 3. Save working copy # One OFF
###############################################################################

## 0. Preliminaries
###############################################################################
library(readstata13)
library(tidyr)
library(dplyr)


# # ## 1. Import copy .dta file 
# # ###############################################################################
# df <- read.dta13("data/full_data_v1_no_dup.dta")
# 
# ## 2. Annonimize the data 
# ###############################################################################
# ## NEW VARIABLE - id - unique ID
# df$id <- 1:nrow(df)
# attributes(df)$var.labels <- c(attributes(df)$var.labels, "Unique ID")
# 
# ## NEW VARIABLE - n1 - respondent == HoH
# df$n1 <- ifelse(df$hhhead == df$respondent, TRUE, FALSE)
# # for manually checking mistyped names:
# df %>% 
#   select(id, hhhead, respondent, n1) %>% 
#   filter(n1 == FALSE) -> name.test
# 
# # List of names that were actually the same:
# # do tuan thin != do tuaan thin (89)
# # tra ba thai != tran ba thai (139)
# # nguyen thi le ! = nguyn thi le (222)
# # nguyen thi ve != nguyn thi ve (229)
# # trn thi nghanh != tran thi nghanh (238)
# # Nguyen thi hong ! = nguyen thi hong (315)
# # Chu dan chu != chu dan chu (346)
# # nguyen van bo != Nguyen van bo(348)
# 
# actually.correct <- c(89, 139, 222, 229, 238, 315, 346, 348)
# df$n1[actually.correct] <- TRUE
# 
# attributes(df)$var.labels <- c(attributes(df)$var.labels, "Was the respondent also the head of household?")
# 
# ## Annonimyze the data:
# removing <- which(colnames(df) %in% c("hhhead", "respondent", "phongvan"))
# df <- df %>% select(-hhhead, -respondent, -phongvan)
# # need to manually remove the var.labels for the ones i've deleted, don't care about other attrs.
# attributes(df)$var.labels <- attributes(df)$var.labels[-removing]
# rm(name.test, actually.correct, removing)

## 3. save working copy. Comment out
###############################################################################
# save(data, file = "data/working-copy.RData")

# version:0.0, 20.3. 

