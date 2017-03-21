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
library(memisc)
library(tidyr)
library(dplyr)
source("scripts/00-my-functions.R")

## 1. Import copy .dta file and add new slots
###############################################################################
# ds <- as.data.set(Stata.file("data/full_data_v1_no_dup_stata-12-11.dta"))
# # ## 3. Add new slots to annotations
# ## damn, don't know how to vectorise this!
# for (i in 1:ncol(ds)) {
#   annotation(ds[[i]])["flag"] <- "Orig."
#   annotation(ds[[i]])["origin"] <- "Imported from .dta"
# }
# 
#  
# ## 2. Annonimize the data
# ###############################################################################
# ## NEW VARIABLE - id - unique ID
# ds$id <- as.item(1:nrow(ds))
# description(ds$id) <- "ID - unique"
# FunSwap(ds, "id", "hhcode")
# annotation(ds$id)["flag"] <- "Deriv."
# annotation(ds$id)["origin"] <- "nrows"
# 
# ## NEW VARIABLE - n1 - respondent == HoH
# ds$n1 <- as.item(ifelse(ds$hhhead == ds$respondent, TRUE, FALSE))
# description(ds$n1) <- "Was the respondent also the head of household?"
# measurement(ds$n1) <- "nominal"
# # for manually checking mistyped names:
# ds %>% as.data.frame() %>%
#   dplyr::select(id, hhhead, respondent, n1) %>%
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
# ds$n1[actually.correct] <- TRUE
# labels(ds$n1) <- c("TRUE" = 1,
#                    "FALSE" = 0)
# FunSwap(ds, "n1", "id")
# annotation(ds$n1)["flag"] <- "Deriv."
# annotation(ds$n1)["origin"] <- "hhhead & respondent & MANUAL error checking "
# 
# ## Annonimyze the data:
# removing <- which(names(ds) %in% c("hhhead", "respondent", "phongvan"))
# ds <- ds[-removing]
# # need to manually remove the var.labels for the ones i've deleted, don't care about other attrs.
# rm(name.test, actually.correct, removing)
# 
# 
# # # ## 3. save working copy. Comment out
# # # ###############################################################################
# save(ds, file = "data/working-copy.RData")
# rmarkdown::render("scripts/r00-survey-variable-list-originals-only.Rmd", 
#                   output_format = "pdf_document", output_dir = "reports")

# version:0.0, 20.3.
# version:0.1, 21.3 (error had save data instead of df!)
# version:1.0, 21.3 - changed to memisc and data.set import
# version:1.1, 21.3 - added source functions to swap positions of two new vars
# version:1.2, 21.3 - added annotations for the two new vars 
# version:1.3, 21.3 - added final push to .pdf list of original variables only. 