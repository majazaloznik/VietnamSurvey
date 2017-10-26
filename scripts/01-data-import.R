###############################################################################
##
##  DATA IMPORT, ANNONYMIZATION  ##############################################
##
###############################################################################
## 0. Preliminaries  ##########################################################
## 1. Import and check Section A-G     ########################################
## 1.1 Import copy .xlsx file # One OFF  ######################################
## 1.2 Import old .dta file to compare# One OFF  ##############################
## 2. Annonimize the data # One OFF
## 3. Save working copy # One OFF
###############################################################################

## 0. Preliminaries  ##########################################################
###############################################################################
library(memisc)
library(tidyverse)
library(readxl)


## 1. Import and check Section A-G     ########################################
## 1.1 Import stata version ###########  ######################################
ds.1 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_A-G.dta"))

# add new slots to annotations
for (i in 1:ncol(ds.1)) {
  annotation(ds.1[[i]])["flag"] <- "Orig."
  annotation(ds.1[[i]])["origin"] <- "Imported from .dta"
}

## 1.2 Import copy .xlsx file # One OFF  ######################################
ds.x.1 <- read_excel("data/data-raw/DATA/Excel/Section_A-G.xls")

## 1.2 Import old .dta file to compare# One OFF  ##############################

ds.old <- as.data.set(Stata.file("old/data-old/full_data_v1_no_dup_stata-12-11.dta"))

## 1.3 compare new stata with new excel######### ##############################

# stata file, unfactor
df.1 <- as.data.frame(ds.1[1:6])
levels(df.1$province) <- unique(ds.x.1$province)
levels(df.1$district) <- unique(ds.x.1$district)
levels(df.1$commune) <- unique(ds.x.1$commune)
df.1$province <- as.numeric(as.character(df.1$province))
df.1$district <- as.numeric(as.character(df.1$district))
df.1$commune <- as.numeric(as.character(df.1$commune))
df.1.stata.only <- anti_join(df.1, ds.x.1[1:6])

df.1.stata.only$new.excel <- FALSE
df.1.stata.only$new.stata <- TRUE
df.1.stata.only$old.stata <- TRUE

## 1.4 compare new stata with old stata ######################################

df.old <- as.data.frame(ds.old[c(2,4,6,7,8,9)])
colnames(df.old) <-  c("province", "district", "commune", "hhhead","hhcode","respondent")

# don't match on hhcode, because that's where there are errors. 
df.1.new.only <- anti_join(df.1, df.old,
                  by = c("province" = "province", 
                         "district" = "district", 
                         "commune" = "commune", 
                         "respondent" = "respondent"))
df.1.new.only$new.excel <- TRUE
df.1.new.only$new.stata <- TRUE
df.1.new.only$old.stata <- FALSE


df.1.old.only <- anti_join( df.old, df.1,
                            by = c("province" = "province", 
                                   "district" = "district", 
                                   "commune" = "commune", 
                                   "respondent" = "respondent"))
df.1.old.only$new.excel <- FALSE
df.1.old.only$new.stata <- FALSE
df.1.old.only$old.stata <- TRUE


write.csv(file = "data/outputs/00.missing.cases.csv", 
         rbind(df.1.new.only, df.1.old.only, df.1.stata.only))

## 2. Import and check Section B     ########################################
## 2.1 Import stata version ###########  ######################################
ds.2 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_B.dta"))

# add new slots to annotations
for (i in 1:ncol(ds.2)) {
  annotation(ds.2[[i]])["flag"] <- "Orig."
  annotation(ds.2[[i]])["origin"] <- "Imported from .dta"
}

## 2.2 Import copy .xlsx file # One OFF  ######################################
ds.x.2 <- read_excel("data/data-raw/DATA/Excel/Section_B.xls")

## 2.3 compare new stata with new excel######### ##############################

# stata file, unfactor
df.2 <- as.data.frame(ds.2[1:7])
levels(df.2$commune) <- unique(ds.x.2$commune)
df.2$commune <- as.numeric(as.character(df.2$commune))

df.2.stata.only <- anti_join( df.2, ds.x.2,
                             by = c("commune" = "commune",
                                    "hhcode" = "hhcode",
                                    "mcode" = "mcode",
                                    "b1" = "b1"))


df.2.stata.only$new.excel <- FALSE
df.2.stata.only$new.stata <- TRUE
df.2.stata.only$old.stata <- TRUE

## 2.4 compare new stata with old stata ######################################

df.2.old <- as.data.frame(ds.old[c(6,8,21:68)])
# manual correction of errors:
# 111-4 is 2
df.2.old[(df.2.old$commcode==111 & df.2.old$hhcode == 4), "hhcode"] <- c(4,2)
df.2.old[(df.2.old$commcode==112 & df.2.old$hhcode == 10),"hhcode"] <- c(10,11)
df.2.old[(df.2.old$commcode==112 & df.2.old$hhcode == 46),"hhcode"] <- c(46,50)
df.2.old[(df.2.old$commcode==121 & df.2.old$hhcode == 19),"hhcode"] <- c(18, 19)
df.2.old[(df.2.old$commcode==121 & df.2.old$hhcode == 21),"hhcode"] <- c(21,41)
df.2.old[(df.2.old$commcode==121 & df.2.old$hhcode == 28),"hhcode"] <- c(28,38)


df.2.old %>% 
  gather( key = "var", value = "value", 3:50) %>% 
  separate(var, c("question", "mcode"), sep = 2) %>% 
  mutate(mcode= as.numeric(mcode)) %>% 
  arrange(commcode, hhcode, mcode) %>% 
  filter(!is.na(value)) %>% 
  filter(value != "") %>% 
  spread(key = question, value = value)  -> df.2.old

colnames(df.2.old)[1] <-  c("commune")

df.2.new.only <- anti_join(df.2, df.2.old,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode" = "mcode"))
df.2.new.only$new.excel <- TRUE
df.2.new.only$new.stata <- TRUE
df.2.new.only$old.stata <- FALSE


df.2.old.only <- anti_join(df.2.old, df.2,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode" = "mcode"))

df.2.old.only$new.excel <- FALSE
df.2.old.only$new.stata <- FALSE
df.2.old.only$old.stata <- TRUE

# save to 
# remove the ones that are actually in both old and new, but only had wrong hhcodes 
# that's all the df.2.new.only ones, and all byt rows 5 and 6 of the df.2.old.only

write.csv(file = "data/outputs/01.missing.cases.2.csv", 
          rbind( df.2.old.only[5:6,], df.2.stata.only))

## 3. Import and check Section C    ########################################
## 3.1 Import stata version ###########  ######################################
ds.3 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_C.dta"))

# add new slots to annotations
for (i in 1:ncol(ds.3 )) {
  annotation(ds.3 [[i]])["flag"] <- "Orig."
  annotation(ds.3 [[i]])["origin"] <- "Imported from .dta"
}
nrow(ds.3)
## 3.2 Import copy .xlsx file # One OFF  ######################################
ds.x.3 <- read_excel("data/data-raw/DATA/Excel/Section_C.xls")

## 3.3 compare new stata with new excel######### ##############################

# stata file, unfactor
df.3 <- as.data.frame(ds.3[1:7])
levels(df.3$commune) <- unique(ds.x.3$commune)
df.3$commune <- as.numeric(as.character(df.3$commune))

df.3.stata.only <- anti_join( df.3, ds.x.3,
                              by = c("commune" = "commune",
                                     "hhcode" = "hhcode",
                                     "mcode_c" = "mcode_c"))

## 3.4 compare new stata with old stata ######################################

df.3.old <- as.data.frame(ds.old[c(6,8,69:100)])
# manual correction of errors:
# 111-4 is 2
df.3.old[(df.3.old$commcode==111 & df.3.old$hhcode == 4), "hhcode"] <- c(2,4)
df.3.old[(df.3.old$commcode==112 & df.3.old$hhcode == 10),"hhcode"] <- c(10,11)
df.3.old[(df.3.old$commcode==112 & df.3.old$hhcode == 46),"hhcode"] <- c(46,50)
df.3.old[(df.3.old$commcode==121 & df.3.old$hhcode == 19),"hhcode"] <- c(18, 19)
df.3.old[(df.3.old$commcode==121 & df.3.old$hhcode == 21),"hhcode"] <- c(21,41)
df.3.old[(df.3.old$commcode==121 & df.3.old$hhcode == 28),"hhcode"] <- c(28,38)

# reshape
df.3.old %>% 
  gather( key = "var", value = "value", 3:34) %>% 
  separate(var, c("question", "mcode_c"), sep = 2) %>% 
  mutate(mcode_c= as.numeric(mcode_c)) %>% 
  arrange(commcode, hhcode, mcode_c) %>% 
  filter(!is.na(value)) %>% 
  filter(value != "") %>% 
  spread(key = question, value = value)  -> df.3.old

colnames(df.3.old)[1] <-  c("commune")

df.3.new.only <- anti_join(df.3, df.3.old,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode_c" = "mcode_c"))

df.3.old.only <- anti_join(df.3.old, df.3,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode_c" = "mcode_c"))




## 4. Import and check Section G3   ########################################

ds.4 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_G3.dta"))



## 5. Import and check Section H_K    ########################################
ds.5 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_H_K.dta"))

