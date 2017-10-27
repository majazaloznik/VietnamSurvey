###############################################################################
##  DATA IMPORT   #############################################################
###############################################################################
## 0. Preliminaries  ##########################################################
## 1. Import and check Section A-G     ########################################
## 1.1 Import stata version ###################################################
## 1.2 Import copy .xlsx file # One OFF  ######################################
## 1.3 Import old .dta file to compare# One OFF  ##############################
## 1.4 compare new stata with new excel########################################
## 1.5 compare new stata with old stata #######################################
## 2. Import and check Section B     ##########################################
## 2.1 Import stata version ###################################################
## 2.2 Import copy .xlsx file # One OFF  ######################################
## 2.3 compare new stata with new excel########################################
## 2.4 compare new stata with old stata #######################################
## 3. Import and check Section C    ###########################################
## 3.1 Import stata version ###################################################
## 3.2 Import copy .xlsx file # One OFF  ######################################
## 3.3 compare new stata with new excel######### ##############################
## 3.4 compare new stata with old stata #######################################
## 3.5 check gender consistency for sons and daughters  #######################
## 3.6. remove this case from stata file  #####################################
## 4. Import and check Section G3   ###########################################
## 4.1 Import stata version ###################################################
## 4.2 Import copy .xlsx file # One OFF  ######################################
## 4.3 compare new stata with new excel######### ##############################
## 4.4 compare new stata with old stata #######################################
## 5. Import and check Section H_K    #########################################
## 5.1 Import stata version ###################################################
## 5.2 Import copy .xlsx file # One OFF  ######################################
## 5.3 compare new stata with new excel########################################
## 5.4 compare new stata with old stata #######################################
## 6. save ####################################################################
###############################################################################

## 0. Preliminaries  ##########################################################
###############################################################################
library(memisc)
library(tidyverse)
library(readxl)

## 1. Import and check Section A-G     ########################################
## 1.1 Import stata version ###################################################
ds.1 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_A-G.dta"))

# add new slots to annotations
for (i in 1:ncol(ds.1)) {
  annotation(ds.1[[i]])["flag"] <- "Orig."
  annotation(ds.1[[i]])["origin"] <- "Imported from .dta"
}

## 1.2 Import copy .xlsx file # One OFF  ######################################
ds.x.1 <- read_excel("data/data-raw/DATA/Excel/Section_A-G.xls")

## 1.3 Import old .dta file to compare# One OFF  ##############################

ds.old <- as.data.set(Stata.file("old/data-old/full_data_v1_no_dup_stata-12-11.dta"))

## 1.4 compare new stata with new excel########################################

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

## 1.5 compare new stata with old stata ######################################

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

rm(df.1, df.1.new.only, df.1.old.only, df.1.stata.only, df.old, ds.x.1)

## 2. Import and check Section B     ##########################################
## 2.1 Import stata version ###################################################
ds.2 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_B.dta"))

# add new slots to annotations
for (i in 1:ncol(ds.2)) {
  annotation(ds.2[[i]])["flag"] <- "Orig."
  annotation(ds.2[[i]])["origin"] <- "Imported from .dta"
}

## 2.2 Import copy .xlsx file # One OFF  ######################################
ds.x.2 <- read_excel("data/data-raw/DATA/Excel/Section_B.xls")

## 2.3 compare new stata with new excel########################################

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
# 111-4 is 2 etc. 
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

rm(df.2, df.2.new.only, df.2.old.only, df.2.stata.only, df.2.old, ds.x.2)

## 3. Import and check Section C    ###########################################
## 3.1 Import stata version ###################################################
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
df.3$c1 <- as.character(df.3$c1)
df.3$c2 <- as.numeric(df.3$c2)
df.3$c3 <- as.numeric(df.3$c3)
df.3$c4 <- as.numeric(df.3$c4)
ds.x.3$c3 <- ds.x.3$c3-1

#  So there is one case that is different, because it is missing c4 in the stata 
# file, but also i'll change back to the labels, so you can see it's a male daubguther.. 
df.3.stata.only <- anti_join( df.3, ds.x.3)
df.3.excel.only <- anti_join( ds.x.3, df.3 )
df.3 <- as.data.frame(ds.3[1:7])

df.3.stata.only$c2 <- levels(df.3$c2)[df.3.stata.only$c2 ]
df.3.stata.only$c3 <- levels(df.3$c3)[df.3.stata.only$c3 ]
df.3.excel.only$c2 <- levels(df.3$c2)[df.3.excel.only$c2 ]
df.3.excel.only$c3 <- levels(df.3$c3)[df.3.excel.only$c3 ]

df.3.stata.only$new.excel <- FALSE
df.3.stata.only$new.stata <- TRUE
df.3.excel.only$new.excel <- TRUE
df.3.excel.only$new.stata <- FALSE


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
df.3 <- as.data.frame(ds.3[1:7])
levels(df.3$commune) <- unique(ds.x.3$commune)
df.3$commune <- as.numeric(as.character(df.3$commune))
df.3$c1 <- as.character(df.3$c1)
df.3$c2 <- as.numeric(df.3$c2)
df.3$c3 <- as.numeric(df.3$c3)
df.3$c4 <- as.numeric(df.3$c4)
ds.x.3$c3 <- ds.x.3$c3-1
df.3.new.only <- anti_join(df.3, df.3.old,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode_c" = "mcode_c"))

df.3.old.only <- anti_join(df.3.old, df.3,
                           by = c("commune" = "commune",
                                  "hhcode" = "hhcode",
                                  "mcode_c" = "mcode_c"))

write.csv(file = "data/outputs/02.missing.cases.3.csv", 
          rbind(df.3.stata.only, df.3.excel.only))

## 3.5 check gender consistency for sons and daughters  #######################
table(ds.3$c2, ds.3$c3) # ok
## 3.6. remove this case from stata file  #####################################
ds.3 <- ds.3[-(ds.3$c1 == "pham thi hong uyen"),]

rm(df.3, df.3.new.only, df.3.old.only, df.3.stata.only, df.3.excel.only, df.3.old, ds.x.3)

## 4. Import and check Section G3   ###########################################
## 4.1 Import stata version ###################################################
ds.4 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_G3.dta"))
nrow(ds.4)
# add new slots to annotations
for (i in 1:ncol(ds.4)) {
  annotation(ds.4[[i]])["flag"] <- "Orig."
  annotation(ds.4[[i]])["origin"] <- "Imported from .dta"
}

## 4.2 Import copy .xlsx file # One OFF  ######################################
ds.x.4 <- read_excel("data/data-raw/DATA/Excel/Section_G3.xls")

## 4.3 compare new stata with new excel######### ##############################

# stata file, unfactor
df.4 <- as.data.frame(ds.4)
levels(df.4$commune) <- unique(ds.x.4$commune)
df.4$commune <- as.numeric(as.character(df.4$commune))
df.4$g32 <- as.numeric(df.4$g32)
df.4$g33 <- as.numeric(df.4$g33)
df.4$g34 <- as.numeric(df.4$g34)
df.4$g35 <- as.numeric(df.4$g35)
df.4$g36 <- as.numeric(df.4$g36)

df.4.stata.only <- anti_join( df.4, ds.x.4)
df.4.excel.only <- anti_join( ds.x.4, df.4)

# files are identical!


## 4.4 compare new stata with old stata ######################################

df.4.old <- as.data.frame(ds.old[c(6,8,130:219)])
# manual correction of errors:
# 111-4 is 2 etc. 
df.4.old[(df.4.old$commcode==111 & df.4.old$hhcode == 4), "hhcode"] <- c(2,4)
df.4.old[(df.4.old$commcode==112 & df.4.old$hhcode == 10),"hhcode"] <- c(10,11)
df.4.old[(df.4.old$commcode==112 & df.4.old$hhcode == 46),"hhcode"] <- c(46,50)
df.4.old[(df.4.old$commcode==121 & df.4.old$hhcode == 19),"hhcode"] <- c(18, 19)
df.4.old[(df.4.old$commcode==121 & df.4.old$hhcode == 21),"hhcode"] <- c(21,41)
df.4.old[(df.4.old$commcode==121 & df.4.old$hhcode == 28),"hhcode"] <- c(28,38)

# reshape
df.4.old %>% 
  gather( key = "var", value = "value", 3:92) %>% 
  separate(var, c("question", "plot"), sep = 3) %>% 
  mutate(plot= as.numeric(plot)) %>% 
  arrange(commcode, hhcode, plot) %>% 
  filter(!is.na(value)) %>% 
  filter(value != "") %>% 
  spread(key = question, value = value)  -> df.4.old

colnames(df.4.old)[1] <-  c("commune")

df.4.new.only <- anti_join(df.4, df.4.old)


df.4.old.only <- anti_join(df.4.old, df.4)

write.csv(file = "data/outputs/02.missing.cases.3.csv", 
          rbind(df.3.stata.only, df.3.excel.only))

rm(df.4, df.4.new.only, df.4.old.only, df.4.stata.only, df.4.excel.only, df.4.old, ds.x.4)

## 5. Import and check Section H_K    #########################################

## 5.1 Import stata version ###################################################
ds.5 <- as.data.set(Stata.file("data/data-raw/DATA/Stata/Section_H_K.dta"))
nrow(ds.5)
# add new slots to annotations
for (i in 1:ncol(ds.5)) {
  annotation(ds.5[[i]])["flag"] <- "Orig."
  annotation(ds.5[[i]])["origin"] <- "Imported from .dta"
}
## 5.2 Import copy .xlsx file # One OFF  ######################################
ds.x.5 <- read_excel("data/data-raw/DATA/Excel/Section_H_K.xls")

## 5.3 compare new stata with new excel########################################

# stata file, unfactor
df.5 <- as.data.frame(ds.5[1:6])
levels(df.5$province) <- unique(ds.x.5$province)
levels(df.5$district) <- unique(ds.x.5$district)
levels(df.5$commune) <- unique(ds.x.5$commune)
df.5$province <- as.numeric(as.character(df.5$province))
df.5$district <- as.numeric(as.character(df.5$district))
df.5$commune <- as.numeric(as.character(df.5$commune))
df.5.stata.only <- anti_join(df.5, ds.x.5[1:6])

df.5.stata.only$new.excel <- FALSE
df.5.stata.only$new.stata <- TRUE
df.5.stata.only$old.stata <- TRUE

## 5.4 compare new stata with old stata ######################################

df.old <- as.data.frame(ds.old[c(2,4,6,7,8,9)])
colnames(df.old) <-  c("province", "district", "commune", "hhhead","hhcode","respondent")

# don't match on hhcode, because that's where there are errors. 
df.5.new.only <- anti_join(df.5, df.old,
                           by = c("province" = "province", 
                                  "district" = "district", 
                                  "commune" = "commune", 
                                  "respondent" = "respondent"))
df.5.new.only$new.excel <- TRUE
df.5.new.only$new.stata <- TRUE
df.5.new.only$old.stata <- FALSE


df.5.old.only <- anti_join( df.old, df.5,
                            by = c("province" = "province", 
                                   "district" = "district", 
                                   "commune" = "commune", 
                                   "respondent" = "respondent"))
df.5.old.only$new.excel <- FALSE
df.5.old.only$new.stata <- FALSE
df.5.old.only$old.stata <- TRUE


write.csv(file = "data/outputs/03.missing.cases.4.csv", 
          rbind(df.5.new.only, df.5.old.only, df.5.stata.only))

rm(df.5, df.5.new.only, df.5.old.only, df.5.stata.only, df.5.excel.only, df.5.old, ds.x.5)
rm(df.old, ds.old, i)




## 6. save ####################################################################
save.image("data/outputs/tmp.clean.import.RData")
rm(list = ls())
