rm(list = ls())

library(dplyr)
library(memisc)

load("data/working-copy.RData") # to get data direct from import
load("data/updated-copy.RData") # to get ds as far as new vars has gotten 


var <- ds$a6

codebook(var)
measurement(var)
table(var)

library(readstata13)
x<- read.dta13("full_data_v1_no_dup_stata-12-11.dta")

attributes(x)$time.stamp

var <- ds$a2

FunIntervalTable(var)

