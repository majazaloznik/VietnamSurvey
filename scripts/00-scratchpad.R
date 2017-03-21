rm(list = ls())

library(dplyr)
library(memisc)

load("data/working-copy.RData")

var <- ds$a6

codebook(var)
measurement(var)
table(var)
