
###############################################################################
## FUNCTIONS
###############################################################################
## 1.1  Swapping positions in the dataset.
## 
##
##
###############################################################################



# 1.1 Swapping positions in the dataset. Used in:
# 01-import-data.R
# 02-new.variables.R
###############################################################################
FunSwap <- function(ds = ds, New, After) {# only use to move back!!
  index <- 1:ncol(ds)
  from <- which(names(ds)[index] == New)
  to <- which(names(ds)[index] == After)
  index <- append(index, values = from, after = to) # insert in new position
  index <- index[-(from+1)] # remove from old
  ds <<- ds[index]
}


FunRemove <- function(col.names){
  removing <- which(names(ds) %in% col.names)
  ds <<- ds[-removing]
}